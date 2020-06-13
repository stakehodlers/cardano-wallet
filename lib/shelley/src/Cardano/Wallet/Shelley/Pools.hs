{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Haskell-node "shelley" implementation of the @StakePoolLayer@ abstraction,
-- i.e. some boring glue.
module Cardano.Wallet.Shelley.Pools where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Pool.DB
    ( DBLayer (..) )
import Cardano.Wallet.Api.Server
    ( LiftHandler (..), apiError )
import Cardano.Wallet.Api.Types
    ( ApiErrorCode (..), ApiT (..) )
import Cardano.Wallet.Network
    ( FollowLog (..), NetworkLayer (..), NextBlocksResult (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , Coin (..)
    , GenesisParameters (..)
    , PoolId
    , PoolOwner
    , PoolRegistrationCertificate (..)
    , SlotId (..)
    , SlotId
    , StakePoolMetadata
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley
    , ShelleyBlock
    , TPraosStandardCrypto
    , fromNonMyopicMemberRewards
    , fromPoolDistr
    , fromPoolDistr
    , fromPoolId
    , fromShelleyBlock
    , optimumNumberOfPools
    , toPoint
    , toShelleyCoin
    )
import Cardano.Wallet.Shelley.Network
    ( pattern Cursor )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage, unsafeRunExceptT )
import Control.Concurrent
    ( forkIO )
import Control.Monad
    ( forM_ )
import Control.Monad.Class.MonadSTM
    ( MonadSTM, TQueue )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, withExceptT )
import Control.Tracer
    ( Tracer, traceWith )
import Data.Foldable
    ( toList )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Map
    ( Map )
import Data.Map.Merge.Strict
    ( dropMissing, traverseMissing, zipWithMatched )
import Data.Maybe
    ( catMaybes, fromMaybe, mapMaybe )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Sort
    ( sortOn )
import Data.Text.Class
    ( ToText (..) )
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import Ouroboros.Network.Block
    ( Point )
import Ouroboros.Network.Client.Wallet
    ( ChainSyncCmd (..), LocalStateQueryCmd (..), send )
import Servant
    ( err500 )

import qualified Cardano.Wallet.Api.Types as Api
import qualified Data.Map.Merge.Strict as Map
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Shelley.Ledger as OC
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Tx as SL
import qualified Shelley.Spec.Ledger.TxData as SL

monitorStakePools
    :: Tracer IO StakePoolLog
    -> GenesisParameters
    -> [PoolRegistrationCertificate]
       -- ^ Genesis certs
    -> NetworkLayer IO (IO Shelley) ShelleyBlock
    -> DBLayer IO
    -> IO (StakePoolLayer)
monitorStakePools tr gp _genesisPoolRegCerts nl db@DBLayer{..} = do
    liftIO $ putStrLn "### monitorStakePools"
    Cursor _workerTip csQ _ <- liftIO $ initCursor nl []
    _ <- forkIO $ go csQ
    return $ newStakePoolLayer gp nl db
  where
    go csQ = do
        res <- csQ `send` CmdNextBlocks
        case res of
            RollForward _ _ blocks -> forward blocks >> go csQ
            RollBackward to -> rollback to >> go csQ
            AwaitReply -> go csQ -- impossible?

    getHeader = header . fromShelleyBlock (getGenesisBlockHash gp) (getEpochLength gp)

    forward
        :: [ShelleyBlock]
        -> IO ()
    forward blocks = do
        atomically $ do
            forM_ blocks $ \b -> do
                forM_ (txs b >>= regCerts) $ \cert -> do
                    case cert of
                        SL.RegPool params -> do
                            let pool = PoolRegistrationCertificate
                                    { poolId = fromPoolId $ SL._poolPubKey params
                                    , poolOwners = []
                                    , poolMargin = unsafeMkPercentage
                                            $ toRational
                                            $ SL.intervalValue
                                            $ SL._poolMargin params
                                    , poolCost = Quantity
                                        $ fromIntegral
                                        $ SL._poolCost params
                                    }

                            liftIO $ putStrLn "Found a cert!"
                            putPoolRegistration (view #slotId $ getHeader b) pool
                            liftIO $ traceWith tr $ MsgStakePoolRegistration pool
                        SL.RetirePool _ _ -> return () -- TODO

    rollback :: Point ShelleyBlock -> IO ()
    rollback _to = return () -- TODO: atomically $ rollbackTo (fromSlotNo el $ f to)

    txs :: OC.ShelleyBlock TPraosStandardCrypto -> [SL.Tx TPraosStandardCrypto]
    txs (OC.ShelleyBlock (SL.Block _ (SL.TxSeq ts)) _) = toList ts

    regCerts :: SL.Tx TPraosStandardCrypto -> [SL.PoolCert TPraosStandardCrypto]
    regCerts (SL.Tx (SL.TxBody _ _ certs _ _ _ _ _) _ _) =
        mapMaybe reg $ toList certs
    reg = \case
        SL.DCertDeleg _ -> Nothing
        SL.DCertPool c            -> Just c
        SL.DCertGenesis{}         -> Nothing
        SL.DCertMir{}             -> Nothing

-- | Stake Pool Data fields fetched from the node via LSQ
data PoolLsqMetrics = PoolLsqMetrics
    { nonMyopicMemberRewards :: Quantity "lovelace" Word64
    , relativeStake :: Percentage
    , saturation :: Double
    } deriving (Eq, Show, Generic)


data ErrFetchMetrics = ErrFetchMetrics
  deriving Show

-- | Fetches information about pools availible over LSQ from the node, at the
-- nodes' tip.
fetchLsqPoolMetrics
    :: MonadSTM m
    => TQueue m (LocalStateQueryCmd ShelleyBlock m)
    -> Point ShelleyBlock
    -> Coin
    -> ExceptT ErrFetchMetrics m (Map PoolId PoolLsqMetrics)
fetchLsqPoolMetrics queue pt coin = do
    stakeMap <- fromPoolDistr <$> handleQueryFailure
        (queue `send` CmdQueryLocalState pt OC.GetStakeDistribution)
    let toStake = Set.singleton $ Left $ toShelleyCoin coin
    rewardsPerAccount <- fromNonMyopicMemberRewards <$> handleQueryFailure
        (queue `send` CmdQueryLocalState pt (OC.GetNonMyopicMemberRewards toStake))
    pparams <- handleQueryFailure
        (queue `send` CmdQueryLocalState pt OC.GetCurrentPParams)

    let rewardMap = fromMaybe
            (error "askNode: requested rewards not included in response")
            (Map.lookup (Left coin) rewardsPerAccount)

    return $ combine
        (optimumNumberOfPools pparams)
        stakeMap
        rewardMap
  where
    handleQueryFailure = withExceptT (const ErrFetchMetrics) . ExceptT

    combine
        :: Int -- ^ Desired number of pools
        -> Map PoolId Percentage
        -> Map PoolId (Quantity "lovelace" Word64)
        -> Map PoolId PoolLsqMetrics
    combine nOpt =
        Map.merge stakeButNoRewards rewardsButNoStake bothPresent
      where
        -- calculate the saturation from the relative stake
        sat s = fromRational $ (getPercentage s) / (1 / fromIntegral nOpt)

        -- If we fetch non-myopic member rewards of pools using the wallet
        -- balance of 0, the resulting map will be empty. So we set the rewards
        -- to 0 here:
        stakeButNoRewards = traverseMissing $ \_k s -> pure $ PoolLsqMetrics
            { nonMyopicMemberRewards = Quantity 0
            , relativeStake = s
            , saturation = (sat s)
            }

        rewardsButNoStake = dropMissing

        bothPresent       = zipWithMatched  $ \_k s r -> PoolLsqMetrics r s (sat s)

readBlockProductions :: IO (Map PoolId Int)
readBlockProductions = return Map.empty

--
-- Api Server Handler
--

instance LiftHandler ErrFetchMetrics where
    handler = \case
        ErrFetchMetrics ->
            apiError err500 NotSynced $ mconcat
                [ "There was a problem fetching metrics from the node."
                ]

data StakePoolLayer = StakePoolLayer
    { knownPools :: IO [PoolId]
    , listStakePools :: Coin -> ExceptT ErrFetchMetrics IO [Api.ApiStakePool]
    }


newStakePoolLayer
    :: GenesisParameters
    -> NetworkLayer IO (IO Shelley) b
    -> DBLayer IO
    -> StakePoolLayer
newStakePoolLayer gp nl DBLayer{..} = StakePoolLayer
    { knownPools = _knownPools
    , listStakePools = _listPools
    }
  where
    dummyCoin = Coin 0

    -- Note: We shouldn't have to do this conversion.
    el = getEpochLength gp
    gh = getGenesisBlockHash gp
    getTip = fmap (toPoint gh el) . liftIO $ unsafeRunExceptT $ currentNodeTip nl

    _knownPools
        :: IO [PoolId]
    _knownPools = do
        Cursor _workerTip _ lsqQ <- initCursor nl []
        pt <- getTip
        res <- runExceptT $ map fst . Map.toList
            <$> fetchLsqPoolMetrics lsqQ pt dummyCoin
        case res of
            Right x -> return x
            Left _e -> return []


    readDBCerts :: IO (Map PoolId PoolRegistrationCertificate)
    readDBCerts = atomically $ do
        pools <- listRegisteredPools
        x <- mapM (\p -> ((fmap (p,)) <$> readPoolRegistration p) ) pools
        return $ Map.fromList $ catMaybes x

    _listPools
        :: Coin
        -- ^ The amount of stake the user intends to delegate, which may affect the
        -- ranking of the pools.
        -> ExceptT ErrFetchMetrics IO [Api.ApiStakePool]
    _listPools s = do
            Cursor _workerTip _ lsqQ <- liftIO $ initCursor nl []
            pt <- liftIO getTip
            lsqData <- fetchLsqPoolMetrics lsqQ pt s
            dbData <- liftIO readDBCerts
            return $
                sortOn (Down . (view (#metrics . #nonMyopicMemberRewards)))
                . map snd
                . Map.toList
                $ combineLsqAndDb lsqData dbData
      where
        combineLsqAndDb
            :: Map PoolId PoolLsqMetrics
            -> Map PoolId PoolRegistrationCertificate
            -> Map PoolId Api.ApiStakePool
        combineLsqAndDb =
            Map.merge dropMissing dropMissing bothPresent
          where
            bothPresent = zipWithMatched  $ \k m r -> mkApiPool k m r

            mkApiPool pid (PoolLsqMetrics prew pstk psat) reg = Api.ApiStakePool
                { Api.id = (ApiT pid)
                , Api.metrics = Api.ApiStakePoolMetrics
                    { Api.nonMyopicMemberRewards = (mapQ fromIntegral prew)
                    , Api.relativeStake = Quantity pstk
                    , Api.saturation = psat
                    , Api.producedBlocks = Quantity 0 -- TODO: Implement
                    }
                , Api.metadata = Nothing -- TODO: Implement
                , Api.cost = mapQ fromIntegral $ poolCost reg
                , Api.margin = Quantity $ poolMargin reg
                }

            mapQ f (Quantity x) = Quantity $ f x

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | Messages associated with stake pool layer.
data StakePoolLog
    = MsgListStakePoolsBegin
    | MsgMetadataUnavailable
    | MsgMetadataUsing PoolId PoolOwner StakePoolMetadata
    | MsgMetadataMissing PoolId
    | MsgMetadataMultiple PoolId [(PoolOwner, StakePoolMetadata)]
    | MsgComputedProgress BlockHeader BlockHeader
    | MsgStartMonitoring [BlockHeader]
    | MsgFollow FollowLog
    | MsgStakePoolRegistration PoolRegistrationCertificate
    | MsgRollingBackTo SlotId
    deriving (Show, Eq)

instance HasPrivacyAnnotation StakePoolLog
instance HasSeverityAnnotation StakePoolLog where
    getSeverityAnnotation ev = case ev of
        MsgListStakePoolsBegin -> Debug
        MsgMetadataUnavailable -> Notice
        MsgComputedProgress{} -> Debug
        MsgMetadataUsing{} -> Debug
        MsgMetadataMissing{} -> Debug
        MsgMetadataMultiple{} -> Debug
        MsgStartMonitoring _ -> Info
        MsgFollow msg -> getSeverityAnnotation msg
        MsgStakePoolRegistration _ -> Info
        MsgRollingBackTo _ -> Info

instance ToText StakePoolLog where
    toText = \case
        MsgListStakePoolsBegin -> "Listing stake pools"
        MsgMetadataUnavailable -> "Stake pool metadata is unavailable"
        MsgComputedProgress prodTip nodeTip -> mconcat
            [ "The node tip is:\n"
            , pretty nodeTip
            , ",\nbut the last pool production stored in the db"
            , " is from:\n"
            , pretty prodTip
            ]
        MsgMetadataUsing pid owner _ ->
            "Using stake pool metadata from " <>
            toText owner <> " for " <> toText pid
        MsgMetadataMissing pid ->
            "No stake pool metadata for " <> toText pid
        MsgMetadataMultiple pid _ ->
            "Multiple different metadata registered for " <> toText pid
        MsgStartMonitoring cursor -> mconcat
            [ "Monitoring stake pools. Currently at "
            , case cursor of
                [] -> "genesis"
                _  -> pretty (last cursor)
            ]
        MsgFollow msg ->
            toText msg
        MsgStakePoolRegistration pool ->
            "Discovered stake pool registration: " <> pretty pool
        MsgRollingBackTo point ->
            "Rolling back to " <> pretty point
