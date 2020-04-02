#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils gnugrep

# A handy utility for filling in the github RELEASE_TEMPLATE.
# Since we are using nix, this script shoud work whether on linux or mac.

REPO="input-output-hk/cardano-wallet"
$DOCKER_WIKI_FILE="Docker.md"
$JORM_CLI_FILE="Wallet-command-line-interface.md"
$BYRON_CLI_FILE="Wallet-Command-Line-Interface-(cardano-wallet-byron).md"
TMP_CWD=$(pwd)
TMP_WIKI_DIR=/tmp/cardano-wallet-script
mkdir $TMP_WIKI_DIR
echo $TMP_WIKI_DIR
cd $TMP_WIKI_DIR
git clone https://github.com/$REPO.wiki.git
cd cardano-wallet.wiki

DOCKER_WIKI_COMMIT=$(git log -n 1 --pretty=format:%H -- $DOCKER_WIKI_FILE)
JORM_CLI_WIKI_COMMIT=$(git log -n 1 --pretty=format:%H -- $JORM_CLI_FILE)
BYRON_CLI_WIKI_COMMIT=$(git log -n 1 --pretty=format:%H -- $BYRON_CLI_FILE)

# NOTE: Maybe we don't need to retrieve a commit for EACH file? We would be fine with the latest commit?
# Also NOTE: This script will target the LATEST wiki version. Make sure the wiki hasn't changed undesirably since
# the release tag.
echo "Wiki commits for permament:"
echo "$DOCKER_WIKI_COMMIT (Docker)"
echo "$JORM_CLI_WIKI_COMMIT (Jormungandr CLI)"
echo "$BYRON_CLI_WIKI_COMMIT (Byron CLI)"

rm -rf $TMP_WIKI_DIR
cd $TMP_CWD

# Parameters (Change when you bump the version)
GIT_TAG="v2020-04-01"
CABAL_VERSION="2020.4.1"
JORM_TAG="v0.8.15"
CARDANO_NODE_TAG="v1.9.3"

sed "
s/{{GIT_TAG}}/$GIT_TAG/g
s/{{JORM_TAG}}/$JORM_TAG/g
s/{{CABAL_VERSION}}/$CABAL_VERSION/g
s/{{DOCKER_WIKI_COMMIT}}/$DOCKER_WIKI_COMMIT/g
s/{{JORM_CLI_WIKI_COMMIT}}/$JORM_CLI_WIKI_COMMIT/g
s/{{BYRON_CLI_WIKI_COMMIT}}/$BYRON_CLI_WIKI_COMMIT/g
" ../.github/RELEASE_TEMPLATE.md > GENERATED_RELEASE_NOTES-$GIT_TAG.md


