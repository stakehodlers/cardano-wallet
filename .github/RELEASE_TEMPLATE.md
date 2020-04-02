Compatible with [`jormungandr@{{JORM_TAG}}`](https://github.com/input-output-hk/jormungandr/releases/tag/{{JORM_TAG}}) and [`cardano-node@{{JORM_TAG}}`](https://github.com/input-output-hk/cardano-node/releases/tag/{{JORM_TAG}}).

# Changelog

<!-- A CHANGELOG, organized in milestones. Ideally, we put it within
some <details></details> elements to avoid cluttering the release notes -->

## Bug Fixes

<!-- Fixes included in this release that were present in the previous release -->
<!-- TODO: can this be merged with the changelog above? -->

## Known Issues

<!-- Bugs known at the moment of the release, or discovered after and not fixed -->

# Weekly Reports

- [Week 12 - 2020-03-20](https://github.com/input-output-hk/cardano-wallet/tree/weekly-reports/2020-03-20)

# Documentation

<!-- A snapshot of the documentation at the time of releasing. -->

| Link                                                                                                                                        | Audience                                                   |
| ---                                                                                                                                         | ---                                                        |
| [API Documentation](https://input-output-hk.github.io/cardano-wallet/api/{{GIT_TAG}})                                                       | Users of the Cardano Wallet API                            |
| CLI Manual: [jormungandr](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-Command-Line-Interface) / [byron](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-Command-Line-Interface-(cardano-wallet-byron)) | Users of the Cardano Wallet API                            |
| [Docker Manual](https://github.com/input-output-hk/cardano-wallet/wiki/Docker/11246e7633eba794fb90fab385239753ba32d70e)                     | Users of the Cardano Wallet API                            |
| [Haddock Documentation](https://input-output-hk.github.io/cardano-wallet/haddock/{{GIT_TAG}})                                               | Haskell Developers using the `cardano-wallet` as a library |

# Installation Instructions 

<!-- Specific installation steps for this particular release. This should
basically captures whatever is currently available on the repository at
the moment of releasing. -->

## Linux 64-bit

1. Install [jormungandr@{{JORM_TAG}}](https://github.com/input-output-hk/jormungandr/releases/tag/{{JORM_TAG}}) from the official repository.

2. Download `cardano-wallet-jormungandr-linux64-{{GIT_TAG}}.tar.gz` and uncompress it in a directory that is on your `$PATH`, e.g. `/usr/local/bin`.

  ```
  $ curl -L https://github.com/input-output-hk/cardano-wallet/releases/download/{{GIT_TAG}}/cardano-wallet-jormungandr-linux64-{{GIT_TAG}}.tar.gz | tar xz -C $HOME/.local/bin
  ```

3. (optional) Install the bash/zsh auto-completion script:

  ```
  $ mv $HOME/.local/bin/cardano-wallet.sh > /etc/bash_completion.d/cardano-wallet.sh
  $ source /etc/bash_completion.d/cardano-wallet.sh
  ```

4. Start `cardano-wallet --help` and see available parameters.

## Mac OS 64-bit 

1. Install [jormungandr@{{JORM_TAG}}](https://github.com/input-output-hk/jormungandr/releases/tag/{{JORM_TAG}}) from the official repository.

2. Download `cardano-wallet-jormungandr-macos64-{{GIT_TAG}}.tar.gz` and uncompress it in a directory that is on your `$PATH`, e.g. `/usr/local/bin`.

*Note:* Make sure all `*.dylib` files are in the same directory as `cardano-wallet` binary.

## Windows 64-bit

1. Install [jormungandr@{{JORM_TAG}}](https://github.com/input-output-hk/jormungandr/releases/tag/{{JORM_TAG}}) from the official repository.

2. Download `cardano-wallet-jormungandr-win64-{{GIT_TAG}}.zip` and uncompress it in a directory that is on your `%PATH%`.

## Docker

1. Pull from DockerHub.

```
$ docker pull inputoutput/cardano-wallet:{{CABAL_VERSION}}-jormungandr
```

2. Verify the image using the command-line.

```
$ docker run --rm inputoutput/cardano-wallet:{{CABAL_VERSION}}-jormungandr version
```

# Signatures

<!-- Signatures of people responsible for the release -->

Name                           | Role                | Approval
---                            | ---                 | ---:
Matthias Benkort @KtorZ        | Technical Team Lead | :hourglass: 
Piotr Stachyra @piotr-iohk     | QA Engineer         | :hourglass: 
Tatyana Valkevych @tatyanavych | Release Manager     | :hourglass: 

[new]: https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/.github/images/badge-new.png
