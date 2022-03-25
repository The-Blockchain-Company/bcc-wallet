<p align="center">
  <big><strong>Bcc Wallet</strong></big>
</p>

<p align="center">
  <img width="200" src=".github/images/bcc-logo.png"/>
</p>

<p align="center">
  <a href="https://github.com/The-Blockchain-Company/bcc-wallet/releases"><img src="https://img.shields.io/github/release-pre/The-Blockchain-Company/bcc-wallet.svg?style=for-the-badge" /></a>
  <a href="https://buildkite.com/The-Blockchain-Company/bcc-wallet"><img src="https://img.shields.io/buildkite/7ea3dac7a16f066d8dfc8f426a9a9f7a2131e899cd96c444cf/master?label=BUILD&style=for-the-badge"/></a>
  <a href="https://buildkite.com/The-Blockchain-Company/bcc-wallet-nightly"><img src="https://img.shields.io/buildkite/59ea9363b8526e867005ca8839db47715bc5f661f36e490143/master?label=BENCHMARK&style=for-the-badge" /></a>
  <a href="https://github.com/The-Blockchain-Company/bcc-wallet/actions?query=workflow%3Awindows"><img src="https://img.shields.io/github/workflow/status/The-Blockchain-Company/bcc-wallet/windows?label=Windows&style=for-the-badge" /></a>
  <!--
  <a href="https://coveralls.io/github/The-Blockchain-Company/bcc-wallet?branch=HEAD"><img src="https://img.shields.io/coveralls/github/The-Blockchain-Company/bcc-wallet/HEAD?style=for-the-badge" /></a>
  -->
</p>

<hr/>

## Overview

Bcc Wallet helps you manage your Bcc. You can use it to send and
receive payments on the [Bcc](https://www.bcc.org) blockchain.

This project provides an HTTP Application Programming Interface (API)
and command-line interface (CLI) for working with your wallet.

It can be used as a component of a frontend such as
[Klarity](https://klaritywallet.io), which provides a friendly user
interface for wallets. Most users who would like to use Bcc should
start with Klarity.

> :information_source: This source code repository contains the next major version of Bcc
> Wallet, which has been completely rewritten for the
> [Sophie](https://roadmap.bcc.org/) phase.
>
> :bulb: The Cole version of Bcc Wallet is in the
> [bcc-sl](https://github.com/The-Blockchain-Company/bcc-sl)
> repository.

## Getting Started

```
wget https://raw.githubusercontent.com/The-Blockchain-Company/bcc-wallet/master/docker-compose.yml
NETWORK=testnet docker-compose up
```

Fantastic! The server is up-and-running, waiting for HTTP requests on `localhost:8090/v2` e.g.:

```
curl http://localhost:8090/v2/network/information
```

or to be accessed via CLI, e.g.:

```
docker run --network host --rm inputoutput/bcc-wallet network information
```

See also [Wiki - Docker](https://github.com/The-Blockchain-Company/bcc-wallet/wiki/Docker) for more information about using docker.

## How to install (Linux / Windows / Mac OS)

See **Installation Instructions** for each available [release](https://github.com/The-Blockchain-Company/bcc-wallet/releases).

> ### Latest releases
>
> | bcc-wallet | bcc-node (compatible versions) | SMASH (compatible versions)
> | --- | --- | ---
> | `master` branch | [1.30.1](https://github.com/The-Blockchain-Company/bcc-node/releases/tag/1.30.1) | [1.6.0](https://github.com/The-Blockchain-Company/smash/releases/tag/1.6.0)
> | [v2021-09-29](https://github.com/The-Blockchain-Company/bcc-wallet/releases/tag/v2021-09-29) | [1.30.1](https://github.com/The-Blockchain-Company/bcc-node/releases/tag/1.30.1) | [1.6.0](https://github.com/The-Blockchain-Company/smash/releases/tag/1.6.0)
> | [v2021-09-09](https://github.com/The-Blockchain-Company/bcc-wallet/releases/tag/v2021-09-09) | [1.29.0](https://github.com/The-Blockchain-Company/bcc-node/releases/tag/1.29.0) | [1.6.0](https://github.com/The-Blockchain-Company/smash/releases/tag/1.6.0)
> | [v2021-08-27](https://github.com/The-Blockchain-Company/bcc-wallet/releases/tag/v2021-08-27) | [1.29.0](https://github.com/The-Blockchain-Company/bcc-node/releases/tag/1.29.0) | [1.6.0](https://github.com/The-Blockchain-Company/smash/releases/tag/1.6.0)

## How to build from sources

See [Wiki - Building](https://github.com/The-Blockchain-Company/bcc-wallet/wiki/Building)

## How to test

See [Wiki - Testing](https://github.com/The-Blockchain-Company/bcc-wallet/wiki/Testing)

## Documentation

| Link                                                                                               | Audience                                                     |
| ---                                                                                                | ---                                                          |
| [API Documentation](https://The-Blockchain-Company.github.io/bcc-wallet/api/edge)                     | Users of the Bcc Wallet API                              |
| [Wiki](https://github.com/The-Blockchain-Company/bcc-wallet/wiki)                                     | Anyone interested in the project and our development process |
| [CLI Manual](https://github.com/The-Blockchain-Company/bcc-wallet/wiki/Wallet-command-line-interface) | Users of the Bcc Wallet API                              |

<hr/>

<p align="center">
  <a href="https://github.com/The-Blockchain-Company/bcc-wallet/blob/master/LICENSE"><img src="https://img.shields.io/github/license/The-Blockchain-Company/bcc-wallet.svg?style=for-the-badge" /></a>
</p>
