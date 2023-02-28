# ENCS Distribution app
This is an implementation of the decentralized token distribution algorithm described [here](https://encoins-crypto.medium.com/fully-decentralized-token-distribution-on-cardano-9d7317d8de6).

# Prerequisites

* System requirements: Linux OS, a processor capable of running `cardano-node`, 16 GB RAM, 300 GB of disk storage.
* `cardano-node` must be running on port 3003. Synchronize it before proceeding.
* `kupo` must be running. Synchonize it before proceeding.
* `cardano-wallet` must be running on port 8090 (the default) with the backend wallet loaded.
*  The folder from which you run `encs` should contain the following files: "blockfrost.token", "config.json", "encs-params.json", "distribution.json", and "protocol-parameters.json". You can get a free blockfrost token [here](https://blockfrost.io/). The token must be written in quotes. In "encs-params.json", change the contents of "getTxId" and "txOutRefIdx" fields to an unspent output from your backend wallet. The file "distribution.json" should contain the list of addresses and token amounts to distribute.

# How to use

It is assumed that the folder containing `encs` is in `PATH`.

1. To mint tokens, execute:
```console
$ encs setup
```

2. To run the distribution algorithm, execute:
```console
$ encs run
```

3. You can verify the correctness of the distribution with this command:
```console
$ encs verify
```
It matches every entry in the "distribution.json" to the transaction ID in which the tokens are delivered to the address.

# Dependencies
To run this app you need [the haskell cabal](https://www.haskell.org/cabal/). The app has the following key dependencies:

- [ENCS](https://github.com/encryptedcoins/ENCS)
- [cardano-server](https://github.com/encryptedcoins/cardano-server)
- [plutus-tx-extra](https://github.com/encryptedcoins/plutus-tx-extra.git)
- [plutus-apps-extra](https://github.com/encryptedcoins/plutus-apps-extra)
- [plutus-apps](https://github.com/input-output-hk/plutus-apps)