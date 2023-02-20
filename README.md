# ENCS Distribution app
This is an implementation of the decentralized token distribution algorithm described [here](https://encoins-crypto.medium.com/fully-decentralized-token-distribution-on-cardano-9d7317d8de6).

# How to use

1. Fill [encs-params.json file](https://github.com/encryptedcoins/encs-distribution-app/blob/main/result/testnet-preview/encs-params.json).You should provide txOutReference (both hash and ID) from which you will mint encs-tokens and amount of these tokens respectively. You also need to create blockfrost.token file and fill it with your token which you can get [here](https://blockfrost.dev/).

2. Execute minting transaction:
```console
$ cabal run encs -- setup
```

3. Execute distributing transactions:
```console
$ cabal run encoinsRelayServer -- run
```

4. You can also verify the completeness of your distribution with this command:
```console
$ cabal run encoinsRelayServer -- verify
```

# Dependencies
To run this app you need [the haskell cabal](https://www.haskell.org/cabal/). This app also has the following dependencies:

- [ENCS](https://github.com/encryptedcoins/ENCS)
- [cardano-server](https://github.com/encryptedcoins/cardano-server)
- [plutus-tx-extra](https://github.com/encryptedcoins/plutus-tx-extra.git)
- [plutus-apps-extra](https://github.com/encryptedcoins/plutus-apps-extra)
- [plutus-apps](https://github.com/input-output-hk/plutus-apps)
- [cardano-wallet](https://github.com/input-output-hk/cardano-wallet)
- [quickcheck-contractmodel](https://github.com/Quviq/quickcheck-contractmodel)
- [cardano-addresses](https://github.com/input-output-hk/cardano-addresses)
- [cardano-ledger](https://github.com/input-output-hk/cardano-ledger)