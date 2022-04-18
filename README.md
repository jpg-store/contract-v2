# jpg.store Bulk Purchase Contract

## Assets

The Plutus Core and script addresses for testnet and mainnet are checked in the `assets` folder.

- `assets/swap.plutus`
- `assets/testnet/swap.addr`
- `assets/mainnet/swap.addr`

## Specification

Multiple sellers can list one or more assets in a lock transaction.

A buyer can buy one or more asset bundles from multiple buyers.

Seller "offer" or "buy" datum contains:
* owner pkh
* payouts
* assets being sold
* optional expiration

buyer submits "buy" for one or more "offer"s:
* includes the utxos and "offer" datums for each thing on offer along with "buy" redeemers that contains no additional info
* validation:
  * assume that all other redeemers are "buy" redeemers
    * expectation is that if one of the other redeemers is a different redeemer, either the validation of the "buy" redeemer or the validation of the other redeemers will fail
  * gathers all the datums as "offer" datums
    * sums up the assets from the "offer" datums and makes sure they are being paid to the signer (there should be only one)
    * sums up the payouts from the "offer" datums and make sure they are being paid out correctly

seller can "cancel" in bulk things they own
* includes the utxos and "offer" datums for each thing on offer along with "cancel" redeemers that contains no additional info
* validation:
  * assume that all other redeemers are "cancel" redeemers
    * expectation is that if one of the other redeemers is a different redeemer, either the validation of the "cancel" redeemer or the validation of the other redeemers will fail
  * gathers all the datums as "offer" datums
    * checks that the signer (there should be only one) is the owner of each
    * sums up the assets from the "offer" datums and makes sure they are being sent back to the owner(s)

a special compiled in pkh can do an "emergency cancel" of "offers" in bulk
* includes the utxos and "offer" datums for each thing on offer along with "emergency cancel" redeemers that contains no additional info
* validation:
  * assume that all other redeemers are "emergency cancel" redeemers
    * expectation is that if one of the other redeemers is a different redeemer, either the validation of the "emergency cancel" redeemer or the validation of the other redeemers will fail
  * check that the signer (there should be only one) is the special, compiled in pkh
  * gathers all the datums as "offer" datums
    * sums up the assets from the "offer" datums and makes sure they are being sent back to the owner(s)

after expiration has passed, anyone can bulk "close" "offers":
* includes the utxos and "offer" datums for each thing on offer along with "close" redeemers that contains no additional info
* validation:
  * assume that all other redeemers are "emergency cancel" redeemers
    * expectation is that if one of the other redeemers is a different redeemer, either the validation of the "emergency cancel" redeemer or the validation of the other redeemers will fail
  * gathers all the datums as "offer" datums
    * check that the expiration of each has passed
    * sums up the assets from the "offer" datums and makes sure they are being sent back to the owner(s)

implications of the above:

* a "close" can occur in any tx, even ones including "buy", "cancel", or "emergency cancel" redeemers
  * we only check the expiration has passed, not *who* is issuing it

* a "buy" *could* occur in the same tx as a "cancel"
  * the owner of the "offer" being cancelled is the buyer of another "offer"
  * could they exploit the SC in some way?
    * TBD

* a "buy" and "emergency cancel" *could* occur in the same tx
  * if the buyer is the compiled in pkh
  * could they exploit the SC in some way?
    * TBD

* a "cancel" and "emergency close" *could* occur in the same tx
  * if the owner of the offer being "cancelled" is the same as the special compiled in pkh
  * could they exploit the SC in some way?
    * TBD

* multiple different redeemers appearing in the same tx for the same "offer" datum will fail the tx, with some exceptions
  * "buy" and "cancel" would expect the asset to go to different addresses
    * unless the seller and buyer are the same
  * "buy" and "emergency cancel" would expected the asset to go to different addresses
    * unless the seller is the buyer and the special compiled in pkh
  * "buy" and "close" would fail either because the expiration is passed or because it hasn't
  * "cancel" and "close" would fail either because the expiration is passed or because it hasn't
  * "cancel" and "emergency cancel" would have different signers
    * unless the owner is the special compiled in pkh
  * "emergency cancel" and "close" would only work if
    * the signer was the special compiled in pkh and the expiration had passed


## Creating the Script Address

After compiling the smart contract, it is necessary to make a script address.

Next, run:

```bash
scripts/compile.sh
```

## Example Transactions

The `integration-tests/Main.hs` file has example transactions of various kinds.

## Running the Tests

Before running the tests you need to set up environment variables.

```bash
export CARDANO_NODE_SOCKET_PATH=<your node path>
```

Set `CARDANO_NODE_SOCKET_PATH` to the path of the appropriate Daedalus socket file (either Testnet Daedalus or the regular mainnet Daedalus). It you run a `cardano-node` on your own you should set this environment variable to your socket file location.

```bash
export WALLET_DIR=<dir with wallet skey and vkey files>
```

Set `WALLET_DIR` to a directory which contains the signing key files and verification key files of the wallets for `seller1`, `seller2`, `buyer`, `marketplace`, and `royalities` wallets. If the file `$WALLET_DIR/<name>.vkey` does not exist, a new wallet will be created and 100 ada transferred to it. If the `WALLET_DIR` is not set, a temporary directory will be used.

```bash
export SOURCE_WALLET_ADDRESS_PATH=<path to user addr file>
export SOURCE_WALLET_SKEY_PATH=<path to user skey file>
```

Set `SOURCE_WALLET_ADDRESS_PATH` and `SOURCE_WALLET_SKEY_PATH` to the files containing the wallet address and signing key that will be used as a source of funds for any newly created wallets. These are required, but if the `WALLET_DIR` is set and all the wallets already exist, they will not be used.

```bash
export TESTNET_MAGIC=1097911063
```

Set `TESTNET_MAGIC` if you are using a testnet. Otherwise, mainnet will be assumed.

### Environment Variable Files

To make setting the environment variables easier, there are three environment variable files you can source.

However, the paths to the `cardano-node` socket might need to be modified to work on your system.

The three files are:
- `scripts/envars/local-testnet.envs`
- `scripts/envars/testnet.envs`
- `scripts/envars/mainnet.envs`

To source them run:

```bash
source scripts/envars/testnet.envs
```

### Actually Running the Tests

To run the tests run

```bash
cabal run integration-tests
```
