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

First source either the testnet or mainnet environment variables.

For testnet

```
$ source scripts/envars/testnet-env.envars
```

For mainnet

```
$ source scripts/envars/mainnet-env.envars
```

The environment variable files set `CARDANO_NODE_SOCKET_PATH` to the path of the appropriate Daedalus socket file (either Testnet Daedalus or the regular mainnet Daedalus). It you run a `cardano-node` on your own you should set this environment variable to your socket file location after sourcing the environment variable file.

First create the wallets and get the protocol parameters.

```
$ ./scripts/wallets/make-all-wallets.sh
$ ./scripts/query-protocol-parameters.sh
```

Next, run:

```bash
scripts/compile.sh
```

This will make the files `scripts/testnet/escrow.addr` or `scripts/mainnet/escrow.addr`.

## Example Transactions

The folder `scripts/core` has parameterized example transactions. These are used by the wrappers in `scripts/happy-path` and `scripts/failure-cases`. The various transactions are combined in test scripts in the folder `scripts/tests`.

## Running the Tests

To run the tests run `scripts/tests/all.sh`
