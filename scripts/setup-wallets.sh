set -eux

mkdir -p temp/$BLOCKCHAIN_PREFIX

bodyFile=temp/consolidate-tx-body.01
signingKey=/Users/jonathanfischoff/prototypes/cardano-node/example/utxo-keys/utxo1.skey
senderAddr=$(cardano-cli address build --testnet-magic "42" --payment-verification-key-file /Users/jonathanfischoff/prototypes/cardano-node/example/utxo-keys/utxo1.vkey)
outFile=temp/consolidate-tx.01
sellerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)


cardano-cli transaction build \
  --babbage-era \
  $BLOCKCHAIN \
  $(cardano-cli-balance-fixer input --address $senderAddr $BLOCKCHAIN ) \
  --tx-out "$sellerAddr + 1500000000 lovelace" \
  --change-address $senderAddr \
  --protocol-params-file temp/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
  --out-file $bodyFile

echo "saved transaction to $bodyFile"

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $signingKey \
   $BLOCKCHAIN \
   --out-file $outFile

echo "signed transaction and saved as $outFile"

cardano-cli transaction submit \
 $BLOCKCHAIN \
 --tx-file $outFile

echo "submitted transaction"
