set -eux
thisDir=$(dirname "$0")
mainDir=$thisDir/..

initialUtxo=${1:-$(./scripts/query/seller.sh | tail -1 | head | cardano-cli-balance-fixer parse-as-utxo)}

(
cd $mainDir
cabal run create-smart-contract -- \
  --swap-output assets/swap.plutus \
  --nft-output assets/nft.plutus \
  --nft-policy-id-output assets/nft-policy-id.txt \
  --market-place-fee 25 \
  --nft-utxo $initialUtxo \
  --nft-token-name "CONFIG"
)

cardano-cli address build \
  --payment-script-file $mainDir/assets/swap.plutus \
  --mainnet \
  --out-file $mainDir/assets/mainnet/swap.addr

cardano-cli address build \
  --payment-script-file $mainDir/assets/swap.plutus \
  --testnet-magic 1097911063 \
  --out-file $mainDir/assets/testnet/swap.addr
