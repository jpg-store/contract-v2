set -eu
thisDir=$(dirname "$0")
mainDir=$thisDir/..

(
cd $mainDir
cabal run create-smart-contract -- assets/swap.plutus
)

cardano-cli address build \
  --payment-script-file $mainDir/assets/swap.plutus \
  --mainnet \
  --out-file $mainDir/assets/mainnet/swap.addr

cardano-cli address build \
  --payment-script-file $mainDir/assets/swap.plutus \
  --testnet-magic 1097911063 \
  --out-file $mainDir/assets/testnet/swap.addr
