{ version
, system ? builtins.currentSystem
}:

let
  flake = builtins.getFlake (toString ./.);
  pkgs = flake.legacyPackages.${system};
  src = pkgs.jpg-store-bulk-purchase.project.jpg-store-bulk-purchase.src;
  apps = flake.apps.${system};
  create-smart-contract = apps."jpg-store-bulk-purchase:exe:create-smart-contract".program;
  cardano-cli = apps.cardano-cli.program;
in
pkgs.runCommand "jpg-store-bulk-purchase-${version}.tar.gz" { } ''
  DIR=jpg-store-bulk-purchase-${version}

  mkdir -p $DIR

  # copy the source
  cp --recursive --no-preserve=all ${src}/. $DIR/.

  # create the assets
  mkdir -p $DIR/assets/{testnet,mainnet}
  ${create-smart-contract} $DIR/assets/swap.plutus

  ${cardano-cli} address build \
    --payment-script-file $DIR/assets/swap.plutus \
    --mainnet \
    --out-file $DIR/assets/mainnet/swap.addr

  ${cardano-cli} address build \
    --payment-script-file $DIR/assets/swap.plutus \
    --testnet-magic 1097911063 \
    --out-file $DIR/assets/testnet/swap.addr

  # create the tarball
  tar -zcf $out $DIR
''
