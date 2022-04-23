{ version
, system ? builtins.currentSystem
}:

let
  flake = builtins.getFlake (toString ./.);
  pkgs = flake.legacyPackages.${system};
  inherit (pkgs.jpg-store-bulk-purchase.project.jpg-store-bulk-purchase) src;
  inherit (pkgs.jpg-store-bulk-purchase) assets;
in
pkgs.runCommand "jpg-store-bulk-purchase-${version}.tar.gz" { } ''
  DIR=jpg-store-bulk-purchase-${version}

  mkdir -p $DIR

  # copy the source
  cp --recursive --no-preserve=all ${src}/. $DIR/.

  # copy the assets (plutus script file, address files)
  cp --recursive --no-preserve=all ${assets}/. $DIR/.

  # create the tarball
  tar -zcf $out $DIR
''
