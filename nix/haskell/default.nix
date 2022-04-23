final: prev:

let
  project = final.callPackage ./project.nix { };
  inherit (project.jpg-store-bulk-purchase.components.exes)
    integration-tests
    create-smart-contract;
  cardano-cli = "${prev.cardano-cli}/bin/cardano-cli";
in
{
  jpg-store-bulk-purchase = {
    inherit project integration-tests create-smart-contract;

    assets = final.runCommand "jpg-store-bulk-purchase-assets" { } ''
      mkdir -p $out/{mainnet,testnet}

      ${create-smart-contract}/bin/create-smart-contract $out/swap.plutus

      ${cardano-cli} address build \
        --payment-script-file $out/swap.plutus \
        --mainnet \
        --out-file $out/mainnet/swap.addr

      ${cardano-cli} address build \
        --payment-script-file $out/swap.plutus \
        --testnet-magic 1097911063 \
        --out-file $out/testnet/swap.addr
    '';

    update-materialized = final.writeShellScriptBin "update-materialized" ''
      set -euo pipefail

      ${project.plan-nix.passthru.calculateMaterializedSha} > nix/haskell/plan-sha256
      mkdir -p nix/haskell/materialized
      ${project.plan-nix.passthru.generateMaterialized} nix/haskell/materialized
    '';
  };
}
