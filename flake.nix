{
  description = "A Plutus flake";

  inputs = {
    # we use this to get newer versions of cardano-node and cardano-cli than are
    # use in plutus-apps. we can probably get rid of this when plutus-apps is
    # updated
    cardano-node = {
      url = "github:input-output-hk/cardano-node/1.34.1";
    };

    cardano-cli-balance-fixer = {
      url = "github:Canonical-LLC/cardano-cli-balance-fixer";
    };

    flake-utils.url = "github:numtide/flake-utils";

    plutus-apps.url = "github:input-output-hk/plutus-apps/plutus-starter-devcontainer/v1.0.14";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = { cardano-cli-balance-fixer, cardano-node, flake-utils, plutus-apps, pre-commit-hooks, ... }:
    flake-utils.lib.eachSystem (import ./supported-systems.nix)
      (system:
        let
          plutus = import plutus-apps { inherit system; };
          pkgs = plutus.pkgs.appendOverlays [
            (_: _: {
              inherit plutus;

              inherit (cardano-node.packages.${system}) cardano-node cardano-cli;
            })

            (import ./nix/haskell)
          ];
          inherit (pkgs.jpg-store-bulk-purchase) project update-materialized;

          flake = project.flake { };

          hsTools = project.tools (import ./nix/haskell/tools.nix { inherit (pkgs.jpg-store-bulk-purchase.project) index-state; });

          pre-commit = pkgs.callPackage ./nix/pre-commit-hooks.nix { inherit pre-commit-hooks hsTools; };

        in
        pkgs.lib.recursiveUpdate flake {
          checks = {
            inherit (pre-commit) pre-commit-check;
          };

          # so `nix build` will build the exe
          # defaultPackage = flake.packages."jpg-store-bulk-purchase:exe:jpg-store-bulk-purchase";

          # so `nix run`  will run the exe
          defaultApp = {
            # type = "app";
            # program = "${flake.packages."jpg-store-bulk-purchase:exe:jpg-store-bulk-purchase"}/bin/jpg-store-bulk-purchase";
          };

          apps = {
            inherit (cardano-node.apps.${system}) cardano-node cardano-cli;

            update-materialized = {
              type = "app";
              program = "${update-materialized}/bin/update-materialized";
            };

            format = {
              type = "app";
              program = "${pre-commit.format}/bin/format";
            };
          };

          devShell =
            flake.devShell.overrideAttrs (attrs: {
              inherit (pre-commit) shellHook;

              buildInputs = attrs.buildInputs ++ [
                update-materialized
                plutus.plutus-apps.haskell-language-server
                cardano-cli-balance-fixer.defaultPackage.${system}
              ] ++ (with cardano-node.packages.${system}; [
                cardano-node
                cardano-cli
              ]) ++ pre-commit.shellBuildInputs;
            });

          legacyPackages = pkgs;
        }
      );
}
