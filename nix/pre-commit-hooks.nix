{ findutils
, git
, gnugrep
, hsTools
, lib
, pre-commit-hooks
, system
, writeShellScriptBin
}:

let
  # overrides brittany, hlint, etc. to be the same as those in our dev shell
  tools = pre-commit-hooks.packages.${system} // hsTools;
  pre-commit-check = pre-commit-hooks.lib.${system}.run {
    inherit tools;

    src = ./..;
    excludes = [
      "nix/haskell/materialized/"
      "dist-newstyle/"
    ];
    hooks =
      {
        brittany.enable = true;
        cabal-fmt.enable = true;
        hlint.enable = true;
        nix-linter.enable = true;
        nixpkgs-fmt.enable = true;
      };
  };

  format = writeShellScriptBin "format" ''
    set -euo pipefail

    PATH="${lib.makeBinPath [
      git
      gnugrep
      findutils
      hsTools.cabal-fmt
      hsTools.brittany
      pre-commit-hooks.packages.${system}.nixpkgs-fmt
    ]}"

    git ls-files | grep -v ^nix/haskell/materialized | grep .nix$ | xargs nixpkgs-fmt
    git ls-files | grep .hs$ | xargs brittany --write-mode inplace --config-file ${../brittany.yaml}
    git ls-files | grep .cabal$ | xargs cabal-fmt --inplace
  '';
in
{
  inherit pre-commit-check format;
  inherit (pre-commit-check) shellHook;

  shellBuildInputs =
    [ format ]
    ++ (with pre-commit-hooks.packages.${system}; [
      nixpkgs-fmt
      nix-linter
    ])
    ++ (builtins.attrValues hsTools);
}

