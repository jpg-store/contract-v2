{ index-state }:

# haskell.nix tools configuration for use with `project.tools`.
# note: we don't add this directory to the shell in `project.nix`.
# instead, we configure them into the `devShell` directory so that
# we can reuse the same versions for the pre-commit-hooks
{
  brittany = {
    inherit index-state;

    version = "latest";
    # cabalProject = ''
    #   packages: .
    #   allow-newer: data-tree-print:base, butcher:base
    # '';
    # modules = [ nonReinstallablePkgsModule ];
  };
  fourmolu = {
    inherit index-state;
    version = "latest";
  };
  cabal-fmt = {
    inherit index-state;
    version = "latest";
  };
  cabal-install = {
    inherit index-state;
    version = "latest";
  };
  ghcid = {
    inherit index-state;
    version = "latest";
  };
  # see the devShell in flake.nix for HLS
  hlint = {
    inherit index-state;

    version = "latest";
    # modules = [ nonReinstallablePkgsModule ];
  };
}

