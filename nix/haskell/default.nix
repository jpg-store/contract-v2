final:

let
  project = final.callPackage ./project.nix { };
in
_prev:
{
  jpg-store-bulk-purchase = {
    inherit project;

    # TODO add exes?
  };
}
