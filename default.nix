{ system ? builtins.currentSystem
}:

(builtins.getFlake (toString ./.)).legacyPackages.${system}.jpg-store-bulk-purchase.project
