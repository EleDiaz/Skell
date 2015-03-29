with (import <nixpkgs> {}).pkgs;
(haskellngPackages.callPackage ./default.nix {}).env
