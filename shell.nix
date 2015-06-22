let pkgs = import <nixpkgs> {};
    HaskellPackages = pkgs.haskellPackages;
    haskellPackages = HaskellPackages.override {
        extension = self: super: {
            skell = HaskellPackages.callPackage ./. {};
        };
    };
in pkgs.lib.overrideDerivation
    haskellPackages.skell (attrs: {
    noHaddock = true;
    buildInputs = [ ] ++ attrs.buildInputs;
})
