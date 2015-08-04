{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, data-default, lens, mtl
      , parsec, pipes, process, reflex, reflex-dom, stdenv, stm, text
      , yi-rope
      }:
      mkDerivation {
        pname = "Skell";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        buildDepends = [
          base containers data-default lens mtl parsec pipes process reflex
          reflex-dom stm text yi-rope
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
