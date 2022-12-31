{ pkgs ? import <nixpkgs> { } }:
with pkgs;

let
  haskellDeps = ps: with ps; [
    base
    lens
    mtl
    random
  ];
  haskellEnv = haskell.packages.ghc865.ghcWithPackages haskellDeps;
in mkShell {
  buildInputs = [
    ghc
    haskellPackages.cabal-install
    haskellPackages.stack
    haskell-language-server
  ];
}