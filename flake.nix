{

description = "A very simple preprocessor";

inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

outputs = { self, nixpkgs }: let
  pkgs = nixpkgs.legacyPackages.x86_64-linux;
  lib = pkgs.lib;
  haskellPackages = pkgs.haskell.packages.ghc94;
  yip = haskellPackages.callPackage ./yip.nix { };
in {
  packages.x86_64-linux.default = yip;

  devShells.x86_64-linux.default = haskellPackages.shellFor {
    packages = lib.const [ yip ];
    nativeBuildInputs = with haskellPackages; [
      ghc
      ghcid
      hlint
      cabal-install
      cabal2nix
    ];
  };
};

}
