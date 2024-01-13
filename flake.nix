{

description = "A very simple preprocessor";

inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

outputs = { self, nixpkgs }: let
  supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
  allSystems = output: nixpkgs.lib.genAttrs supportedSystems
    (system: output nixpkgs.legacyPackages.${system});

  haskellPackages = pkgs: pkgs.haskell.packages.ghc94;
  yip = pkgs: (haskellPackages pkgs).callPackage ./yip.nix { };
in {
  packages = allSystems (pkgs: {
    default = yip pkgs;
  });

  devShells = allSystems (pkgs: {
    default = (haskellPackages pkgs).shellFor {
      packages = pkgs.lib.const [ (yip pkgs) ];
      nativeBuildInputs = with (haskellPackages pkgs); [
        ghc
        ghcid
        hlint
        cabal-install
        cabal2nix
      ];
    };
  });
};

}
