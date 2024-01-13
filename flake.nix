{

description = "A very simple preprocessor";

inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

outputs = { self, nixpkgs }: let
  supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
  allSystems = output: nixpkgs.lib.genAttrs supportedSystems
    (system: output nixpkgs.legacyPackages.${system});

  yip = pkgs: pkgs.haskellPackages.callPackage ./yip.nix { };
in {
  packages = allSystems (pkgs: {
    default = yip pkgs;
  });

  devShells = allSystems (pkgs: {
    default = pkgs.haskellPackages.shellFor {
      packages = pkgs.lib.const [ (yip pkgs) ];
      nativeBuildInputs = with pkgs.haskellPackages; [
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
