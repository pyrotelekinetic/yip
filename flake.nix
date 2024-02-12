{

description = "A very simple preprocessor";

inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

outputs = { self, nixpkgs }: let
  supportedSystems = nixpkgs.lib.systems.flakeExposed;
  allSystems = output: nixpkgs.lib.genAttrs supportedSystems
    (system: output nixpkgs.legacyPackages.${system});
in {
  packages = allSystems (pkgs: {
    default = pkgs.haskellPackages.developPackage { root = ./.; };
  });

  devShells = allSystems (pkgs: {
    default = pkgs.haskellPackages.shellFor {
      packages = _: [ self.packages.${pkgs.system}.default ];
      nativeBuildInputs = with pkgs.haskellPackages; [
        ghc
        ghcid
        hlint
        cabal-install
      ];
    };
  });
};

}
