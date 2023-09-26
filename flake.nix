{

description = "A very simple preprocessor";

inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

outputs = { self, nixpkgs }:
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
    ghc = pkgs.haskellPackages.ghcWithPackages
      (a: with a; [
        megaparsec
        optparse-applicative
      ]);
  in {
  packages.x86_64-linux.default =
    pkgs.stdenv.mkDerivation {
      name = "yip";
      meta = {
        license = pkgs.lib.licenses.agpl3Plus;
        description = "A very simple preprocessor";
      };
      src = ./src;
      buildInputs = [
        ghc
      ];
      buildPhase = "ghc Main.hs -o yip";
      installPhase = "mkdir -p $out/bin; install -t $out/bin yip";
    };

  devShells.x86_64-linux.default = with pkgs;
    mkShell {
      packages = [
        ghcid
        hlint
      ];
      buildInputs = [
        ghc
      ];
    };
  };

}
