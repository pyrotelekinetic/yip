{

description = "yip: a very simple preprocessor";

inputs = {
	nixpkgs = {
		type = "github";
		owner = "NixOS";
		repo = "nixpkgs";
		ref = "release-22.11";
	};
};

outputs = { self, nixpkgs }:
	let
		pkgs = import nixpkgs { system = "x86_64-linux"; };
		ghc = pkgs.haskellPackages.ghcWithPackages
			(a: with a; [
			]);
	in {
	packages.x86_64-linux.default =
		pkgs.stdenv.mkDerivation {
			name = "yip";
			src = ./src;
			buildInputs = [
				ghc
			];
			buildPhase = "ghc Main.hs -o yip";
			installPhase = "mkdir -p $out/bin; install -t $out/bin yip";
		};

	devShells.x86_64-linux.default = with pkgs;
		mkShell {
			buildInputs = [
				ghc
				ghcid
				hlint
			];
		};
	};

}
