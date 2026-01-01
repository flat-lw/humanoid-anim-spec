{
  description = "Humanoid Animation Generator - Unity Humanoid rig animation clip generator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        humanoid-anim = haskellPackages.callCabal2nix "humanoid-anim" ./. { };

      in {
        packages = {
          default = humanoid-anim;
          humanoid-anim = humanoid-anim;
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell toolchain
            ghc
            cabal-install
            haskell-language-server

            # Development tools
            haskellPackages.hlint
            haskellPackages.ormolu
            haskellPackages.cabal-fmt

            # Libraries (for cabal repl/build)
            zlib
          ];

          shellHook = ''
            echo "Humanoid Animation Generator Development Environment"
            echo "GHC version: $(ghc --version)"
            echo ""
            echo "Commands:"
            echo "  cabal build    - Build the project"
            echo "  cabal run      - Run the CLI"
            echo "  cabal test     - Run tests"
            echo "  cabal repl     - Start REPL"
          '';
        };
      }
    );
}
