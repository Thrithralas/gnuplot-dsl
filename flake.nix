{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.fourmolu-nix.flakeModule
      ];

      perSystem = { self', pkgs, config, ... }: {

        haskellProjects.default = {
          devShell = {
            hlsCheck.enable = true;
            tools = _: { fourmolu = config.fourmolu.wrapper; };
          };
          autoWire = [ "packages" "apps" "checks" ];
        };

        treefmt.config = {
          projectRootFile = "flake.nix";
          programs.fourmolu = {
            enable = true;
            package = config.fourmolu.wrapper;
          };
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;
        };

        fourmolu.settings = {
          indentation = 2;
          comma-style = "trailing";
          indent-wheres = true;
          newlines-between-decls = 1;
          single-constraint-parens = "never";
          import-export-style = "diff-friendly";
          respectful = true;
          haddock-style = "single-line";
        };

        packages.default = self'.packages.gnuplot-dsl;
        devShells.default = pkgs.mkShell {
          name = "gnuplot-dsl-devshell";
          meta.description = "Gnuplot DSl devshell";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.treefmt.build.devShell
          ];
        };
      };
    };
}
