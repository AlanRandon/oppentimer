{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs =
    inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem =
        {
          self',
          # lib,
          ...
        }:
        {
          haskellProjects.default = {
            # Extra package information. See https://community.flake.parts/haskell-flake/dependency
            packages = { };
            settings = { };

            devShell = {
              enable = true;
            };
          };

          packages.default = self'.packages.project;
        };
    };
}
