{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    zig-overlay = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zls-overlay = {
      url = "github:zigtools/zls";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        zig-overlay.follows = "zig-overlay";
      };
    };

    pre-commit-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , zig-overlay
    , zls-overlay
    , pre-commit-hooks
    ,
    }:
    flake-utils.lib.eachSystem
      [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ]
      (
        system:
        let
          zigPackage = zig-overlay.packages.${system}.master;
          zlsPackage = zls-overlay.packages.${system}.zls;
          pkgs = nixpkgs.legacyPackages.${system};
        in
        rec {
          checks =
            let
              inherit (pkgs) lib;

              hooks = {
                nixpkgs-fmt = {
                  enable = true;
                  name = pkgs.lib.mkForce "Nix files format";
                };
                check-merge-conflicts.enable = true;
                check-shebang-scripts-are-executable.enable = true;
                check-added-large-files.enable = true;
                zig-fmt = {
                  enable = true;
                  entry = "${lib.getExe zigPackage} fmt --check .";
                  files = "\\.z(ig|on)$";
                };
              };
            in
            {
              pre-commit-check = pre-commit-hooks.lib.${system}.run {
                inherit hooks;
                src = ./.;
              };
            };

          formatter = pkgs.nixpkgs-fmt;
          devShells.default = pkgs.mkShell {
            inherit (checks.pre-commit-check) shellHook;
            packages = [
              zigPackage
              zlsPackage
              pkgs.nixfmt
            ];
          };
        }
      );
}
