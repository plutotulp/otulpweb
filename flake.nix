{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      mkSystemOutputs = system:
        let pkgs =
              import nixpkgs {
                inherit system;
                overlays = [ self.overlays.default ];
              };
        in {
          packages.default = pkgs.otulpweb;
          devShells.default = pkgs.callPackage ./shell.nix {};
        };
    in
      {
        inherit inputs;
        overlays.default = import ./overlay.nix;
      } // flake-utils.lib.eachSystem systems mkSystemOutputs;
}
