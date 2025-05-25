{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    miso = {
      url = "github:dmjio/miso";
      flake = false;
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      gitignore,
      miso,
      ...
    }:
    let
      systems = [
        "aarch64-linux"
        "x86_64-linux"
      ];
    in
    {
      overlays = {
        miso = final: prev: {
          miso = prev.callPackage miso { inherit (final) system; };
        };
        otulpweb = final: prev: {
          otulpweb = prev.callPackage ./. { inherit gitignore; };
        };
        default = [
          self.overlays.miso
          self.overlays.otulpweb
        ];
      };
      nixosModules.otulpweb = import ./otulpweb-module.nix;

      # FIXME: Denne kan dø.
      nixosConfigurations.blargh = let
        pkgs = import nixpkgs {

        };
      in
        nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = [
            { nixpkgs.overlays = self.overlays.default; }
            self.nixosModules.otulpweb
          ];
        };

    }
    // flake-utils.lib.eachSystem systems (system:
      let pkgs = import nixpkgs {
            inherit system;
            overlays = self.overlays.default;
          };
      in
      {
        formatter = pkgs.nixfmt-rfc-style;
        # packages = {

        #   otulpweb-common-server = pkgs.otulpweb.otulpweb-common.server;

        #   otulpweb-common-webclient-ghcjs = pkgs.otulpweb.otulpweb-common.webclient.ghcjs;
        #   otulpweb-common-webclient-ghc = pkgs.otulpweb.otulpweb-common.webclient.ghc;

        #   otulpweb-webclient-ghcjs = pkgs.otulpweb.otulpweb-webclient.ghcjs;
        #   otulpweb-webclient-ghc = pkgs.otulpweb.otulpweb-webclient.ghc;

        #   inherit (pkgs.otulpweb) otulpweb-server bootstrap;
        #   inherit (pkgs.otulpweb.devTools) cabal-install ghcid hlint;

        # };
      });
}
