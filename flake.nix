{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
      gitignore,
      miso,
      ...
    }:
    let
      system = "x86_64-linux";
      misopkgs = import miso { inherit system; };
      overlay = super: self: {
        misoHaskellPackages = misopkgs.pkgs.haskellPackages;
        misoGhcjsHaskellPackages = misopkgs.pkgs.haskell.packages.ghcjs;
        misoJsaddle = misopkgs.miso-jsaddle;
        gitignoreSource = gitignore.lib.gitignoreSource;
      };
      pkgs = import nixpkgs {
        config = {};
        inherit system;
        overlays = [ overlay ];
      };
      otulpweb = pkgs.callPackage ./package.nix { };
    in
    {
      inherit nixpkgs;

      nixosModules.default = import ./module.nix;

      overlays.default = final: prev: {
        inherit (overlay) misoHaskellPackages misoGhcjsHaskellPackages misoJsaddle gitignoreSource;
        inherit otulpweb;
      };

      formatter.${system} = pkgs.nixfmt-rfc-style;

      devShells.${system}.default = pkgs.mkShell {
        packages = builtins.attrValues {
          inherit (pkgs)
            niv
            hlint
            cabal-install
            ghcid
            ;
        };
      };

      packages.${system}.default = otulpweb;

      checks.${system}.default = pkgs.nixosTest {

        name = "otulpweb-service-starts";

        # FIXME: Needs nix flake update first to be available
        #interactive.sshBackdoor.enable = true;

        nodes = {
          machine =
            {
              imports = [
                { config.nixpkgs.overlays = [ self.overlays.default ]; }
                self.nixosModules.default
              ];

              config.services.otulpweb.enable = true;
              config.services.otulpweb.settings.listenPort = 9090;
            };
        };

        testScript = { nodes, ... }:
          ''
            start_all()
            machine.wait_for_unit("multi-user.target")
            machine.wait_for_unit("otulpweb.service")
            machine.wait_for_open_port(${builtins.toString nodes.machine.services.otulpweb.settings.listenPort})
            machine.succeed("curl http://localhost:${builtins.toString nodes.machine.services.otulpweb.settings.listenPort}")
          '';

      };

    };
}
