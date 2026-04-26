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
      systems = [ "x86_64-linux" ];
      misopkgsFor = system: import miso { inherit system; };
      pkgsFor =
        system:
        import nixpkgs {
          config = { };
          localSystem.system = system;
          overlays = [ self.overlays.default ];
        };
    in
    {
      nixosModules.default = import ./module.nix;

      overlays = {

        default = nixpkgs.lib.composeManyExtensions (
          builtins.attrValues (builtins.removeAttrs self.overlays [ "default" ])
        );

        miso =
          final: prev:
          (
            let
              misopkgs = misopkgsFor final.stdenv.hostPlatform.system;
            in
            {
              misoHaskellPackages = misopkgs.pkgs.haskellPackages;
              misoGhcjsHaskellPackages = misopkgs.pkgs.haskell.packages.ghcjs;
              misoJsaddle = misopkgs.miso-jsaddle;
            }
          );

        gitignore = final: prev: {
          gitignoreSource = gitignore.lib.gitignoreSource;
        };

        bootstrap = final: prev: {
          bootstrap = prev.callPackage ./bootstrap.nix { };
        };

        generate-otulpweb-cabal-nix-files = final: prev: {
          generate-otulpweb-cabal-nix-files = prev.callPackage ./generate-otulpweb-cabal-nix-files.nix { };
        };

        otulpweb-common = final: prev: {
          otulpweb-common-server =
            prev.haskellPackages.callPackage ./otulpweb-common.nix { };
          # otulpweb-common-webclient-ghc =
          #   prev.misoHaskellPackages.callPackage ./otulpweb-common-webclient-ghc.nix { };
          otulpweb-common-webclient-ghcjs =
            prev.misoGhcjsHaskellPackages.callPackage ./otulpweb-common-webclient-ghcjs.nix { };
        };

        otulpweb-webclient = final: prev: {
          # otulpweb-webclient-ghc =
          #   prev.misoHaskellPackages.callPackage ./otulpweb-webclient-ghc.nix {
          #     miso = prev.misoJsaddle;
          #     otulpweb-common = prev.otulpweb-common-webclient-ghc;
          #   };
          otulpweb-webclient-ghcjs =
            prev.misoGhcjsHaskellPackages.callPackage ./otulpweb-webclient-ghcjs.nix {
              otulpweb-common = prev.otulpweb-common-webclient-ghcjs;
            };
        };

        otulpweb-webclient-closurecompiled = final: prev: {
          otulpweb-webclient-closurecompiled =
            prev.callPackage ./otulpweb-webclient-closurecompiled.nix { };
        };

        otulpweb-server = final: prev: {
          otulpweb-server =
            prev.haskellPackages.callPackage ./otulpweb-server.nix {
              otulpweb-common = prev.otulpweb-common-server;
            };
        };

        otulpweb = final: prev: {
          otulpweb = prev.callPackage ./otulpweb.nix { };
        };

      };
    }
    // flake-utils.lib.eachSystem systems (
      system:
      let
        pkgs = pkgsFor system;
      in
      {

        formatter = pkgs.nixfmt;

        devShells.default = pkgs.mkShell {
          packages = builtins.attrValues {
            inherit (pkgs)
              niv
              hlint
              cabal-install
              ghcid
              ;
          };
        };

        packages = {
          inherit (pkgs)
            cabal2nix
            bootstrap
            generate-otulpweb-cabal-nix-files
            otulpweb-common-server
            #otulpweb-common-webclient-ghc
            otulpweb-common-webclient-ghcjs
            #otulpweb-webclient-ghc
            otulpweb-webclient-ghcjs
            otulpweb-webclient-closurecompiled
            otulpweb-server
            otulpweb
          ;
          misoCabal2nix = pkgs.misoHaskellPackages.cabal2nix;
          default = pkgs.otulpweb;
        };

        apps.generate-otulpweb-cabal-nix-files = {
            type = "app";
            program = "${nixpkgs.lib.getExe pkgs.generate-otulpweb-cabal-nix-files}";
            # nix flake check klager hvis jeg ikke har meta?!
            meta.description = "generate otulpweb-*.nix files from cabal files";
        };

        checks.default = pkgs.testers.nixosTest {

          name = "otulpweb-service-starts";

          interactive.sshBackdoor.enable = true;

          nodes = {
            machine = {
              imports = [
                { config.nixpkgs.overlays = [ self.overlays.default ]; }
                self.nixosModules.default
              ];

              config.services.otulpweb.enable = true;
              config.services.otulpweb.settings.listenPort = 9090;
            };
          };

          testScript =
            { nodes, ... }:
            ''
              start_all()
              machine.wait_for_unit("multi-user.target")
              machine.wait_for_unit("otulpweb.service")
              machine.wait_for_open_port(${builtins.toString nodes.machine.services.otulpweb.settings.listenPort})
              machine.succeed("curl http://localhost:${builtins.toString nodes.machine.services.otulpweb.settings.listenPort}")
            '';

        };

      }
    );
}
