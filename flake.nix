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

        otulpweb = final: prev: {
          otulpweb = prev.callPackage ./package.nix { };
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

        packages.default = pkgs.otulpweb;

        checks.default = pkgs.testers.nixosTest {

          name = "otulpweb-service-starts";

          # FIXME: Needs nix flake update first to be available
          #interactive.sshBackdoor.enable = true;

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
