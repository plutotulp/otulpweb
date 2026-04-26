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
      misopkgsFrom = system: import miso { inherit system; };
      pkgsFrom =
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
              misopkgs = misopkgsFrom final.stdenv.hostPlatform.system;
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
    // (
      let
        system = "x86_64-linux";
        pkgs = pkgsFrom system;
      in
      {

        formatter.${system} = pkgs.nixfmt;

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

        packages.${system}.default = pkgs.otulpweb;

        checks.${system}.default = pkgs.testers.nixosTest {

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
