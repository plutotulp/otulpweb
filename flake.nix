{ inputs =
     {
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
    { self, nixpkgs, gitignore, miso, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      otulpweb = import ./. {
        inherit pkgs gitignore;
        miso = import miso { inherit system; };
      };

      otulpweb-module = { lib, config, pkgs, ... }:
        let
          cfg = config.services.otulpweb;
        in
        {
          imports = [];

          options.services.otulpweb = {
            enable = lib.mkOption {
              default = false;
              example = true;
              description = ''
          Run otulpweb service. Default configuration listens on port 8080 and
          uses all available cores.
          '';
            };
          };

          config = lib.mkIf cfg.enable {
            environment.systemPackages = [ otulpweb.otulpweb-deployment ];
            systemd.services.otulpweb = otulpweb.otulpweb-service;
          };
        };
    in
    {
      packages.${system}.default = otulpweb.otulpweb-deployment;
      nixosModules.default = otulpweb-module;
    };
}
