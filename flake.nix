{ inputs =
     {
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
    in
    {
      packages.${system}.default = otulpweb.otulpweb-deployment;
    };
}
