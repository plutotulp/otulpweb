{ sources  ? import ./nix/sources.nix
, nixpkgs  ? import sources.nixpkgs {}
, misopkgs ? import sources.miso {}
}:
let
  cfg = {
    webclient = {
      ghc = {
        haskellPackages = misopkgs.pkgs.haskell.packages.ghc865;
      };
      ghcjs = {
        haskellPackages = misopkgs.pkgs.haskell.packages.ghcjs;
      };
    };
    server = {
      ghc = {
        haskellPackages = nixpkgs.haskell.packages.ghc865;
      };
    };
  };
in
{
  inherit misopkgs nixpkgs cfg;

  webclient =
    import ./otulpweb-webclient { inherit nixpkgs misopkgs; };

  common =
    import ./otulpweb-common {inherit nixpkgs misopkgs; };

  server =
    import ./otulpweb-server {inherit nixpkgs misopkgs; };

}
