{ # Basically GHCJS and some miso-specific stuff.
  nixpkgs  ? import ./nix/pinned-nixpkgs.nix
, misopkgs ? import ./nix/pinned-misopkgs.nix
}:
let
  cfg = {
    webclient = {
      ghc = {
        # compiler = misopkgs.pkgs.haskell.compiler.ghc865;
        haskellPackages = misopkgs.pkgs.haskell.packages.ghc865;
      };
      ghcjs = {
        # compiler = misopkgs.pkgs.haskell.compiler.ghcjs;
        haskellPackages = misopkgs.pkgs.haskell.packages.ghcjs;
      };
    };
    server = {
      ghc = {
        # compiler = nixpkgs.haskell.compiler.ghc8104;
        haskellPackages = nixpkgs.haskell.packages.ghc8104;
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
