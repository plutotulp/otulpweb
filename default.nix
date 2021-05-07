{ sources  ? import ./nix/sources.nix
}:
let
  miso = import sources.miso {};
  pkgs = import sources.nixpkgs {};
  gitignore = import sources."gitignore.nix" { inherit (pkgs) lib; };
  gitignoreSource = gitignore.gitignoreSource;

  src = gitignoreSource ./.;

  # Choosing haskellPackage set also means you've selected the GHC
  # version.
  cfg = {
    webclient = {
      ghc = {
        haskellPackages = miso.pkgs.haskell.packages.ghc865;
      };
      ghcjs = {
        haskellPackages = miso.pkgs.haskell.packages.ghcjs;
      };
    };
    server = {
      haskellPackages = pkgs.haskell.packages.ghc8104;
    };
  };

  devTools = {
    inherit (pkgs) niv hlint cabal-install ghcid;
  };

  otulpweb-common =
    let
      mkDerivation = pkgs:
        pkgs.haskellPackages.callCabal2nix
          "otulpweb-common" ./otulpweb-common {};
    in {
      server          = mkDerivation cfg.server;
      webclient.ghc   = mkDerivation cfg.webclient.ghc;
      webclient.ghcjs = mkDerivation cfg.webclient.ghcjs;
    };

in let
  otulpweb-webclient =
    let
      mkDerivation = pkgs: args:
        pkgs.haskellPackages.callCabal2nix
          "otulpweb-webclient" ./otulpweb-webclient args;
    in {
      ghc = mkDerivation cfg.webclient.ghc {
        miso = miso.miso-jsaddle;
        otulpweb-common = otulpweb-common.webclient.ghc;
      };

      ghcjs = mkDerivation cfg.webclient.ghcjs {
        otulpweb-common = otulpweb-common.webclient.ghcjs;
      };

    };

  otulpweb-server =
    cfg.server.haskellPackages.callCabal2nix
      "otulpweb-server" ./otulpweb-server {
        otulpweb-common = otulpweb-common.server;
      };

in {

  inherit miso pkgs;
  inherit cfg src devTools;
  inherit otulpweb-common otulpweb-webclient otulpweb-server;

}
