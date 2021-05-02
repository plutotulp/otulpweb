{ nixpkgs  ? import ../nix/pinned-nixpkgs.nix
, misopkgs ? import ../nix/pinned-misopkgs.nix
}:
let
  def =
    import ../. { inherit nixpkgs misopkgs; };
  cfg =
    def.cfg;
  common =
    def.common;
in
{

  # This is used by nix-shell.
  shell = {

    ghcid =
      cfg.webclient.ghc.haskellPackages.ghcid;

    cabal-install =
      cfg.webclient.ghc.haskellPackages.cabal-install;

  };

  ghc =
    cfg.webclient.ghc.haskellPackages.callCabal2nix
      "otulpweb-webclient" ./. {
        miso = misopkgs.miso-jsaddle;
        otulpweb-common = common.ghc.webclient;
      };

  # Build webclient for release, using ghcjs.
  ghcjs =
    cfg.webclient.ghcjs.haskellPackages.callCabal2nix
      "otulpweb-webclient" ./. {
        otulpweb-common = common.ghcjs;
      };
}
