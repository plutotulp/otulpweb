{ sources  ? import ../nix/sources.nix
, nixpkgs  ? import sources.nixpkgs {}
, misopkgs ? import sources.miso {}
}:
let
  cfg =
    (import ../. { inherit nixpkgs misopkgs; }).cfg;
in
{

  # This is used by nix-shell.
  shell = {

    ghcid =
      cfg.webclient.ghc.haskellPackages.ghcid;

    cabal-install =
      cfg.webclient.ghc.haskellPackages.cabal-install;

    hlint =
      nixpkgs.hlint;

  };

  ghc = {
    server =
      import ./otulpweb-common.nix {
        haskellPackages = cfg.server.ghc.haskellPackages;
      };
    webclient =
      import ./otulpweb-common.nix {
        haskellPackages = cfg.webclient.ghc.haskellPackages;
      };
  };
  ghcjs =
    import ./otulpweb-common.nix {
      haskellPackages = cfg.webclient.ghcjs.haskellPackages;
    };

}
