{ sources  ? import ../nix/sources.nix
, nixpkgs  ? import sources.nixpkgs {}
, misopkgs ? import sources.miso {}
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
      cfg.server.ghc.haskellPackages.ghcid;

    cabal-install =
      cfg.server.ghc.haskellPackages.cabal-install;

    hlint =
      nixpkgs.hlint;

  };

  ghc =
    cfg.server.ghc.haskellPackages.callCabal2nix
      "otulpweb-server" ./. {
        otulpweb-common = common.ghc.server;
      };

}
