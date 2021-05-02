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
