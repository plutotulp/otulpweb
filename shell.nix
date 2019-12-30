# Sets up development environment, currently only for the webclient
# package. Run the 'run-webclient-ghcid' command to start a ghcid
# loop.

let
  def =
    import ./default.nix {};
  misoGhcid =
    "${def.misopkgs.pkgs.haskell.packages.ghc865.ghcid}/bin/ghcid";
  misoCabal =
    "${def.misopkgs.pkgs.haskell.packages.ghc865.cabal-install}/bin/cabal";
in
def.webclient-dev.env.overrideAttrs (old: {
  shellHook = ''
    function run-webclient-ghcid () {
      pushd otulpweb-webclient >/dev/null
      ${misoGhcid} --poll=1 -c '${misoCabal} new-repl --write-ghc-environment-files=never'
    }
  '';
  # shellHook = ''
  #   function run-webclient-ghcid () {
  #     pushd otulpweb-webclient >/dev/null
  #     ${misoGhcid} -r --poll=1 -c '${misoCabal} new-repl --write-ghc-environment-files=never'
  #   }
  # '';
})
