# How to start a ghcid dev session for webclient:
#
#   nix-shell --run run-ghcid
#

let
  def =
    import ./default.nix {};
  ghcid =
    "${def.shell.ghcid}/bin/ghcid";
  cabal =
    "${def.shell.cabal-install}/bin/cabal";
in
def.ghc.env.overrideAttrs (old: {
  shellHook = ''
    function run-ghcid () {
      ${ghcid} --poll=1 -c '${cabal} new-repl --write-ghc-environment-files=never'
    }
  '';
})
