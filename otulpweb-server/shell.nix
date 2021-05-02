# How to start a ghcid dev session for otulpweb-common:
#
#   nix-shell --run run-ghcid
#

let
  def =
    import ./default.nix {};
  ghcid =
    "${def.shell.ghcid.bin}/bin/ghcid";
  cabal =
    "${def.shell.cabal-install}/bin/cabal";
  hlint =
    "${def.shell.hlint}/bin/hlint";

in
def.ghc.env.overrideAttrs (old: {
  shellHook = ''
      function run-ghcid () {
          ${ghcid} --lint='${hlint}' --poll=1 -c \
          '${cabal} new-repl --write-ghc-environment-files=never'
      }
  '';
})
