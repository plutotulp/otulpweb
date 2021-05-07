# Make shell environment for a cabal project.
devTools: pkg:
let
  ghcid =
    "${devTools.ghcid.bin}/bin/ghcid";
  cabal =
    "${devTools.cabal-install}/bin/cabal";
  hlint =
    "${devTools.hlint}/bin/hlint";

in pkg.env.overrideAttrs (old: {
  shellHook = ''
    function run-ghcid () {
      target="$1"
      ${ghcid} --lint='${hlint}' --poll=1 -c \
      "${cabal} new-repl --write-ghc-environment-files=never $1"
    }
  '';
})
