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
  buildInputs = old.buildInputs ++ (builtins.attrValues devTools);
  shellHook = ''
    # ghcid with linting
    function run-ghcid () {
      ${ghcid} --lint='${hlint}' --poll=1 -c \
      "${cabal} new-repl --write-ghc-environment-files=never $1"
    }

    # ghcid with linting, incl. running the built program.
    function run-ghcid-main () {
      ${ghcid} -r --lint='${hlint}' --poll=1 -c \
      "${cabal} new-repl --write-ghc-environment-files=never $1"
    }
  '';
})
