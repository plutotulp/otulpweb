with (import ./default.nix);
let
  ghcid =
    "${misopkgs.pkgs.haskell.packages.ghc865.ghcid}/bin/ghcid";
  cabal =
    "${misopkgs.pkgs.haskell.packages.ghc865.cabal-install}/bin/cabal";
in
webclient-dev.env.overrideAttrs (old: {
  shellHook = ''
    function gcid_webclient_jsaddle () {
      pushd otulpweb-webclient 2>/dev/null
      ${ghcid} -r --poll=1 -c '${cabal} new-repl --write-ghc-environment-files=never'
    }
  '';
})
