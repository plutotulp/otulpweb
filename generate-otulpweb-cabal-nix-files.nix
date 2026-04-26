{ writeShellScriptBin, cabal2nix, misoHaskellPackages }:
let
  name = "generate-otulpweb-cabal-nix-files";
  cabal2nixCmd = "${cabal2nix}/bin/cabal2nix";
  misoCabal2nixCmd = "${misoHaskellPackages.cabal2nix}/bin/cabal2nix";
in
writeShellScriptBin "${name}"
  ''
  set -euo pipefail
  ${cabal2nixCmd}                      otulpweb-common    >otulpweb-common.nix
  ${misoCabal2nixCmd}                  otulpweb-common    >otulpweb-common-webclient-ghc.nix
  ${misoCabal2nixCmd} --compiler ghcjs otulpweb-common    >otulpweb-common-webclient-ghcjs.nix

  ${misoCabal2nixCmd}                  otulpweb-webclient >otulpweb-webclient-ghc.nix
  ${misoCabal2nixCmd} --compiler ghcjs otulpweb-webclient >otulpweb-webclient-ghcjs.nix

  ${cabal2nixCmd}                      otulpweb-server    >otulpweb-server.nix
  ''
// {
  meta = {
    description =
      ''
      Utility for the otulpweb git repository that generates
      otulpweb-*.nix files from underlying Haskell cabal files. Uses
      the cabal2nix tool. This tools must be re-run in the repository
      directory every time one of the cabal files changes, in order
      for nix to pick up changes in Haskell dependencies.
      '';
    mainProgram = "${name}";
  };
}
