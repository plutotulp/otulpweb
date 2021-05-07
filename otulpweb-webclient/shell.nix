#  # ghcid session for library code
#  nix-shell --run run-ghcid
#
#  # ghcid session for test suite
#  nix-shell --run run-ghcid test

let proj = import ../. {};
in (import ../nix/mkShell.nix) proj.devTools proj.otulpweb-webclient.ghc
