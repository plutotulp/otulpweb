#  # ghcid session
#  nix-shell --run run-ghcid
#
#  # ghcid session for test suite
#  nix-shell --run run-ghcid test

let proj = import ../. {};
in (import ../nix/mkShell.nix) proj.devTools proj.otulpweb-server
