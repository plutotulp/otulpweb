import (builtins.fetchTarball {
  # A Miso version of nixpkgs. Use with cachix "cachix use miso" to
  # avoid waiting forever for GHCJS to compile (and then running out
  # of memory before it finishes).
  url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
  sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
}) {}
