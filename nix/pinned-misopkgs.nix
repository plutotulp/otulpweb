import (builtins.fetchTarball {
  # A Miso version of nixpkgs. Use with cachix "cachix use miso" to
  # avoid waiting forever for GHCJS to compile (and then running out
  # of memory before it finishes).
  url = "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
  sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
}) {}
