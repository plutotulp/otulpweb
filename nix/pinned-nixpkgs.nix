# Pinned version of nixpkgs, created by update-nixpkgs.sh.
# Updated at 2021-05-02. Used nixpgks upstream
# branch haskell-updates.
#
import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/3224bded7b2fca2e0c6a91f05964fef8b05c3b81.tar.gz";
  sha256 = "1vj07qinbmrnl5pll669lshyx2klmr1nmvbrnk4l64lnqzmv58h2";
}) {}
