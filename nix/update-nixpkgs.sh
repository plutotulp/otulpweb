#!/usr/bin/env bash

set -eu

if [[ $# -eq 0 ]]; then
    cat <<EOF
USAGE: $(basename $0) LOCAL_NIXPKGS_REPO_DIR [BRANCH_NAME]
EOF
fi

local_nixpkgs_repo="$1"
if [[ $# -eq 2 ]]; then
    branch="$2"
else
    branch="haskell-updates"
fi

upstream="upstream"
fullbranch="${upstream}/${branch}"

yyyy_mm_dd=$(date -I)

pushd "$local_nixpkgs_repo" >/dev/null
git fetch "${upstream}"
rev=$(git rev-parse "$fullbranch")
popd >/dev/null

url=https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz
sha256=$(nix-prefetch-url --unpack "$url")

cat <<EOF >pinned-nixpkgs.nix
# Pinned version of nixpkgs, created by $(basename $0).
# Updated at ${yyyy_mm_dd}. Used nixpgks upstream
# branch ${branch}.
#
import (builtins.fetchTarball {
  url = "${url}";
  sha256 = "${sha256}";
}) {}
EOF
