{
  misopkgs ?
  import (builtins.fetchTarball {
    # A Miso version of nixpkgs. Use with cachix to avoid waiting
    # forever for GHCJS to compile (and then running out of memory
    # before it finishes).
    url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
    sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
  }) {}

, nixpkgs ?
  import (builtins.fetchTarball {
    # Pinned nixpkgs, this one from the haskell-packages branch at
    # 2019-12-30.
    url = "https://github.com/NixOS/nixpkgs/archive/07d0412f8de5ea6051ab22a7577a97705699459e.tar.gz";
    sha256 = "0f2ly7si7cb9g7qg4jzscqip1bzb3zpzvp6rn1sadzc11smrmz6a";
  }) {}
}:
{
  # Required by shell.nix.
  misopkgs =
    misopkgs;

  # Run tight interpreter loop with ghcid, using ghc and jsaddle
  # instead of ghcjs. This is good for catching type errors, but less
  # so for actually running the code.
  webclient-dev =
    with misopkgs;
    pkgs.haskell.packages.ghc865.callCabal2nix
      "otulpweb-webclient" ./otulpweb-webclient { miso = miso-jsaddle; };

  # Build webclient for release, using ghcjs.
  webclient =
    with misopkgs;
    pkgs.haskell.packages.ghcjs.callCabal2nix
      "otulpweb-webclient" ./otulpweb-webclient {};

  # Build server for release.
  server =
    with nixpkgs;
    haskellPackages.callCabal2nix "otulpweb-server" ./otulpweb-server {};

}
