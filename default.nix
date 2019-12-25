let
  misopkgs = import (builtins.fetchTarball {
      url = "https://github.com/dmjio/miso/archive/561ffad.Tar.gz";
      sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
    }) {};

  nixpkgs = import <nixpkgs> {};
in
{
  misopkgs =
    misopkgs;

  nixpkgs =
    nixpkgs;

  webclient-dev =
    with misopkgs;
    pkgs.haskell.packages.ghc865.callCabal2nix
      "otulpweb-webclient" ./otulpweb-webclient { miso = miso-jsaddle; };

  webclient =
    with misopkgs;
    pkgs.haskell.packages.ghcjs.callCabal2nix "otulpweb-webclient" ./otulpweb-webclient {};

  server =
    with nixpkgs;
    haskellPackages.callCabal2nix "otulpweb-server" ./otulpweb-server {};

}
