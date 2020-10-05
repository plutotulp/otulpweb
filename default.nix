{ # Basically GHCJS and some miso-specific stuff.
  misopkgs ? import ./nix/pinned-misopkgs.nix
, nixpkgs ? import ./nix/pinned-nixpkgs.nix
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
