{ sources  ? import ./nix/sources.nix
}:

let
  miso = import sources.miso {};
  pkgs = import sources.nixpkgs {};
  gitignore = import sources."gitignore.nix" { inherit (pkgs) lib; };
  gitignoreSource = gitignore.gitignoreSource;

  src = gitignoreSource ./.;

  # Choosing haskellPackage set also means you've selected the GHC
  # version.
  cfg = {
    webclient = {
      ghc = {
        haskellPackages = miso.pkgs.haskellPackages;
      };
      ghcjs = {
        haskellPackages = miso.pkgs.haskell.packages.ghcjs;
      };
    };
    server = {
      haskellPackages = pkgs.haskellPackages;
    };
  };

  devTools = {
    inherit (pkgs) niv hlint cabal-install ghcid;
  };

  otulpweb-common =
    let
      mkDerivation = pkgs:
        pkgs.haskellPackages.callCabal2nix
          "otulpweb-common" ./otulpweb-common {};
    in {
      server          = mkDerivation cfg.server;
      webclient.ghc   = mkDerivation cfg.webclient.ghc;
      webclient.ghcjs = mkDerivation cfg.webclient.ghcjs;
    };

in let

  otulpweb-webclient =
    let
      mkDerivation = pkgs: args:
        pkgs.haskellPackages.callCabal2nix
          "otulpweb-webclient" ./otulpweb-webclient args;
    in {
      ghc = mkDerivation cfg.webclient.ghc {
        miso = miso.miso-jsaddle;
        otulpweb-common = otulpweb-common.webclient.ghc;
      };

      ghcjs = mkDerivation cfg.webclient.ghcjs {
        otulpweb-common = otulpweb-common.webclient.ghcjs;
      };

    };

  otulpweb-server =
    cfg.server.haskellPackages.callCabal2nix
      "otulpweb-server" ./otulpweb-server {
        otulpweb-common = otulpweb-common.server;
      };

in let

  otulpweb-webclient-closurecompiled =
    pkgs.runCommand "otulpweb-webclient-closurecompiled" {
      client = otulpweb-webclient.ghcjs;
      inherit (pkgs) closurecompiler;
    } ''

    mkdir -p $out
    $closurecompiler/bin/closure-compiler \
      --compilation_level ADVANCED \
      --jscomp_off checkVars \
      --externs $client/bin/webclient.jsexe/all.js.externs \
      --js $client/bin/webclient.jsexe/all.js \
      --js_output_file all.js
    cp -v all.js $out/
    '';

in let

  bootstrap =
    let
      boostrap_base_url = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist";

      bootstrap_js =
        builtins.fetchurl {
          url = "${boostrap_base_url}/js/bootstrap.bundle.min.js";
          sha256 = "1lsgvx7qbiccmdyy2iccsbyrkvy0j802hzzba565l97hwyihy8gm";
        };

      bootstrap_js_map =
        builtins.fetchurl {
          url = "${boostrap_base_url}/js/bootstrap.bundle.min.js.map";
          sha256 = "1xk8x528hmanycjhsdfhbn1wd2bcgq85cgcf1xmlhb1g6pw200p0";
        };

      bootstrap_css =
        builtins.fetchurl {
          url = "${boostrap_base_url}/css/bootstrap.min.css";
          sha256 = "1awhg3x1c1ccj9caf9x6v2s0khwljmqqwqscal1rza14z0f4pxv2";
        };

      bootstrap_css_map =
        builtins.fetchurl {
          url = "${boostrap_base_url}/css/bootstrap.min.css.map";
          sha256 = "04swi2mg6asrqqsayz66avf08m7lk05hhl87v23y2c9123wkxvxa";
        };

    in pkgs.runCommand "bootstrap" {
      inherit bootstrap_css bootstrap_css_map bootstrap_js bootstrap_js_map;
    } ''
    mkdir -p $out
    cp -v $bootstrap_js      $out/bootstrap.bundle.min.js
    cp -v $bootstrap_js_map  $out/bootstrap.bundle.min.js.map
    cp -v $bootstrap_css     $out/bootstrap.min.css
    cp -v $bootstrap_css_map $out/bootstrap.min.css.map
    '';

  otulpweb-deployment =
    pkgs.runCommand "otulpweb-deployment" {
      server = otulpweb-server;
      client = otulpweb-webclient-closurecompiled;
      inherit src bootstrap;
    } ''

    mkdir -p $out/static
    cp -v $src/index.html $out/static/
    cp -v $src/favicon.ico $out/static/
    cp -v $bootstrap/bootstrap.bundle.min.js     $out/static/bootstrap.bundle.min.js
    cp -v $bootstrap/bootstrap.bundle.min.js.map $out/static/bootstrap.bundle.min.js.map
    cp -v $bootstrap/bootstrap.min.css           $out/static/bootstrap.min.css
    cp -v $bootstrap/bootstrap.min.css.map       $out/static/bootstrap.min.css.map
    cp -v $client/all.js $out/static/

    cp -v $src/config.dhall $out/
    cp -v $server/bin/server $out/otulpweb-server
    '';

in let

  otulpweb-service = {
    description = "Otulpweb server";
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      WorkingDirectory = "${otulpweb-deployment}";
      ExecStart = "${otulpweb-deployment}/otulpweb-server --config config.dhall +RTS -N -T -S";
      Restart = "always";
    };
  };

in {

  # Package sets.
  inherit miso pkgs;

  inherit cfg src devTools;

  # This is just a directory of bootstrap files.
  inherit bootstrap;

  # Derivations for the cabal projects.
  inherit otulpweb-common otulpweb-webclient otulpweb-server;


  # For NixOS. In /etc/nixos/config.nix:
  #
  #   systemd.services.otulpweb = (import /path/to/this/dir {}).otulpweb-service;
  #
  inherit otulpweb-deployment otulpweb-service;

}
