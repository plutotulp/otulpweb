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
        haskellPackages = miso.pkgs.haskell.packages.ghc865;
      };
      ghcjs = {
        haskellPackages = miso.pkgs.haskell.packages.ghcjs;
      };
    };
    server = {
      haskellPackages = pkgs.haskell.packages.ghc8107;
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

  boostrap_base_url = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0/dist";

in let

  bootstrap_js =
    builtins.fetchurl {
      url = "${boostrap_base_url}/js/bootstrap.bundle.min.js";
      sha256 = "0shl5z7kpkq35k6xfg99pf67qi5z89zqfaiqkwpxhapnl7wijp9j";
    };

  bootstrap_js_map =
    builtins.fetchurl {
      url = "${boostrap_base_url}/js/bootstrap.bundle.min.js.map";
      sha256 = "097yv76039wmyb0mnrmdfmcb2pawajyxcfgvvs2b8pj1r2pscp1m";
    };

  bootstrap_css =
    builtins.fetchurl {
      url = "${boostrap_base_url}/css/bootstrap.min.css";
      sha256 = "0r7g1wianw6l3xzqs6gh6399hz50g91p600yrlm9pqfrrp73y204";
    };

  bootstrap_css_map =
    builtins.fetchurl {
      url = "${boostrap_base_url}/css/bootstrap.min.css.map";
      sha256 = "0lmq9j0adrvxqa0x3g48rgb0l4h8wpdf7arf1aynsdx82vahpjdn";
    };

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

  otulpweb-deployment =
    pkgs.runCommand "otulpweb-deployment" {
      server = otulpweb-server;
      client = otulpweb-webclient-closurecompiled;
      inherit src bootstrap_css bootstrap_css_map bootstrap_js bootstrap_js_map;
    } ''

    mkdir -p $out/static
    cp -v $src/index.html $out/static/
    cp -v $src/favicon.ico $out/static/
    cp -v $bootstrap_js      $out/static/bootstrap.bundle.min.js
    cp -v $bootstrap_js_map  $out/static/bootstrap.bundle.min.js.map
    cp -v $bootstrap_css     $out/static/bootstrap.min.css
    cp -v $bootstrap_css_map $out/static/bootstrap.min.css.map
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

  # Derivations for the cabal projects.
  inherit otulpweb-common otulpweb-webclient otulpweb-server;


  # For NixOS. In /etc/nixos/config.nix:
  #
  #   systemd.services.otulpweb = (import /path/to/this/dir {}).otulpweb-service;
  #
  inherit otulpweb-deployment otulpweb-service;

}
