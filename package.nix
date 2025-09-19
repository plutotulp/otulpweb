{
  # GHC from the Miso repo's haskellPackages.
  misoHaskellPackages,

  # GHCJS from the Miso repo's haskellPackages.
  misoGhcjsHaskellPackages,

  # Jsaddle from the Miso repo.
  misoJsaddle,

  # Normally GHC haskellPackages from nixpkgs.
  haskellPackages,

  runCommand,

  # Source files filter, leaving out e.g. the .git folder.
  gitignoreSource,

  closurecompiler,
}:

let
  src = gitignoreSource ./.;

  otulpweb-common-server = haskellPackages.callCabal2nix "otulpweb-common" ./otulpweb-common { };
  otulpweb-common-webclient-ghc = misoHaskellPackages.callCabal2nix "otulpweb-common" ./otulpweb-common { };
  otulpweb-common-webclient-ghcjs = misoGhcjsHaskellPackages.callCabal2nix "otulpweb-common" ./otulpweb-common { };

in
let

  otulpweb-webclient-ghc = misoHaskellPackages.callCabal2nix "otulpweb-webclient" ./otulpweb-webclient {
    miso = misoJsaddle;
    otulpweb-common = otulpweb-common-webclient-ghc;
  };

  otulpweb-webclient-ghcjs = misoGhcjsHaskellPackages.callCabal2nix "otulpweb-webclient" ./otulpweb-webclient {
    otulpweb-common = otulpweb-common-webclient-ghcjs;
  };

  otulpweb-server = haskellPackages.callCabal2nix "otulpweb-server" ./otulpweb-server {
    otulpweb-common = otulpweb-common-server;
  };

in
let

  otulpweb-webclient-closurecompiled =
    runCommand "otulpweb-webclient-closurecompiled"
      {
        client = otulpweb-webclient-ghcjs;
        inherit closurecompiler;
      }
      ''

        mkdir -p $out
        $closurecompiler/bin/closure-compiler \
          --compilation_level ADVANCED \
          --jscomp_off checkVars \
          --externs $client/bin/webclient.jsexe/all.js.externs \
          --js $client/bin/webclient.jsexe/all.js \
          --js_output_file all.js
        cp -v all.js $out/
      '';

in
let

  bootstrap =
    let
      boostrap_base_url = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist";

      bootstrap_js = builtins.fetchurl {
        url = "${boostrap_base_url}/js/bootstrap.bundle.min.js";
        sha256 = "1lsgvx7qbiccmdyy2iccsbyrkvy0j802hzzba565l97hwyihy8gm";
      };

      bootstrap_js_map = builtins.fetchurl {
        url = "${boostrap_base_url}/js/bootstrap.bundle.min.js.map";
        sha256 = "1xk8x528hmanycjhsdfhbn1wd2bcgq85cgcf1xmlhb1g6pw200p0";
      };

      bootstrap_css = builtins.fetchurl {
        url = "${boostrap_base_url}/css/bootstrap.min.css";
        sha256 = "1awhg3x1c1ccj9caf9x6v2s0khwljmqqwqscal1rza14z0f4pxv2";
      };

      bootstrap_css_map = builtins.fetchurl {
        url = "${boostrap_base_url}/css/bootstrap.min.css.map";
        sha256 = "04swi2mg6asrqqsayz66avf08m7lk05hhl87v23y2c9123wkxvxa";
      };

    in
    runCommand "bootstrap"
      {
        inherit
          bootstrap_css
          bootstrap_css_map
          bootstrap_js
          bootstrap_js_map
          ;
      }
      ''
        mkdir -p $out
        cp -v $bootstrap_js      $out/bootstrap.bundle.min.js
        cp -v $bootstrap_js_map  $out/bootstrap.bundle.min.js.map
        cp -v $bootstrap_css     $out/bootstrap.min.css
        cp -v $bootstrap_css_map $out/bootstrap.min.css.map
      '';

  otulpweb-deployment =
    runCommand "otulpweb-deployment"
      {
        server = otulpweb-server;
        client = otulpweb-webclient-closurecompiled;
        inherit src bootstrap;
      }
      ''

        mkdir -p $out/static
        cp -v $src/index.html $out/static/
        cp -v $src/favicon.ico $out/static/
        cp -v $bootstrap/bootstrap.bundle.min.js     $out/static/bootstrap.bundle.min.js
        cp -v $bootstrap/bootstrap.bundle.min.js.map $out/static/bootstrap.bundle.min.js.map
        cp -v $bootstrap/bootstrap.min.css           $out/static/bootstrap.min.css
        cp -v $bootstrap/bootstrap.min.css.map       $out/static/bootstrap.min.css.map
        cp -v $client/all.js $out/static/

        cp -v $server/bin/server $out/otulpweb-server
      '';

in
otulpweb-deployment
