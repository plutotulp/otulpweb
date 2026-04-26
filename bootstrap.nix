{ runCommand }:
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
  ''
