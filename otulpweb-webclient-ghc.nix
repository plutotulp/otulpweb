{ mkDerivation, base, containers, generic-lens, jsaddle
, jsaddle-dom, jsaddle-warp, lens, linear, miso, mtl
, otulpweb-common, stdenv, text, warp, websockets
}:
mkDerivation {
  pname = "otulpweb-webclient";
  version = "0.1.0.3";
  src = ./otulpweb-webclient;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers generic-lens jsaddle jsaddle-dom jsaddle-warp lens
    linear miso mtl otulpweb-common text warp websockets
  ];
  description = "www.otulp.net website";
  license = stdenv.lib.licenses.isc;
}
