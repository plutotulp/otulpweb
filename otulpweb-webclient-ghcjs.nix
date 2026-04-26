{ mkDerivation, base, containers, generic-lens, jsaddle-dom, lens
, linear, miso, mtl, otulpweb-common, stdenv, text
}:
mkDerivation {
  pname = "otulpweb-webclient";
  version = "0.1.0.3";
  src = ./otulpweb-webclient;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers generic-lens jsaddle-dom lens linear miso mtl
    otulpweb-common text
  ];
  description = "www.otulp.net website";
  license = stdenv.lib.licenses.isc;
}
