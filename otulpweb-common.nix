{ mkDerivation, base, containers, lib, QuickCheck, servant
, svg-builder, tasty, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "otulpweb-common";
  version = "0.1.0.3";
  src = ./otulpweb-common;
  libraryHaskellDepends = [
    base containers QuickCheck servant svg-builder text
  ];
  testHaskellDepends = [
    base containers tasty tasty-hunit tasty-quickcheck
  ];
  description = "www.otulp.net shared code";
  license = lib.licensesSpdx."ISC";
}
