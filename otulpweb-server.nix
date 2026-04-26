{ mkDerivation, aeson, base, bytestring, conduit, containers, dhall
, diagrams-lib, diagrams-svg, exceptions, generic-lens, http-media
, http-types, interpolatedstring-perl6, lens, lib, mtl
, optparse-applicative, otulpweb-common, polysemy, resourcet
, servant, servant-server, svg-builder, text, time, uuid, wai
, wai-app-static, warp
}:
mkDerivation {
  pname = "otulpweb-server";
  version = "0.1.0.3";
  src = ./otulpweb-server;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring conduit containers dhall diagrams-lib
    diagrams-svg exceptions generic-lens http-media http-types
    interpolatedstring-perl6 lens mtl optparse-applicative
    otulpweb-common polysemy resourcet servant servant-server
    svg-builder text time uuid wai wai-app-static warp
  ];
  description = "www.otulp.net server";
  license = lib.licensesSpdx."ISC";
  mainProgram = "server";
}
