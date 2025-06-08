{ stdenv, pkgs, ...}:
stdenv.mkDerivation {
  pname = "otulpweb";
  version = "2.0.0";
  src = ./.;
  buildInputs = [
    pkgs.erlang
    pkgs.rebar3
    pkgs.gleam
  ];
}
