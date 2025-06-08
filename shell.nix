{ pkgs }:
pkgs.mkShell {
  inputsFrom = [
    pkgs.otulpweb
  ];
  packages = [
    pkgs.git
    pkgs.jujutsu
    pkgs.fish
  ];
  shellHook = "exec fish";
}
