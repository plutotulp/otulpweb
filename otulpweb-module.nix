{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.services.otulpweb;
in
{
  imports = [ ];

  options.services.otulpweb = {
    enable = lib.mkOption {
      default = false;
      example = true;
      description = ''
        Run otulpweb service. Default configuration listens on port 8080 and
        uses all available cores.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.otulpweb.otulpweb-deployment ];
    systemd.services.otulpweb = pkgs.otulpweb.otulpweb-service;
  };
}
