{ lib, config, pkgs, ... }:
let
  cfg = config.services.otulpweb;
in
{
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
    environment.systemPackages = [ pkgs.otulpweb.deployment ];
    systemd.services.otulpweb = pkgs.otulpweb.service;
  };
};
