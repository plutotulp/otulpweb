{ lib, config, pkgs, ... }:
let
  cfg = config.services.otulpweb;
  # TODO: Try to get pkgs.dhallPackages.lib.generators.toDhall
  # working. It wrongly converts cfg.settings.listenPort to an Integer
  # instead of Natural, meaning the generated dhall file has a + in
  # front of the port value. The config file parser in otulpweb-server
  # does not like that.
  configFile = pkgs.writeTextFile {
    name = "otulpweb-server-config.dhall";
    text = ''
      { listenPort = ${builtins.toString cfg.settings.listenPort}
      , clientFilePath = "static"
      }
    '';
  };
in
{

  options.services.otulpweb = {

    enable = lib.mkEnableOption {
      default = false;
      description = ''
        Run otulpweb service. Default configuration listens on port 8080 and
        uses all available cores.
        '';
    };

    settings.listenPort = lib.mkOption {
      default = 8080;
      type = lib.types.ints.u32;
      description = ''
        The TCP port to listen at.
      '';
    };

  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.otulpweb ];
    systemd.services.otulpweb = {
      description = "Otulpweb server";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        DynamicUser = true;
        WorkingDirectory = "${pkgs.otulpweb}";
        ExecStart = "${pkgs.otulpweb}/otulpweb-server --config ${configFile} +RTS -N";
        Restart = "always";

        CapabilityBoundingSet = ""; # Binder til høy port, så bør ikke engang trenge CAP_NET_BIND_SERVICE
        #DevicePolicy = "strict";
        DevicePolicy = "auto";
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateUsers = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;

        # - invisible (ser bare seg selv)
        # - ptraceable (ser prosesser ptrace kan monitorere)
        # - noaccess
        # ProcSubset er mer moderne alternativ til ProtectProc, men
        # invisible er bra default for service, når kombinert med
        # andre sandboxing-opsjoner.
        ProtectProc = "invisible";

        ProtectSystem = "strict"; # ser ut til å være standard
        RestrictAddressFamilies = "AF_INET AF_INET6 AF_UNIX";
        RestrictNamespaces = true; # Om problemer, slå av denne først
        RestrictRealtime = true;
        RestrictSUIDSGID = true; # ser ut til å være standard
        SystemCallArchitectures = "native";
        # SystemCallFilter = [
        #   "@basic-io @file-system @network-io"
        #   "~@privileged @mount @resources @clock @module @raw-io @debug @process"
        # ];
        UMask = "0077";
      };
    };
  };
}
