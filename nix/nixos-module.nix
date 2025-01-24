{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.tailscale-manager;
  configFile = pkgs.writeTextFile {
    name = "tailscale-manager.json";
    text = generators.toJSON {} {
      routes = cfg.routes;
      hostRoutes = cfg.hostRoutes;
      extraArgs = cfg.extraArgs;
      awsManagedPrefixLists = cfg.awsManagedPrefixLists;
    };
  };
in {
  options.services.tailscale-manager = {
    enable = mkEnableOption "tailscale-manager";
    package = mkPackageOption pkgs "tailscale-manager" {};
    interval = mkOption {
      type = types.int;
      default = 300;
      description = "Interval between runs, in seconds";
    };
    routes = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "List of CIDR prefix routes to advertise";
    };
    hostRoutes = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "List of hostnames and IP addresses to add as /32 routes";
    };
    awsManagedPrefixLists = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "AWS prefix list IDs for route discovery";
    };
    extraArgs = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Extra arguments for `tailscale set`";
    };
    dryRun = mkOption {
      type = types.bool;
      default = false;
      description = "Enable dry-run mode, don't actually apply changes.";
    };
    maxShrinkRatio = mkOption {
      type = types.float;
      default = 0.5;
      description = "How much route shrinkage is allowed between subsequent runs (between 0 and 1)";
    };
    socketPath = mkOption {
      type = types.path;
      default = "/var/run/tailscale/tailscaled.sock";
      description = "Path to the tailscaled socket";
    };
  };
  config = mkIf cfg.enable {
    systemd.services.tailscale-manager = {
      after = ["tailscaled.service"];
      wants = ["tailscaled.service"];
      wantedBy = ["multi-user.target"];
      # Never give up on trying to restart
      startLimitIntervalSec = 0;
      serviceConfig = {
        Type = "exec";
        Restart = "always";
        # Restart at increasing intervals to avoid things like EC2
        # metadata service rate limits
        RestartSec = 1;
        RestartSteps = 30;
        RestartMaxDelaySec = 60;
        ExecStart = lib.escapeShellArgs (
          [ "${cfg.package}/bin/tailscale-manager" configFile
            "--tailscale=${config.services.tailscale.package}/bin/tailscale"
            "--socket=${cfg.socketPath}"
            "--interval=${toString cfg.interval}"
            "--max-shrink-ratio=${toString cfg.maxShrinkRatio}"
          ] ++ lib.optional cfg.dryRun "--dryrun"
        );
      };
    };
  };
}
