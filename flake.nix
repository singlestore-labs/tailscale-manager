{
  description = "tailscale-manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "tailscale-manager";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

        packages.default = self.packages.${system}.${packageName};
        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
          ];
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;

        nixosModules.default = self.nixosModules.${system}.tailscale-manager;
        nixosModules.tailscale-manager = { config, lib, pkgs, ... }:
          with lib;
          let
            cfg = config.services.tailscale-manager;
            configFile = pkgs.writeTextFile {
              name = "tailscale-manager.json";
              text = generators.toJSON {} {
                routes = cfg.routes;
                hostRoutes = cfg.hostRoutes;
                advertiseExitNode = cfg.advertiseExitNode;
                extraArgs = cfg.extraArgs;
              };
            };
          in {
          options.services.tailscale-manager = {
            enable = mkEnableOption "tailscale-manager";
            package = mkPackageOption self.packages.${system} "tailscale-manager" {};
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
            advertiseExitNode = mkOption {
              type = types.bool;
              default = false;
              description = "Advertise as a tailscale exit node?";
            };
            extraArgs = mkOption {
              type = types.listOf types.str;
              default = [];
              description = "Extra arguments for `tailscale set`";
            };
          };
          config = mkIf cfg.enable {
            systemd.services.tailscale-manager = {
              after = ["tailscaled.service"];
              wants = ["tailscaled.service"];
              wantedBy = ["multi-user.target"];
              serviceConfig = {
                ExecStart = lib.escapeShellArgs [
                  "${cfg.package}/bin/tailscale-manager" configFile
                  "--tailscale=${config.services.tailscale.package}/bin/tailscale"
                  "--interval=${toString cfg.interval}"
                ];
              };
            };
          };
        };
      });
}
