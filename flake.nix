{
  description = "tailscale-manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
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
        packages.${packageName} = (
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          }).overrideAttrs (x: {
            outputs = x.outputs ++ ["testreport"];
            preCheck = ''
              checkFlagsArray+=("--test-options=--xml=$testreport/junit.xml")
            '';
          });

        packages.default = self.packages.${system}.${packageName};

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
          ];
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };

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
          };
          config = mkIf cfg.enable {
            systemd.services.tailscale-manager = {
              after = ["tailscaled.service"];
              wants = ["tailscaled.service"];
              wantedBy = ["multi-user.target"];
              serviceConfig = {
                Type = "exec";
                Restart = "on-failure";
                ExecStart = lib.escapeShellArgs (
                  [ "${cfg.package}/bin/tailscale-manager" configFile
                    "--tailscale=${config.services.tailscale.package}/bin/tailscale"
                    "--interval=${toString cfg.interval}"
                    "--max-shrink-ratio=${toString cfg.maxShrinkRatio}"
                  ] ++ lib.optional cfg.dryRun "--dryrun"
                );
              };
            };
          };
        };
      });
}
