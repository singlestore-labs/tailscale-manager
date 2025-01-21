{ self, lib, pkgs, system, ... }:

let fakeTailscale = pkgs.writeScriptBin "tailscale" ''
  #!/bin/sh
  echo "Fake tailscale invoked with args: $*" 1>&2
'';
in
pkgs.nixosTest {
  name = "tailscale-manager";
  nodes.machine1 = { config, pkgs, ... }: {
    imports = [ self.nixosModules.${system}.tailscale-manager ];
    services.tailscale.package = fakeTailscale;
    services.tailscale-manager = {
      enable = true;
      routes = ["192.168.254.0/24"];
    };
    system.stateVersion = "24.11";
  };
  testScript = ''
    machine1.wait_for_unit("tailscale-manager.service")
    machine1.wait_for_console_text("Fake tailscale invoked with args:.*--advertise-routes=192.168.254.0/24")
  '';
}
