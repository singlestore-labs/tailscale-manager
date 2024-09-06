# Tailscale routes manager

**tailscale-manager** is an app that can emulate [Tailscale App Connectors](https://tailscale.com/kb/1281/app-connectors) by dynamically resolving a set of hostnames in DNS and keeping Tailscale `--advertise-routes` in sync with the resulting IP addresses.  This is most useful when using [Headscale](https://headscale.net/), which doesn't normally support App Connectors.  It runs alongside tailscaled on the node(s) where you want to advertise hostname-based routes.

## Example

Here is a sample config file, in JSON format:

``` json
{
  "routes": [
    "172.16.0.0/22",
    "192.168.0.0/24",
  ],
  "hostRoutes": [
    "github.com",
    "private-app.example.com"
  ]
}
```

Run tailscale-manager:

```sh
tailscale-manager your-config-file.json --interval 300
```

The above will result in individual /32 (or /128 for ipv6) static route advertisements on your tailnet for the IP addresses that `github.com` and `private-app.example.com` resolve to, along with any static routes provided in the `routes` list.  Every 300 seconds, tailscale-manager will refresh its DNS resolution and update tailscale route advertisements accordingly.

## Commandline options

```
Usage: tailscale-manager <configfile.json> [--dryrun] [--tailscale PATH]
                         [--interval INT] [--max-shrink-ratio RATIO]

  Tailscale routes manager

  Dynamically resolves a list of hostRoutes to IP addresses, then tells tailscale
  to advertise them as /32 routes along with any normal CIDR routes.

  Config file example:

  {
    "routes": [
      "172.16.0.0/22",
      "192.168.0.0/24"
    ],
    "hostRoutes": [
      "special-hostname1.example",
      "special-hostname2.example",
    ],
    "extraArgs": ["--webclient"]
  }

Available options:
  --dryrun                 Dryrun mode
  --tailscale PATH         Path to the tailscale executable
                           (default: "tailscale")
  --interval INT           Interval (in seconds) between runs. 0 means exit
                           after running once. (default: 0)
  --max-shrink-ratio RATIO Max allowed route shrinkage between consecutive runs,
                           as a ratio between 0 and 1. 1 means no limit.
                           (default: 0.33)
  -h,--help                Show this help text
```

## NixOS module

If you use NixOS, this repository provides a flake with a NixOS module to install and run tailscale-manager as a systemd service.  You can incorporate it into your flake.nix like so:

``` nix
{
  description = "my nixos config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    tailscale-manager = {
      url = "git+ssh://git@gitlab.com/singlestore/infra/tailscale-manager.git";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, tailscale-manager }:
  {
    nixosConfigurations.default = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        tailscale-manager.nixosModules.x86_64-linux.default
        ({ config, lib, pkgs, ... }:
         {
           services.tailscale.enable = true;

           services.tailscale-manager = {
             enable = true;
             routes = [
               "172.16.0.0/22"
               "192.168.0.0/24"
             ];
             hostRoutes = [
               "app1.example.com"
               "app2.example.com"
             ];
             interval = 300;
             maxShrinkRatio = 0.25;
           };
         })
      ];
    };
  };
}
```
