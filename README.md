# Tailscale routes manager

**tailscale-manager** dynamically manages Tailscale subnet route advertisements
based on user-configurable discovery sources.  It runs alongside tailscaled on
the node(s) where you want to advertise routes.

## Supported discovery methods

| config keyword          | example                       | description                |
|:------------------------|:------------------------------|:---------------------------|
| `routes`                | `["192.168.0.0/24"]`          | Static routes              |
| `hostRoutes`            | `["private-app.example.com"]` | DNS hostname lookup        |
| `awsManagedPrefixLists` | `["pl-02761f4a40454a3c9"]`    | [AWS Managed Prefix Lists] |

[AWS Managed Prefix Lists]: https://docs.aws.amazon.com/vpc/latest/userguide/managed-prefix-lists.html

`hostRoutes` can be used to emulate [Tailscale App Connectors] by advertising a
set of individual IP address routes that are kept in sync with DNS lookups of a
set of hostnames.  This is most useful when using [Headscale], which doesn't
normally support App Connectors.

[Tailscale App Connectors]: https://tailscale.com/kb/1281/app-connectors
[Headscale]: https://headscale.net/

### Possible future discovery methods

- DNS SRV records
- Extra JSON files on disk
- Generic HTTP service discovery, similar to [Prometheus `http_sd`](https://prometheus.io/docs/prometheus/2.54/http_sd/)
- [NetBox lists](https://github.com/devon-mar/netbox-lists)
- Google Cloud [public-advertised-prefixes](https://cloud.google.com/sdk/gcloud/reference/compute/public-advertised-prefixes)
- Other cloud providers?

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
  ],
  "awsManagedPrefixLists": [
    "pl-02761f4a40454a3c9"
  ]
}
```

Run tailscale-manager:

```sh
tailscale-manager your-config-file.json --interval 300
```

The above will result in individual /32 (or /128 for ipv6) route advertisements
on your tailnet for the IP addresses that `github.com` and
`private-app.example.com` resolve to, plus any routes found in the named AWS
managed prefix lists, and any static routes provided in the `routes` list.
Every 300 seconds, tailscale-manager will refresh its route discovery sources
and update tailscale route advertisements accordingly.

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
    "awsManagedPrefixLists": [
      "pl-02761f4a40454a3c9"
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
      url = "github:singlestore-labs/tailscale-manager";
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
