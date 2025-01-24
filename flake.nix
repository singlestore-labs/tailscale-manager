{
  description = "tailscale-manager";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
    nix-github-actions.url = "github:nix-community/nix-github-actions";
    nix-github-actions.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, nix-github-actions }:
    {
      githubActions = nix-github-actions.lib.mkGithubMatrix {
        checks = nixpkgs.lib.getAttrs [ "x86_64-linux" ] self.checks;
      };

      nixosModules.default = self.nixosModules.tailscale-manager;
      nixosModules.tailscale-manager = import ./nix/nixos-module.nix;

      overlays.default = final: prev: {
        tailscale-manager = self.packages.${prev.system}.tailscale-manager;
      };

    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend self.overlays.default;

        haskellPackages = pkgs.haskellPackages;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
      in {
        packages.tailscale-manager = (
          haskellPackages.callCabal2nix "tailscale-manager" self rec {
            # Dependency overrides go here
          }).overrideAttrs (x: {
            outputs = x.outputs ++ ["testreport"];
            preCheck = ''
              checkFlagsArray+=("--test-options=--xml=$testreport/junit.xml")
            '';
          });

        packages.default = self.packages.${system}.tailscale-manager;

        checks.tailscale-manager = self.packages.${system}.tailscale-manager;

        checks.vm-test = pkgs.callPackage ./nix/vm-test.nix { inherit self; };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            ghcid
            cabal-install
          ];
          inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        };

      });
}
