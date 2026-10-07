{
  description = "gRPC, gRPC-Web and Connect for Haskell, served from one binary";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    core-flake = {
      url = "github:purplenoodlesoop/core-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ai-haskell-linter = {
      url = "github:purplenoodlesoop/ai_haskell_linter";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      ai-haskell-linter,
      core-flake,
      ...
    }:
    core-flake.lib.evalFlake {
      specialArgs = { inherit ai-haskell-linter; };

      perSystem.imports = [
        core-flake.nixosModules.tasks
        ./nix/proto.nix
        ./nix/haskell.nix
        ./nix/conformance.nix
        ./nix/interop.nix
        ./nix/json.nix
        ./nix/apis.nix
        ./nix/validate.nix
        ./nix/openapi.nix
        ./nix/checks.nix
        ./nix/readme.nix
        ./nix/lint.nix
        ./nix/fmt.nix
        ./nix/website.nix
        ./nix/shell.nix
      ];

      topLevel.nixosModules.website =
        { lib, pkgs, ... }:
        {
          imports = [ ./nixos/website.nix ];
          services.peculiar-rpc-website.package =
            lib.mkDefault
              self.packages.${pkgs.stdenv.hostPlatform.system}.website;
        };
    };
}
