{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (pkgs.haskell.lib.compose) appendConfigureFlag doCheck;

  haskellPackages = import ./haskell-packages.nix { inherit lib pkgs; };

  tested = lib.flip lib.pipe [
    doCheck
    (appendConfigureFlag "--ghc-option=-Werror")
  ];
in
{
  flake.output.checks = {
    peculiar-rpc-core-test = tested config.flake.packages.peculiar-rpc-core;
    peculiar-rpc-test = tested config.flake.packages.peculiar-rpc;
    network-run = haskellPackages.network-run;
    peculiar-rpc-client-test = tested config.flake.packages.peculiar-rpc-client;
    client-without-server =
      pkgs.runCommandLocal "client-without-server"
        { closure = pkgs.closureInfo { rootPaths = [ config.flake.packages.peculiar-rpc-client ]; }; }
        ''
          if grep -E -- '-(warp|wai)-[0-9]' "$closure/store-paths"; then
            echo "peculiar-rpc-client pulls in the server stack" >&2
            exit 1
          fi
          touch $out
        '';
  };
}
