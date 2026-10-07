{
  ai-haskell-linter,
  lib,
  pkgs,
  ...
}:
let
  inherit (ai-haskell-linter.packages.${pkgs.stdenv.hostPlatform.system}) fourmolu fourmolu-config;
  inherit (pkgs) git nixfmt;
  inherit (lib.fileset) toSource unions;

  root = ../.;

  directories = [
    "core/src"
    "core/test"
    "client/src"
    "client/test"
    "rpc/src"
    "rpc/test"
    "rpc/app"
    "rpc/validate-conformance"
    "conformance/src"
    "conformance/app"
    "interop/src"
    "interop/app"
    "json/src"
    "json/conformance"
    "json/app"
    "openapi/src"
    "openapi/app"
    "openapi/test"
  ];

  formatHaskell =
    mode:
    "LANG=C.UTF-8 ${fourmolu}/bin/fourmolu --config ${fourmolu-config} --mode ${mode} ${lib.concatStringsSep " " directories}";

  formatNix = arguments: "${nixfmt}/bin/nixfmt ${arguments} flake.nix nix/*.nix nixos/*.nix";

  source = toSource {
    inherit root;
    fileset = unions (
      [
        (root + "/flake.nix")
        (root + "/nix")
        (root + "/nixos")
        (root + "/core/peculiar-rpc-core.cabal")
        (root + "/client/peculiar-rpc-client.cabal")
        (root + "/rpc/peculiar-rpc.cabal")
        (root + "/conformance/peculiar-rpc-conformance.cabal")
        (root + "/interop/peculiar-rpc-interop.cabal")
        (root + "/json/peculiar-rpc-json.cabal")
        (root + "/openapi/peculiar-rpc-openapi.cabal")
      ]
      ++ map (directory: root + "/${directory}") directories
    );
  };
in
{
  flake.output.checks.fmt-check = pkgs.runCommandLocal "fmt-check" { } ''
    cd ${source}
    ${formatHaskell "check"}
    ${formatNix "--check"}
    touch $out
  '';

  tasks.fmt = {
    description = "Format the Haskell and the Nix in place";
    body = ''
      set -euo pipefail
      cd "$(${git}/bin/git rev-parse --show-toplevel)"
      ${formatHaskell "inplace"}
      ${formatNix ""}
    '';
  };
}
