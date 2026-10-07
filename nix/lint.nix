{
  ai-haskell-linter,
  lib,
  pkgs,
  ...
}:
let
  inherit (ai-haskell-linter.packages.${pkgs.stdenv.hostPlatform.system}) hlint hlint-config;
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
    "rpc/readme"
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

  local = ".hlint-local.yaml";

  source = toSource {
    inherit root;
    fileset = unions ([ (root + "/${local}") ] ++ map (directory: root + "/${directory}") directories);
  };

  language = [
    "-XGHC2021"
    "-XDerivingStrategies"
    "-XDerivingVia"
    "-XDeriveAnyClass"
    "-XGeneralizedNewtypeDeriving"
    "-XLambdaCase"
    "-XMultiWayIf"
    "-XBlockArguments"
    "-XViewPatterns"
    "-XPatternSynonyms"
    "-XQuasiQuotes"
    "-XApplicativeDo"
    "-XOverloadedStrings"
    "-XOverloadedLabels"
    "-XOverloadedRecordDot"
    "-XDuplicateRecordFields"
    "-XNoFieldSelectors"
    "-XRecordWildCards"
    "-XNamedFieldPuns"
    "-XTypeFamilies"
    "-XDataKinds"
    "-XFunctionalDependencies"
    "-XUndecidableInstances"
    "-XTemplateHaskell"
  ];

  arguments = lib.concatStringsSep " " (
    [
      "--hint=${hlint-config}"
      "--hint=${local}"
    ]
    ++ language
    ++ directories
  );

  command = "LANG=C.UTF-8 ${hlint}/bin/hlint ${arguments}";
in
{
  flake.output.checks.lint-check = pkgs.runCommandLocal "lint-check" { } ''
    cd ${source}
    ${command}
    touch $out
  '';

  tasks.lint = {
    description = "Run hlint with the shared ruleset";
    body = ''
      set -euo pipefail
      cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)"
      ${command}
    '';
  };
}
