{
  config,
  lib,
  pkgs,
  ...
}:
let
  hp = pkgs.haskellPackages;
  source = config.flake.packages.protovalidate-source;

  protovalidate-conformance = pkgs.buildGoModule {
    pname = "protovalidate-conformance";
    version = "1.2.2";
    src = "${source}/tools";
    vendorHash = "sha256-dIAAlPF8LOjr4fj0OXNyzfr2SUaTHo5BUM0HCOjYIXs=";
    subPackages = [ "protovalidate-conformance" ];
    doCheck = false;
  };

  validate-conformance-proto-hs =
    pkgs.runCommand "peculiar-rpc-validate-conformance-proto-hs"
      {
        nativeBuildInputs = [
          pkgs.protobuf
          hp.proto-lens-protoc
        ];
      }
      ''
        mkdir -p "$out"
        protoc \
          --plugin=protoc-gen-haskell=${hp.proto-lens-protoc}/bin/proto-lens-protoc \
          --haskell_out="$out" \
          -I ${source}/proto/protovalidate-testing \
          -I ${config.flake.packages.api-protos} \
          -I ${pkgs.protobuf}/include \
          buf/validate/conformance/harness/harness.proto
      '';

  knownFailing = pkgs.runCommandLocal "validate-known-failing.yaml" { } ''
    ${pkgs.gawk}/bin/awk -F '\t' \
      'NF >= 3 { cases[$1] = cases[$1] "  - \"" $2 "\"\n" } END { for (suite in cases) printf "%s:\n%s", suite, cases[suite] }' \
      ${../rpc/validate-known-failing.tsv} > $out
  '';

  runSuite = ''
    ${protovalidate-conformance}/bin/protovalidate-conformance \
      --strict_message \
      --strict_error \
      --expected_failures ${knownFailing} \
      ${config.flake.packages.peculiar-rpc}/bin/peculiar-rpc-validate-conformance
  '';
in
{
  flake.packages = {
    inherit protovalidate-conformance validate-conformance-proto-hs;
  };

  flake.output.checks.validate-conformance = pkgs.runCommandLocal "validate-conformance" { } ''
    set -euo pipefail
    ${runSuite}
    touch $out
  '';

  tasks.validate-conformance = {
    description = "Run protovalidate's conformance suite against the validator";
    body = ''
      set -euo pipefail
      ${runSuite}
    '';
  };
}
