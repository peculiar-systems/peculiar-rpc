{
  config,
  lib,
  pkgs,
  ...
}:
let
  version = "1.0.5";

  source = pkgs.fetchFromGitHub {
    owner = "connectrpc";
    repo = "conformance";
    rev = "v${version}";
    hash = "sha256-qVtr9PMsZTFHJLoPT/i0p1L/sYuI/rLPf9dwpxNlzz4=";
  };

  connectconformance = pkgs.buildGoModule {
    pname = "connectconformance";
    inherit version;
    src = source;
    vendorHash = "sha256-RbGRzjAayowIHC07UMxs4yIhfVwK0zNR6cvJo9b1wD8=";
    subPackages = [
      "cmd/connectconformance"
      "cmd/referenceserver"
      "cmd/grpcserver"
    ];
    doCheck = false;
  };
  protoset = pkgs.runCommand "conformance-protoset" { nativeBuildInputs = [ pkgs.protobuf ]; } ''
    protoc --include_imports --descriptor_set_out=$out -I ${source}/proto \
      ${source}/proto/connectrpc/conformance/v1/service.proto
  '';

  suite = lib.fileset.toSource {
    root = ../conformance;
    fileset = lib.fileset.unions [
      ../conformance/server.yaml
      ../conformance/known-failing.txt
      ../conformance/known-flaky.txt
      ../conformance/client.yaml
      ../conformance/client-known-failing.txt
      ../conformance/client-known-flaky.txt
    ];
  };

  runServerSuite = ''
    ${connectconformance}/bin/connectconformance \
      --mode server \
      --conf ${suite}/server.yaml \
      --known-failing @${suite}/known-failing.txt \
      --known-flaky @${suite}/known-flaky.txt \
      -- ${lib.getExe config.flake.packages.peculiar-rpc-conformance}
  '';
  runClientSuite = ''
    ${connectconformance}/bin/connectconformance \
      --mode client \
      --conf ${suite}/client.yaml \
      --known-failing @${suite}/client-known-failing.txt \
      --known-flaky @${suite}/client-known-flaky.txt \
      -- ${config.flake.packages.peculiar-rpc-conformance}/bin/peculiar-rpc-conformance-client
  '';
in
{
  flake.packages = {
    inherit connectconformance;
    conformance-source = source;
  };

  flake.output.checks.conformance-server = pkgs.runCommandLocal "conformance-server" { } ''
    export HOME="$TMPDIR"
    ${runServerSuite}
    touch $out
  '';

  flake.output.checks.conformance-client = pkgs.runCommandLocal "conformance-client" { } ''
    export HOME="$TMPDIR"
    ${runClientSuite}
    touch $out
  '';

  tasks.bench-compare = {
    description = "Load the conformance server of peculiar-rpc, connect-go and grpc-go with ghz and compare";
    body = ''
      set -euo pipefail
      exec ${config.flake.packages.peculiar-rpc-conformance}/bin/peculiar-rpc-compare \
        ${pkgs.ghz}/bin/ghz ${protoset} \
        peculiar-rpc=${lib.getExe config.flake.packages.peculiar-rpc-conformance} \
        connect-go=${connectconformance}/bin/referenceserver \
        grpc-go=${connectconformance}/bin/grpcserver
    '';
  };

  tasks.conformance = {
    description = "Run the Connect conformance suite against the server and the client";
    body = ''
      set -euo pipefail
      ${runServerSuite}
      ${runClientSuite}
    '';
  };
}
