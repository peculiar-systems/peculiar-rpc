{
  config,
  lib,
  pkgs,
  ...
}:
let
  hp = pkgs.haskellPackages;
  inherit (lib.fileset) toSource unions;

  protoSource = toSource {
    root = ../proto;
    fileset = ../proto;
  };

  protoFiles = lib.concatStringsSep " " [
    "peculiar/test/v1/echo.proto"
    "peculiar/test/v1/mood.proto"
  ];

  proto-hs =
    pkgs.runCommand "peculiar-rpc-proto-hs"
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
          --include_imports \
          --descriptor_set_out="$out/echo.descriptors" \
          -I ${protoSource} \
          ${protoFiles}
        protoc \
          --plugin=protoc-gen-haskell=${hp.proto-lens-protoc}/bin/proto-lens-protoc \
          --haskell_out="$out" \
          -I ${testProtoSource} \
          -I ${config.flake.packages.api-protos} \
          peculiar/test/v1/library.proto
      '';

  testProtoSource = toSource {
    root = ../rpc/test/proto;
    fileset = ../rpc/test/proto;
  };

  conformanceFiles = lib.concatStringsSep " " (
    map (name: "connectrpc/conformance/v1/${name}.proto") [
      "config"
      "service"
      "server_compat"
      "client_compat"
      "suite"
    ]
  );

  conformance-proto-hs =
    pkgs.runCommand "peculiar-rpc-conformance-proto-hs"
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
          -I ${config.flake.packages.conformance-source}/proto \
          -I ${pkgs.protobuf}/include \
          ${conformanceFiles}
      '';

  grpc-proto-hs =
    pkgs.runCommand "peculiar-rpc-grpc-proto-hs"
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
          -I ${../rpc/proto} \
          grpc/health/v1/health.proto \
          grpc/reflection/v1/reflection.proto \
          grpc/reflection/v1alpha/reflection.proto
      '';

  grpc-proto-source = pkgs.fetchFromGitHub {
    owner = "grpc";
    repo = "grpc-proto";
    rev = "813330824839bfdd3abc52f41807095c0de2ec19";
    hash = "sha256-wD1EaxPeXPRqwm/fYTMFs9bxlws/ZiNpyqjJEc+vp28=";
  };

  interop-proto-hs =
    pkgs.runCommand "peculiar-rpc-interop-proto-hs"
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
          -I ${grpc-proto-source} \
          grpc/testing/empty.proto \
          grpc/testing/messages.proto \
          grpc/testing/test.proto
      '';

  bufSource = toSource {
    root = ../.;
    fileset = unions [
      ../buf.yaml
      ../proto
    ];
  };
in
{
  flake.packages = {
    inherit
      grpc-proto-source
      proto-hs
      conformance-proto-hs
      grpc-proto-hs
      interop-proto-hs
      ;
  };

  flake.output.checks.proto-check =
    pkgs.runCommandLocal "proto-check" { nativeBuildInputs = [ pkgs.buf ]; }
      ''
        cd ${bufSource}
        export HOME="$TMPDIR"
        buf lint
        buf format --diff --exit-code
        touch $out
      '';

  tasks.codegen = {
    description = "Materialise the generated protobuf code into the tree";
    body = ''
      set -euo pipefail
      cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)"
      rm -rf rpc/gen rpc/grpc-gen conformance/gen interop/gen
      mkdir -p rpc/gen rpc/grpc-gen conformance/gen interop/gen
      cp -r ${config.flake.packages.proto-hs}/Proto rpc/gen/
      cp -r ${config.flake.packages.grpc-proto-hs}/Proto rpc/grpc-gen/
      cp -r ${config.flake.packages.conformance-proto-hs}/Proto conformance/gen/
      cp -r ${config.flake.packages.interop-proto-hs}/Proto interop/gen/
      chmod -R u+w rpc/gen rpc/grpc-gen conformance/gen interop/gen
    '';
  };
}
