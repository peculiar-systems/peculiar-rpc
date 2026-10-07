{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib.fileset) toSource unions;

  grpcGoVersion = "1.84.0";

  grpc-go = pkgs.buildGoModule {
    pname = "grpc-go-interop";
    version = grpcGoVersion;
    src = pkgs.fetchFromGitHub {
      owner = "grpc";
      repo = "grpc-go";
      rev = "v${grpcGoVersion}";
      hash = "sha256-LSq++k7/K8N+ELdl4bfcksImnU7lw6EyTw/Ir69+8JU=";
    };
    vendorHash = "sha256-uCWe7Hw92NytnWPKbM5J5Qdx+y+1dty1RIWR38D/pV8=";
    subPackages = [
      "interop/client"
      "interop/server"
    ];
    doCheck = false;
    postInstall = ''
      mv $out/bin/client $out/bin/interop-client
      mv $out/bin/server $out/bin/interop-server
    '';
  };

  interopFiles = toSource {
    root = ../interop;
    fileset = unions [
      ../interop/peculiar-rpc-interop.cabal
      ../interop/LICENSE
      ../interop/src
      ../interop/app
    ];
  };

  interopSource = pkgs.runCommandLocal "peculiar-rpc-interop-src" { } ''
    cp -r ${interopFiles} "$out"
    chmod -R u+w "$out"
    mkdir -p "$out/gen"
    cp -r ${config.flake.packages.interop-proto-hs}/Proto "$out/gen/"
  '';

  interopPackage =
    {
      mkDerivation,
      aeson,
      async,
      base,
      base64-bytestring,
      bytestring,
      directory,
      filepath,
      lens-family,
      network,
      peculiar-rpc,
      peculiar-rpc-core,
      process,
      proto-lens,
      proto-lens-runtime,
      text,
    }:
    mkDerivation {
      pname = "peculiar-rpc-interop";
      version = "0.1.0.0";
      src = interopSource;
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [
        aeson
        async
        base
        base64-bytestring
        bytestring
        directory
        filepath
        lens-family
        network
        peculiar-rpc
        peculiar-rpc-core
        process
        proto-lens
        proto-lens-runtime
        text
      ];
      executableHaskellDepends = [
        base
        bytestring
        peculiar-rpc
        text
      ];
      license = lib.licenses.mit;
      mainProgram = "peculiar-rpc-interop-server";
    };

  interop = lib.pipe interopPackage [
    (
      package:
      (import ./haskell-packages.nix { inherit lib pkgs; }).callPackage package {
        peculiar-rpc = config.flake.packages.peculiar-rpc;
        peculiar-rpc-core = config.flake.packages.peculiar-rpc-core;
      }
    )
    pkgs.haskell.lib.compose.dontHaddock
    (pkgs.haskell.lib.compose.appendConfigureFlag "--ghc-option=-Werror")
  ];

  certificates = pkgs.runCommand "interop-certificates" { nativeBuildInputs = [ pkgs.openssl ]; } ''
    mkdir -p $out
    cd $out
    openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 -nodes -days 36500 \
      -subj /CN=interop-ca -keyout ca.key -out ca.pem
    openssl req -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 -nodes \
      -subj /CN=localhost -keyout server.key -out server.csr
    printf 'subjectAltName=DNS:localhost,IP:127.0.0.1\n' > san.ext
    openssl x509 -req -in server.csr -CA ca.pem -CAkey ca.key -CAcreateserial -days 36500 \
      -extfile san.ext -out server.pem
    rm ca.key server.csr san.ext ca.srl
  '';

  testingProtoset =
    pkgs.runCommand "grpc-testing-protoset" { nativeBuildInputs = [ pkgs.protobuf ]; }
      ''
        protoc --include_imports --descriptor_set_out=$out -I ${config.flake.packages.grpc-proto-source} \
          grpc/testing/test.proto
      '';

  cases = [
    "empty_unary"
    "large_unary"
    "client_streaming"
    "server_streaming"
    "ping_pong"
    "empty_stream"
    "timeout_on_sleeping_server"
    "cancel_after_begin"
    "cancel_after_first_response"
    "status_code_and_message"
    "special_status_message"
    "custom_metadata"
    "unimplemented_method"
    "unimplemented_service"
    "rpc_soak"
    "channel_soak"
  ];

  goClient = "${grpc-go}/bin/interop-client";
  tlsFlags = "--use_tls --use_test_ca --ca_file=${certificates}/ca.pem --server_host_override=localhost";

  harness = ''
    listening() {
      for _ in $(seq 200); do
        ports=$(${pkgs.lsof}/bin/lsof -a -nP -p "$1" -iTCP -sTCP:LISTEN -Fn 2>/dev/null | sed -n 's/^n.*:\([0-9]*\)$/\1/p') || true
        port=''${ports%%$'\n'*}
        if [ -n "$port" ] && ${goClient} --test_case=empty_unary --server_host=127.0.0.1 --server_port="$port" "''${@:2}" > /dev/null 2>&1; then
          echo "$port"
          return 0
        fi
        sleep 0.1
      done
      echo "the server never answered" >&2
      return 1
    }
  '';

  serverSuite = ''
    ${harness}
    everyCase() {
      for case in ${lib.concatStringsSep " " cases}; do
        ${goClient} --server_host=127.0.0.1 --test_case="$case" "$@"
        echo "PASS $case $*"
      done
    }
    ${lib.getExe interop} 0 &
    plaintext=$!
    ${lib.getExe interop} 0 ${certificates}/server.pem ${certificates}/server.key &
    secure=$!
    trap 'kill $plaintext $secure' EXIT
    plaintextPort=$(listening "$plaintext")
    securePort=$(listening "$secure" ${tlsFlags})
    everyCase --server_port="$plaintextPort"
    everyCase --server_port="$securePort" ${tlsFlags}
  '';

  clientSuite = ''
    ${harness}
    ${grpc-go}/bin/interop-server --port=0 &
    plaintext=$!
    ${grpc-go}/bin/interop-server --port=0 --use_tls \
      --tls_cert_file=${certificates}/server.pem --tls_key_file=${certificates}/server.key &
    secure=$!
    trap 'kill $plaintext $secure' EXIT
    plaintextPort=$(listening "$plaintext")
    securePort=$(listening "$secure" ${tlsFlags})
    ${interop}/bin/peculiar-rpc-interop-client localhost "$plaintextPort"
    ${interop}/bin/peculiar-rpc-interop-client localhost "$securePort" ${certificates}/ca.pem
  '';
in
{
  flake.packages = {
    grpc-go-interop = grpc-go;
    peculiar-rpc-interop = interop;
  };

  flake.output.checks.interop-server =
    pkgs.runCommandLocal "interop-server" { __darwinAllowLocalNetworking = true; }
      ''
        set -euo pipefail
        export HOME="$TMPDIR"
        ${serverSuite}
        touch $out
      '';

  flake.output.checks.interop-client =
    pkgs.runCommandLocal "interop-client" { __darwinAllowLocalNetworking = true; }
      ''
        set -euo pipefail
        export HOME="$TMPDIR"
        ${clientSuite}
        touch $out
      '';

  tasks.bench-streams = {
    description = "Load the gRPC interop servers of peculiar-rpc and grpc-go with ghz: unary, server, client and bidi streams";
    body = ''
      set -euo pipefail
      exec ${interop}/bin/peculiar-rpc-interop-compare ${pkgs.ghz}/bin/ghz ${testingProtoset} \
        "peculiar-rpc=${lib.getExe interop} %PORT%" \
        "grpc-go=${grpc-go}/bin/interop-server --port=%PORT%"
    '';
  };

  tasks.interop = {
    description = "Run the gRPC interop cases between peculiar-rpc and grpc-go, over h2c and TLS, in both directions";
    body = ''
      set -euo pipefail
      (${serverSuite})
      (${clientSuite})
    '';
  };
}
