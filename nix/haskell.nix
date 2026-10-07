{
  config,
  lib,
  pkgs,
  ...
}:
let
  haskellPackages = import ./haskell-packages.nix { inherit lib pkgs; };
  inherit (lib.fileset) toSource unions;

  coreSource = toSource {
    root = ../core;
    fileset = unions [
      ../core/peculiar-rpc-core.cabal
      ../core/LICENSE
      ../core/src
      ../core/test
    ];
  };

  corePackage =
    {
      mkDerivation,
      aeson,
      base,
      base64-bytestring,
      bytestring,
      containers,
      deriving-aeson,
      hedgehog,
      http-types,
      text,
    }:
    mkDerivation {
      pname = "peculiar-rpc-core";
      doHaddock = false;
      version = "0.3.0.0";
      src = coreSource;
      libraryHaskellDepends = [
        aeson
        base
        base64-bytestring
        bytestring
        containers
        deriving-aeson
        http-types
        text
      ];
      testHaskellDepends = [
        base
        bytestring
        hedgehog
      ];
      license = lib.licenses.mit;
    };

  clientFiles = toSource {
    root = ../client;
    fileset = unions [
      ../client/peculiar-rpc-client.cabal
      ../client/LICENSE
      ../client/src
      ../client/test
    ];
  };

  clientSource = pkgs.runCommandLocal "peculiar-rpc-client-src" { } ''
    cp -r ${clientFiles} "$out"
    chmod -R u+w "$out"
    mkdir -p "$out/test-gen/Proto/Peculiar/Test/V1"
    for module in Echo Echo_Fields Mood; do
      cp ${config.flake.packages.proto-hs}/Proto/Peculiar/Test/V1/$module.hs "$out/test-gen/Proto/Peculiar/Test/V1/"
    done
  '';

  clientPackage =
    {
      mkDerivation,
      hedgehog,
      proto-lens-runtime,
      array,
      async,
      base,
      brotli,
      bytestring,
      case-insensitive,
      containers,
      crypton-connection,
      crypton-x509-store,
      crypton-x509-system,
      http-client,
      http-client-tls,
      http-semantics,
      http-types,
      http2,
      lens-family,
      network,
      peculiar-rpc-core,
      peculiar-rpc-json,
      proto-lens,
      proto-lens-protobuf-types,
      recv,
      snappy-c,
      stm,
      template-haskell,
      text,
      time-manager,
      tls,
      zlib,
      zstd,
    }:
    mkDerivation {
      pname = "peculiar-rpc-client";
      doHaddock = false;
      version = "0.3.0.0";
      src = clientSource;
      libraryHaskellDepends = [
        array
        async
        base
        brotli
        bytestring
        case-insensitive
        containers
        crypton-connection
        crypton-x509-store
        crypton-x509-system
        http-client
        http-client-tls
        http-semantics
        http-types
        http2
        lens-family
        network
        peculiar-rpc-core
        peculiar-rpc-json
        proto-lens
        proto-lens-protobuf-types
        proto-lens-runtime
        recv
        snappy-c
        stm
        template-haskell
        text
        time-manager
        tls
        zlib
        zstd
      ];
      testHaskellDepends = [
        base
        hedgehog
        proto-lens
      ];
      license = lib.licenses.mit;
    };

  rpcFiles = toSource {
    root = ../rpc;
    fileset = unions [
      ../rpc/peculiar-rpc.cabal
      ../rpc/LICENSE
      ../rpc/src
      ../rpc/test
      ../rpc/bench
      ../rpc/app
      ../rpc/validate-conformance
    ];
  };

  rpcSource = pkgs.runCommandLocal "peculiar-rpc-src" { } ''
    cp -r ${rpcFiles} "$out"
    chmod -R u+w "$out"
    mkdir -p "$out/gen" "$out/grpc-gen" "$out/validate-gen"
    cp -r ${config.flake.packages.validate-conformance-proto-hs}/Proto "$out/validate-gen/"
    cp -r ${config.flake.packages.proto-hs}/Proto "$out/gen/"
    cp ${config.flake.packages.proto-hs}/echo.descriptors "$out/gen/"
    cp -r ${config.flake.packages.grpc-proto-hs}/Proto "$out/grpc-gen/"
  '';

  rpcPackage =
    {
      mkDerivation,
      megaparsec,
      mtl,
      parser-combinators,
      re2,
      time,
      tz,
      aeson,
      array,
      deriving-aeson,
      peculiar-rpc-apis,
      peculiar-rpc-client,
      vector,
      async,
      base,
      bytestring,
      case-insensitive,
      containers,
      crypton-x509,
      crypton-x509-store,
      crypton-x509-validation,
      directory,
      http-semantics,
      http-types,
      http2,
      network,
      peculiar-rpc-core,
      peculiar-rpc-json,
      recv,
      stm,
      template-haskell,
      text,
      time-manager,
      tls,
      wai,
      warp,
      unix,
      base64-bytestring,
      hedgehog,
      http-client,
      lens-family,
      proto-lens,
      proto-lens-runtime,
      proto-lens-protobuf-types,
      process,
      tasty-bench,
    }:
    mkDerivation {
      pname = "peculiar-rpc";
      doHaddock = false;
      version = "0.3.0.0";
      src = rpcSource;
      isLibrary = true;
      isExecutable = true;
      executableHaskellDepends = [ base ];
      libraryHaskellDepends = [
        megaparsec
        mtl
        parser-combinators
        re2
        time
        tz
        aeson
        array
        deriving-aeson
        peculiar-rpc-apis
        peculiar-rpc-client
        vector
        async
        base
        bytestring
        case-insensitive
        containers
        crypton-x509
        crypton-x509-store
        crypton-x509-validation
        directory
        http-semantics
        http-types
        http2
        lens-family
        network
        peculiar-rpc-core
        peculiar-rpc-json
        proto-lens
        proto-lens-protobuf-types
        recv
        stm
        template-haskell
        text
        time-manager
        tls
        proto-lens-runtime
        unix
        wai
        warp
      ];
      testHaskellDepends = [
        async
        base
        base64-bytestring
        bytestring
        case-insensitive
        containers
        hedgehog
        http-client
        http-types
        lens-family
        peculiar-rpc-apis
        peculiar-rpc-client
        peculiar-rpc-core
        peculiar-rpc-json
        process
        proto-lens
        proto-lens-protobuf-types
        text
      ];
      testToolDepends = [
        pkgs.curl
        pkgs.grpcurl
        pkgs.openssl
      ];
      benchmarkHaskellDepends = [
        async
        base
        bytestring
        http-client
        lens-family
        peculiar-rpc-client
        peculiar-rpc-core
        proto-lens
        tasty-bench
        text
      ];
      license = lib.licenses.mit;
    };

  conformanceFiles = toSource {
    root = ../conformance;
    fileset = unions [
      ../conformance/peculiar-rpc-conformance.cabal
      ../conformance/LICENSE
      ../conformance/src
      ../conformance/app
    ];
  };

  conformanceSource = pkgs.runCommandLocal "peculiar-rpc-conformance-src" { } ''
    cp -r ${conformanceFiles} "$out"
    chmod -R u+w "$out"
    mkdir -p "$out/gen"
    cp -r ${config.flake.packages.conformance-proto-hs}/Proto "$out/gen/"
  '';

  conformancePackage =
    {
      mkDerivation,
      aeson,
      async,
      base,
      base64-bytestring,
      bytestring,
      containers,
      http-client,
      lens-family,
      peculiar-rpc,
      peculiar-rpc-client,
      stm,
      peculiar-rpc-core,
      peculiar-rpc-json,
      process,
      proto-lens,
      proto-lens-protobuf-types,
      proto-lens-runtime,
      text,
    }:
    mkDerivation {
      pname = "peculiar-rpc-conformance";
      doHaddock = false;
      version = "0.1.0.0";
      src = conformanceSource;
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [
        aeson
        async
        base
        base64-bytestring
        bytestring
        containers
        http-client
        lens-family
        peculiar-rpc
        peculiar-rpc-client
        stm
        peculiar-rpc-core
        peculiar-rpc-json
        process
        proto-lens
        proto-lens-protobuf-types
        proto-lens-runtime
        text
      ];
      executableHaskellDepends = [ base ];
      license = lib.licenses.mit;
      mainProgram = "peculiar-rpc-conformance-server";
    };

  inherit (pkgs.haskell.lib.compose)
    appendConfigureFlag
    doBenchmark
    overrideCabal
    ;

  core = haskellPackages.callPackage corePackage { };

  servedWith =
    set:
    let
      client = set.callPackage clientPackage {
        peculiar-rpc-core = core;
        peculiar-rpc-json = config.flake.packages.peculiar-rpc-json;
      };
    in
    {
      inherit client;
      rpc = set.callPackage rpcPackage {
        peculiar-rpc-client = client;
        peculiar-rpc-core = core;
        peculiar-rpc-json = config.flake.packages.peculiar-rpc-json;
        peculiar-rpc-apis = config.flake.packages.peculiar-rpc-apis;
      };
    };

  inherit (servedWith haskellPackages) client rpc;

  unfixedHttp2 = haskellPackages.extend (
    _: super: { http2 = overrideCabal (_: { patches = [ ]; }) super.http2; }
  );

  windowTest = "dist/build/peculiar-rpc-window-test/peculiar-rpc-window-test";

  stallsOnUnfixedHttp2 = overrideCabal (_: {
    pname = "peculiar-rpc-window-unfixed";
    doCheck = true;
    checkPhase = ''
      stalled=0
      for run in $(seq 10); do
        code=0
        ${windowTest} > window.log 2>&1 || code=$?
        if [ "$code" -eq 0 ]; then
          cat window.log
          echo "the window test passed against unfixed http2 on run $run" >&2
          exit 1
        elif [ "$code" -gt 128 ]; then
          echo "run $run: the test died of signal $((code - 128))"
        elif grep -q "Stalled 65535" window.log; then
          stalled=$((stalled + 1))
          echo "run $run: a stream stalled at 65535 bytes"
        else
          cat window.log
          echo "the window test failed on run $run without a stream stalled at 65535 bytes" >&2
          exit 1
        fi
      done
      if [ "$stalled" -eq 0 ]; then
        echo "no run showed a stream stalled at 65535 bytes" >&2
        exit 1
      fi
    '';
  }) (servedWith unfixedHttp2).rpc;

  bench = lib.pipe rpc [
    doBenchmark
    (appendConfigureFlag "--ghc-option=-Werror")
    (overrideCabal (drv: {
      pname = "peculiar-rpc-bench";
      postInstall = (drv.postInstall or "") + ''
        install -Dm755 dist/build/peculiar-rpc-bench/peculiar-rpc-bench "$out/bin/peculiar-rpc-bench"
      '';
      mainProgram = "peculiar-rpc-bench";
    }))
  ];

  conformance = haskellPackages.callPackage conformancePackage {
    peculiar-rpc-client = client;
    peculiar-rpc-core = core;
    peculiar-rpc = rpc;
    peculiar-rpc-json = config.flake.packages.peculiar-rpc-json;
  };
in
{
  flake.packages = {
    default = rpc;
    peculiar-rpc-core = core;
    peculiar-rpc-client = client;
    peculiar-rpc = rpc;
    peculiar-rpc-conformance = conformance;
    peculiar-rpc-bench = bench;
  };

  flake.output.checks = {
    peculiar-rpc-bench = bench;
    peculiar-rpc-window-unfixed = stallsOnUnfixedHttp2;
  };

  tasks.bench = {
    description = "Benchmark the wire formats, the codecs and round trips over every transport";
    body = ''
      set -euo pipefail
      exec ${bench}/bin/peculiar-rpc-bench "$@"
    '';
  };
}
