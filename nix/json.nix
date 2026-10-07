{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib.fileset) toSource unions;

  conformance-runner = pkgs.protobuf.overrideAttrs (old: {
    pname = "protobuf-conformance-runner";
    buildInputs = (old.buildInputs or [ ]) ++ [ pkgs.jsoncpp ];
    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ pkgs.patchelf ];
    cmakeFlags = (old.cmakeFlags or [ ]) ++ [
      "-Dprotobuf_BUILD_CONFORMANCE=ON"
      "-Dprotobuf_BUILD_TESTS=OFF"
    ];
    doCheck = false;
    postInstall =
      (old.postInstall or "")
      + ''
        install -Dm755 conformance_test_runner $out/bin/conformance_test_runner
      ''
      + (
        if pkgs.stdenv.hostPlatform.isDarwin then
          ""
        else
          ''
            kept=$(patchelf --print-rpath $out/bin/conformance_test_runner | tr ':' '\n' | grep -v "^$NIX_BUILD_TOP" | paste -sd: -)
            patchelf --set-rpath "$out/lib''${kept:+:$kept}" $out/bin/conformance_test_runner
          ''
      );
    postFixup =
      (old.postFixup or "")
      + (
        if pkgs.stdenv.hostPlatform.isDarwin then
          "install_name_tool -add_rpath $out/lib $out/bin/conformance_test_runner"
        else
          ""
      );
  });

  haskellPackages = import ./haskell-packages.nix { inherit lib pkgs; };
  hp = pkgs.haskellPackages;

  conformance-proto-hs =
    pkgs.runCommand "peculiar-rpc-json-conformance-proto-hs"
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
          -I ${pkgs.protobuf.src} \
          -I ${pkgs.protobuf.src}/src \
          conformance/conformance.proto \
          google/protobuf/test_messages_proto3.proto \
          google/protobuf/test_messages_proto2.proto
      '';

  jsonFiles = toSource {
    root = ../json;
    fileset = unions [
      ../json/peculiar-rpc-json.cabal
      ../json/LICENSE
      ../json/src
      ../json/conformance
      ../json/app
    ];
  };

  jsonSource = pkgs.runCommandLocal "peculiar-rpc-json-src" { } ''
    cp -r ${jsonFiles} "$out"
    chmod -R u+w "$out"
    mkdir -p "$out/gen"
    cp -r ${conformance-proto-hs}/Proto "$out/gen/"
  '';

  jsonPackage =
    {
      mkDerivation,
      aeson,
      base,
      base64-bytestring,
      bytestring,
      containers,
      lens-family,
      proto-lens,
      proto-lens-protobuf-types,
      proto-lens-runtime,
      scientific,
      text,
      time,
      vector,
    }:
    mkDerivation {
      pname = "peculiar-rpc-json";
      version = "0.3.0.0";
      src = jsonSource;
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [
        aeson
        base
        base64-bytestring
        bytestring
        containers
        lens-family
        proto-lens
        proto-lens-protobuf-types
        proto-lens-runtime
        scientific
        text
        time
        vector
      ];
      executableHaskellDepends = [ base ];
      license = lib.licenses.mit;
      mainProgram = "peculiar-rpc-json-conformance";
    };

  json = lib.pipe jsonPackage [
    (package: haskellPackages.callPackage package { })
    pkgs.haskell.lib.compose.dontHaddock
    (pkgs.haskell.lib.compose.appendConfigureFlag "--ghc-option=-Werror")
  ];

  failing = ../json/conformance/failing.txt;

  runSuite = ''
    ${conformance-runner}/bin/conformance_test_runner \
      --enforce_recommended \
      --output_dir "$PWD" \
      --failure_list ${failing} \
      ${lib.getExe json}
  '';
in
{
  flake.packages = {
    protobuf-conformance-runner = conformance-runner;
    peculiar-rpc-json = json;
  };

  flake.output.checks.json-conformance = pkgs.runCommandLocal "json-conformance" { } ''
    set -euo pipefail
    cd "$TMPDIR"
    ${runSuite}
    touch $out
  '';

  tasks.json-conformance = {
    description = "Run protobuf's conformance suite against the proto3 JSON mapping";
    body = ''
      set -euo pipefail
      cd "$(mktemp -d)"
      ${runSuite}
    '';
  };
}
