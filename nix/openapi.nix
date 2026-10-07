{
  config,
  lib,
  pkgs,
  ...
}:
let
  haskellPackages = import ./haskell-packages.nix { inherit lib pkgs; };
  inherit (lib.fileset) toSource unions;
  inherit (pkgs.haskell.lib.compose) appendConfigureFlag doCheck dontHaddock;

  fixtureProtos = toSource {
    root = ../openapi/test/proto;
    fileset = ../openapi/test/proto;
  };

  fixtures =
    pkgs.runCommand "peculiar-rpc-openapi-fixtures" { nativeBuildInputs = [ pkgs.protobuf ]; }
      ''
        mkdir -p "$out"
        protoc \
          --include_imports \
          --include_source_info \
          --descriptor_set_out="$out/shop.binpb" \
          -I ${fixtureProtos} \
          -I ${config.flake.packages.api-protos} \
          shop/v1/shop.proto
      '';

  openApiFiles = toSource {
    root = ../openapi;
    fileset = unions [
      ../openapi/peculiar-rpc-openapi.cabal
      ../openapi/LICENSE
      ../openapi/src
      ../openapi/app
      ../openapi/test/Main.hs
      ../openapi/test/golden
    ];
  };

  openApiSource = pkgs.runCommandLocal "peculiar-rpc-openapi-src" { } ''
    cp -r ${openApiFiles} "$out"
    chmod -R u+w "$out"
    cp -r ${fixtures} "$out/test/fixtures"
  '';

  openApiPackage =
    {
      mkDerivation,
      aeson,
      base,
      bytestring,
      containers,
      hedgehog,
      lens-family,
      peculiar-rpc,
      proto-lens,
      proto-lens-protobuf-types,
      text,
    }:
    mkDerivation {
      pname = "peculiar-rpc-openapi";
      version = "0.3.0.0";
      src = openApiSource;
      isLibrary = true;
      isExecutable = true;
      libraryHaskellDepends = [
        aeson
        base
        bytestring
        containers
        lens-family
        peculiar-rpc
        proto-lens
        proto-lens-protobuf-types
        text
      ];
      executableHaskellDepends = [
        base
        text
      ];
      testHaskellDepends = [
        aeson
        base
        bytestring
        hedgehog
      ];
      license = lib.licenses.mit;
      mainProgram = "peculiar-rpc-openapi";
    };

  openApi = dontHaddock (
    haskellPackages.callPackage openApiPackage {
      peculiar-rpc = config.flake.packages.peculiar-rpc;
    }
  );
in
{
  flake.packages = {
    peculiar-rpc-openapi = openApi;
    openapi-fixtures = fixtures;
  };

  flake.output.checks.peculiar-rpc-openapi-test = lib.pipe openApi [
    doCheck
    (appendConfigureFlag "--ghc-option=-Werror")
  ];
}
