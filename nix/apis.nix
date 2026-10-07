{
  config,
  lib,
  pkgs,
  ...
}:
let
  haskellPackages = import ./haskell-packages.nix { inherit lib pkgs; };
  hp = pkgs.haskellPackages;

  googleapis = pkgs.fetchFromGitHub {
    owner = "googleapis";
    repo = "googleapis";
    rev = "5174d7c27d275560a3c963d41c3911d0a060a207";
    hash = "sha256-hwREKbZ1kiToEIF8o4MbNi3XOL6uBH/AlVp37uVyUc0=";
  };

  protovalidate = pkgs.fetchFromGitHub {
    owner = "bufbuild";
    repo = "protovalidate";
    rev = "v1.2.2";
    hash = "sha256-7B4D53DYdcoUofmDeV2RXzIIlNdXXsUi4mySy4gK+f4=";
  };

  files = [
    "google/api/http.proto"
    "google/api/annotations.proto"
    "google/api/field_behavior.proto"
    "google/rpc/code.proto"
    "google/rpc/status.proto"
    "google/rpc/error_details.proto"
    "buf/validate/validate.proto"
  ];

  api-protos = pkgs.runCommandLocal "peculiar-rpc-api-protos" { } ''
    mkdir -p $out/google/api $out/google/rpc $out/buf/validate
    for file in ${lib.concatStringsSep " " (lib.filter (lib.hasPrefix "google/") files)}; do
      cp ${googleapis}/$file $out/$file
    done
    cp ${protovalidate}/proto/protovalidate/buf/validate/validate.proto $out/buf/validate/validate.proto
  '';

  api-proto-hs =
    pkgs.runCommand "peculiar-rpc-api-proto-hs"
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
          --descriptor_set_out="$out/apis.descriptors" \
          -I ${api-protos} \
          ${lib.concatStringsSep " " files}
      '';

  apisSource = pkgs.runCommandLocal "peculiar-rpc-apis-src" { } ''
    mkdir -p "$out/gen"
    cp ${../apis/peculiar-rpc-apis.cabal} "$out/peculiar-rpc-apis.cabal"
    cp ${../apis/LICENSE} "$out/LICENSE"
    cp -r ${api-proto-hs}/Proto "$out/gen/"
  '';

  apisPackage =
    {
      mkDerivation,
      base,
      proto-lens-protobuf-types,
      proto-lens-runtime,
    }:
    mkDerivation {
      pname = "peculiar-rpc-apis";
      version = "0.3.0.0";
      src = apisSource;
      libraryHaskellDepends = [
        base
        proto-lens-protobuf-types
        proto-lens-runtime
      ];
      license = lib.licenses.mit;
    };
in
{
  flake.packages = {
    inherit api-protos;
    protovalidate-source = protovalidate;
    peculiar-rpc-apis = pkgs.haskell.lib.compose.dontHaddock (
      haskellPackages.callPackage apisPackage { }
    );
  };
}
