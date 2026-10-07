{ lib, pkgs }:
let
  hackage =
    name: version: hash:
    pkgs.fetchurl {
      url = "https://hackage.haskell.org/package/${name}-${version}/${name}-${version}.tar.gz";
      inherit hash;
    };

  http2 =
    {
      mkDerivation,
      array,
      async,
      base,
      bytestring,
      case-insensitive,
      containers,
      http-semantics,
      http-types,
      iproute,
      network,
      network-byte-order,
      network-control,
      stm,
      time-manager,
      unix-time,
      utf8-string,
    }:
    mkDerivation {
      pname = "http2";
      version = "5.4.6";
      doHaddock = false;
      src = hackage "http2" "5.4.6" "sha256-JCsoEdN2/tYm9l6tvb9tT+CZCIJJMM2+UOCJ9c/lnHk=";
      patches = [ ./http2-settings-window.patch ];
      libraryHaskellDepends = [
        array
        async
        base
        bytestring
        case-insensitive
        containers
        http-semantics
        http-types
        iproute
        network
        network-byte-order
        network-control
        stm
        time-manager
        unix-time
        utf8-string
      ];
      doCheck = false;
      license = lib.licenses.bsd3;
    };

  http-semantics =
    {
      mkDerivation,
      array,
      base,
      bytestring,
      case-insensitive,
      http-types,
      network,
      network-byte-order,
      time-manager,
      utf8-string,
    }:
    mkDerivation {
      pname = "http-semantics";
      version = "0.4.1";
      doHaddock = false;
      src = hackage "http-semantics" "0.4.1" "sha256-oC3xHfjV6rrQKM5MLR76iJrmjlVtupSw0Tp1AXl0U9s=";
      libraryHaskellDepends = [
        array
        base
        bytestring
        case-insensitive
        http-types
        network
        network-byte-order
        time-manager
        utf8-string
      ];
      license = lib.licenses.bsd3;
    };

  wai =
    {
      mkDerivation,
      base,
      bytestring,
      http-types,
      network,
      text,
      vault,
    }:
    mkDerivation {
      pname = "wai";
      version = "3.2.5";
      doHaddock = false;
      src = hackage "wai" "3.2.5" "sha256-et0Z9TcaGFD414XbJAHtsUTfsnVaexrE0jN+vHce1Go=";
      libraryHaskellDepends = [
        base
        bytestring
        http-types
        network
        text
        vault
      ];
      doCheck = false;
      license = lib.licenses.mit;
    };

  warp =
    {
      mkDerivation,
      async,
      auto-update,
      base,
      bsb-http-chunked,
      bytestring,
      case-insensitive,
      containers,
      crypton-x509,
      hashable,
      http-date,
      http-semantics,
      http-types,
      http2,
      iproute,
      network,
      recv,
      simple-sendfile,
      stm,
      streaming-commons,
      text,
      time-manager,
      unix,
      vault,
      wai,
      word8,
    }:
    mkDerivation {
      pname = "warp";
      version = "3.4.15";
      doHaddock = false;
      src = hackage "warp" "3.4.15" "sha256-g1QLj8UCjzIfwrsY006PlGZcd5GJmIGHlXAVpH6cLUE=";
      libraryHaskellDepends = [
        async
        auto-update
        base
        bsb-http-chunked
        bytestring
        case-insensitive
        containers
        crypton-x509
        hashable
        http-date
        http-semantics
        http-types
        http2
        iproute
        network
        recv
        simple-sendfile
        stm
        streaming-commons
        text
        time-manager
        unix
        vault
        wai
        word8
      ];
      doCheck = false;
      license = lib.licenses.mit;
    };

  network-run =
    {
      mkDerivation,
      base,
      bytestring,
      network,
      time-manager,
    }:
    mkDerivation {
      pname = "network-run";
      version = "0.5.0";
      doHaddock = false;
      src = hackage "network-run" "0.5.0" "sha256-3lxkPpUFHn7UyFjPcTS1LLr+W9p+WqORiT5Rx8LL3Hg=";
      libraryHaskellDepends = [
        base
        bytestring
        network
        time-manager
      ];
      license = lib.licenses.bsd3;
    };
in
pkgs.haskellPackages.override {
  overrides = self: super: {
    time-manager = pkgs.haskell.lib.compose.dontHaddock (
      pkgs.haskell.lib.compose.dontCheck super.time-manager_0_3_2
    );
    http-semantics = self.callPackage http-semantics { };
    http2 = self.callPackage http2 { };
    wai = self.callPackage wai { };
    warp = self.callPackage warp { };
    network-run = self.callPackage network-run { };
    re2 = pkgs.haskell.lib.compose.overrideCabal (drv: {
      broken = false;
      doHaddock = false;
      postPatch = (drv.postPatch or "") + ''
        sed -i "1i #include <cstring>" cbits/haskell-re2.cc
      '';
    }) super.re2;
  };
}
