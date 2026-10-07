{
  config,
  lib,
  pkgs,
  ...
}:
let
  haskellPackages = import ./haskell-packages.nix { inherit lib pkgs; };

  local = [
    "peculiar-rpc-core"
    "peculiar-rpc"
  ];

  dependencies =
    package:
    builtins.filter (
      input: !(builtins.elem (input.pname or "") local)
    ) package.getBuildInputs.haskellBuildInputs;

  ghc = haskellPackages.ghcWithPackages (_: dependencies config.flake.packages.peculiar-rpc-core);
in
{
  flake.shell = [
    ghc
    haskellPackages.cabal-install
    pkgs.git
  ];
}
