{
  config,
  lib,
  pkgs,
  ...
}:
let
  haskellPackages = import ./haskell-packages.nix { inherit lib pkgs; };

  ghc = haskellPackages.ghcWithPackages (set: [
    config.flake.packages.peculiar-rpc
    set.http-types
    set.proto-lens-runtime
    set.wai
    set.warp
  ]);

  example = pkgs.writeText "readme-example.awk" ''
    $0 == heading { found = 1; next }
    found && /^```haskell$/ { inside = 1; next }
    inside && /^```$/ { exit }
    inside { print }
  '';
in
{
  flake.output.checks.readme-examples =
    pkgs.runCommandLocal "readme-examples"
      {
        nativeBuildInputs = [ ghc ];
        readme = ../README.md;
        echo = ../rpc/readme/Echo.hs;
        proto = config.flake.packages.proto-hs;
      }
      ''
        set -euo pipefail
        cp -r "$proto/Proto" .
        chmod -R u+w Proto
        cp "$echo" Echo.hs
        section() {
          local text
          text=$(awk -v heading="$1" -f ${example} "$readme")
          [ -n "$text" ] || { echo "README has no Haskell example under: $1" >&2; exit 1; }
          printf '%s\n' "$text"
        }

        {
          echo 'module Signatures (withServer, application, middleware) where'
          echo 'import Network.Wai qualified as Wai'
          echo 'import Peculiar.Rpc (Calls, Endpoint, Server)'
          echo 'import Peculiar.Rpc qualified as Rpc'
          section '### Mounting' | sed -E 's/^([a-z][A-Za-z]*) :: (.*)$/\1 :: \2\n\1 = Rpc.\1/'
        } > Signatures.hs

        {
          echo 'module Mounted (main) where'
          echo 'import Echo (echo)'
          echo 'import Peculiar.Rpc qualified as Rpc'
          section 'Mount it beside the rest of your application:' | sed 's/^app = \.\.\.$/app _ _ = ioError (userError "app")/'
        } > Mounted.hs

        {
          echo 'module Served (main) where'
          echo 'import Echo (app, echo)'
          echo 'import Peculiar.Rpc qualified as Rpc'
          section '### Serving'
        } > Served.hs

        ghc -XBlockArguments -Wall -Werror -no-link -outputdir build Echo.hs Signatures.hs Mounted.hs Served.hs
        touch $out
      '';
}
