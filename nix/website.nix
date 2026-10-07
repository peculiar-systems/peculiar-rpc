{
  lib,
  pkgs,
  ...
}:
let
  inherit (lib.fileset) toSource unions;

  root = ../website;

  src = toSource {
    inherit root;
    fileset = unions [
      (root + "/package.json")
      (root + "/package-lock.json")
      (root + "/tsconfig.json")
      (root + "/vite.config.ts")
      (root + "/index.html")
      (root + "/.oxlintrc.json")
      (root + "/.oxfmtrc.json")
      (root + "/src")
      (root + "/plugins")
      (root + "/content")
      (root + "/public")
    ];
  };

  nodejs = pkgs.nodejs_24;

  npmDeps = pkgs.fetchNpmDeps {
    inherit src;
    name = "peculiar-rpc-website-npm-deps";
    hash = "sha256-dKey0HWHZQ7x4iOHiBlg25EUG4mwaCbFwUfryMKjRiM=";
  };

  website = pkgs.buildNpmPackage {
    pname = "peculiar-rpc-website";
    version = "0.1.0";
    inherit src npmDeps nodejs;
    npmBuildScript = "build";
    dontNpmInstall = true;
    preBuild = ''
      export HOME="$TMPDIR"
      npm run check
      npm run lint
      npm run fmt:check
    '';
    installPhase = ''
      runHook preInstall
      mkdir -p "$out"
      cp -r dist/. "$out/"
      runHook postInstall
    '';
  };
in
{
  flake.packages.website = website;

  flake.output.checks.website-check = website;

  tasks.website-fmt = {
    description = "Format the website's sources and content in place";
    body = ''
      set -euo pipefail
      cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)/website"
      export HOME="''${TMPDIR:-/tmp}"
      ${nodejs}/bin/npm ci --no-fund --no-audit
      ${nodejs}/bin/npm run fmt
    '';
  };

  tasks.website-dev = {
    description = "Run the website's Vite dev server";
    body = ''
      set -euo pipefail
      cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)/website"
      export HOME="''${TMPDIR:-/tmp}"
      ${nodejs}/bin/npm ci --no-fund --no-audit
      ${nodejs}/bin/npm run dev
    '';
  };
}
