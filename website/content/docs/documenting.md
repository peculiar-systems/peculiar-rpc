---
title: Documenting an API
section: Guide
order: 7
---

# Documenting an API

The `.proto` files are the one source of truth, and established tools turn them into the reference. peculiar-rpc adds one thing: the OpenAPI document without a running server.

## Descriptor sets

Every step below reads a descriptor set, the compiled form of the `.proto` files with their comments kept:

```shell
protoc --include_imports --include_source_info --descriptor_set_out=api.binpb -I proto shop/v1/shop.proto
```

`buf build --as-file-descriptor-set -o api.binpb` does the same.

## REST: OpenAPI

A server with `Calls.openApi` serves its document at `/openapi.json`. For a static site, `peculiar-rpc-openapi` prints the same document from a descriptor set:

```shell
nix run github:peculiar-systems/peculiar-rpc#peculiar-rpc-openapi -- \
  --title Shop --version 1.1.0 --description "Things for sale." api.binpb > openapi.json
```

`--service shop.v1.Shop` limits it to the services named; by default it covers every service in the set. The document carries each binding's path, parameters and body, a schema for every message and enum, comments as descriptions, `field_behavior` as `required`, `readOnly` and `writeOnly`, and validation rules as schema constraints.

Any OpenAPI viewer renders it. With [Scalar](https://github.com/scalar/scalar), one HTML file next to `openapi.json` is the whole reference:

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>Shop API</title>
  </head>
  <body>
    <script
      id="api-reference"
      data-url="openapi.json"
      data-configuration='{"showDeveloperTools":"never","agent":{"disabled":true},"mcp":{"disabled":true}}'
    ></script>
    <script
      src="https://cdn.jsdelivr.net/npm/@scalar/api-reference@1.72.1/dist/browser/standalone.js"
      integrity="sha384-U11tb2XnKvmwt8RlTvnwUnYgrN+ur4Xyh9htLhjajWNR/Oyl5AX5DEz00qRmlrmK"
      crossorigin="anonymous"
    ></script>
  </body>
</html>
```

The configuration turns off Scalar's AI agent and its MCP generation, which uploads the document to Scalar's service. Swagger UI and Redoc read the same file.

## gRPC: protoc-gen-doc

[protoc-gen-doc](https://github.com/pseudomuto/protoc-gen-doc) writes a reference of every service, method, message and enum, with their comments, as HTML or Markdown:

```shell
nix shell nixpkgs#protobuf nixpkgs#protoc-gen-doc -c \
  protoc --doc_out=site --doc_opt=html,grpc.html -I proto shop/v1/shop.proto
```

It doesn't show `buf.validate` rules or `google.api` annotations. The OpenAPI document carries both.

## Breaking changes: buf breaking

`buf breaking` compares a new descriptor set with the last released one. It fails on removed or renamed services, methods, fields and enum values, and on changed types:

```shell
buf breaking api.binpb --against released.binpb --exclude-imports
```

Run it in CI against the descriptor set of the last release. `WIRE_JSON` in `buf.yaml`'s `breaking.use` checks only what breaks binary and JSON clients, not what breaks generated source.

It doesn't see three kinds of breaking change: a new required field, a stricter validation rule, and a changed HTTP binding. Diffing `openapi.json` between releases shows most of them: required properties, the rules that have a JSON Schema form, and the paths.
