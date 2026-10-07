---
title: Module reference
section: Internals
order: 11
---

# Module reference

Import `Peculiar.Rpc` qualified as `Rpc` and you rarely need anything else. The modules behind it are listed here for the rest.

## peculiar-rpc-client

The client without the server. `Peculiar.Rpc` re-exports all of it.

| Module                       | Holds                                                                               |
| ---------------------------- | ----------------------------------------------------------------------------------- |
| `Peculiar.Rpc.Client`        | Everything a caller needs, to import qualified as `Rpc`.                            |
| `Peculiar.Rpc.Calling`       | `Calling`, `Options`, `Transport` and `ClientInterceptor`.                          |
| `Peculiar.Rpc.Derive.Client` | `deriveClient`.                                                                     |
| `Peculiar.Rpc.Remote`        | The `Client` class a derived client implements.                                     |
| `Peculiar.Rpc.Client.Http2`  | The HTTP/2 connection, h2c or TLS, reopened when the server closes it.              |
| `Peculiar.Rpc.Client.Http1`  | The HTTP/1.1 transport over http-client: `Target`, `web`.                           |
| `Peculiar.Rpc.Client.Call`   | The three protocols on the client, over either transport.                           |
| `Peculiar.Rpc.Tls.Client`    | Client TLS: `ClientTls`, `Trust`, `systemTls`, `webManager`.                        |
| `Peculiar.Rpc.Error`         | `RpcError`, `throwRpc`, `withMetadata`, and re-exported `Code`, `Status`, `Detail`. |
| `Peculiar.Rpc.Metadata`      | `Metadata`, `header`, `headers`, `lookupHeader`, `lookupHeaders`.                   |
| `Peculiar.Rpc.Compression`   | `Compression`: gzip, deflate, snappy, zstd and brotli, both ways.                   |
| `Peculiar.Rpc.Retry`         | `Retry`, `defaultRetry`, `pushback` and `noRetry`.                                  |
| `Peculiar.Rpc.Rpc`           | The `Method` constraint, `methodPath` and `idempotent`.                             |

## peculiar-rpc

| Module                    | Holds                                                                                     |
| ------------------------- | ----------------------------------------------------------------------------------------- |
| `Peculiar.Rpc`            | The whole public API, re-exported, to import qualified.                                   |
| `Peculiar.Rpc.Derive`     | `deriveService`, `deriveServer`, `deriveClient`.                                          |
| `Peculiar.Rpc.Service`    | `Implementation`, `Client`, `Endpoint`, `endpoint`, `BrowserReachable`.                   |
| `Peculiar.Rpc.Handler`    | The `Handler` shapes and `Served`, with the calling types re-exported.                    |
| `Peculiar.Rpc.Context`    | The `Context` handlers receive.                                                           |
| `Peculiar.Rpc.Intercept`  | `Route` and the server's `Interceptor`.                                                   |
| `Peculiar.Rpc.Mount`      | The service as a WAI value: `Calls`, `Server`, `withServer`, `application`, `middleware`. |
| `Peculiar.Rpc.Serve`      | A server for any WAI application: `Settings`, `serve`, and `serveEndpoints`.              |
| `Peculiar.Rpc.Listen`     | The listener: plaintext sniffed, TLS by ALPN.                                             |
| `Peculiar.Rpc.Drain`      | Graceful shutdown, and `terminated`.                                                      |
| `Peculiar.Rpc.Web`        | The gRPC-Web and Connect translation.                                                     |
| `Peculiar.Rpc.Wai`        | The bridge from the service to WAI, with native gRPC's trailers.                          |
| `Peculiar.Rpc.Tls`        | Server TLS: `Tls`, `tlsFromFiles`, `tlsFromPem`, `requireClients`.                        |
| `Peculiar.Rpc.Health`     | `grpc.health.v1`: `Statuses`, `setServing`, `health`.                                     |
| `Peculiar.Rpc.Reflection` | `withReflection`, `withReflectionFrom`.                                                   |
| `Peculiar.Rpc.Native`     | The native gRPC server: routing, streaming handlers, response compression, trailers.      |
| `Peculiar.Rpc.Rest`       | REST bindings from `google.api.http`, and turning a request into the input message.       |
| `Peculiar.Rpc.OpenApi`    | The OpenAPI 3.1 document, and `Info`.                                                     |
| `Peculiar.Rpc.Validate`   | Validators built from protovalidate rules, and `Violation`.                               |
| `Peculiar.Rpc.Violations` | Violations as `INVALID_ARGUMENT` with a `google.rpc.BadRequest`.                          |
| `Peculiar.Rpc.Catalog`    | Messages, enums and services from the embedded file descriptors, with comments.           |

## peculiar-rpc-core

The wire formats as pure values. No IO, and property-tested with hedgehog. The server translation and the web client are both built on it.

| Module                                  | Holds                                                                         |
| --------------------------------------- | ----------------------------------------------------------------------------- |
| `Peculiar.Rpc.Core.Frame`               | Length-prefixed frames, decoded incrementally under a limit.                  |
| `Peculiar.Rpc.Core.Code`                | Status codes, their Connect names and HTTP mappings.                          |
| `Peculiar.Rpc.Core.Status`              | `google.rpc.Status` in its protobuf wire form, and percent-encoding.          |
| `Peculiar.Rpc.Core.Timeout`             | `Timeout`, `grpc-timeout` and `connect-timeout-ms`.                           |
| `Peculiar.Rpc.Core.Wire`                | Classifying a request by protocol, and planning its native gRPC form.         |
| `Peculiar.Rpc.Core.Response`            | Turning native gRPC responses back into gRPC-Web and Connect.                 |
| `Peculiar.Rpc.Core.Client`              | Client request headers for each protocol, and reading every kind of response. |
| `Peculiar.Rpc.Core.Connect`             | Connect error and end-stream JSON.                                            |
| `Peculiar.Rpc.Core.Cors`                | CORS policy and its headers.                                                  |
| `Peculiar.Rpc.Core.Base64`              | Unpadded, URL-safe and streaming `grpc-web-text` base64.                      |
| `Peculiar.Rpc.Core.Headers`, `Trailers` | Status headers and gRPC-Web trailer blocks.                                   |

## peculiar-rpc-json

The proto3 JSON mapping, for any proto-lens message. It depends on nothing else in this repository.

| Function                   | Does                                                                    |
| -------------------------- | ----------------------------------------------------------------------- |
| `toJson`, `fromJson`       | A message to and from an aeson `Value`.                                 |
| `encodeJson`, `decodeJson` | A message to and from bytes, rejecting duplicate keys and deep nesting. |
| `register`, `wellKnown`    | The `Registry` of types an `Any` may hold.                              |

## peculiar-rpc-apis

Generated proto-lens modules for `google.api` (`http`, `annotations`, `field_behavior`), `google.rpc` (`code`, `status`, `error_details`) and `buf.validate`, from pinned googleapis and protovalidate sources. `packages.api-protos` in the flake holds the `.proto` files, to put on `protoc`'s include path.

## peculiar-rpc-openapi

| Module                         | Holds                                                                         |
| ------------------------------ | ----------------------------------------------------------------------------- |
| `Peculiar.Rpc.OpenApi.Command` | The `peculiar-rpc-openapi` command: a descriptor set to its OpenAPI document. |
