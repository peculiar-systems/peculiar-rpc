# peculiar-rpc

gRPC, gRPC-Web and Connect for Haskell, with JSON on all three, REST from
`google.api.http`, an OpenAPI 3.1 document and protovalidate rules, all from
the same `.proto` files.

The service is a WAI application. Mount it in your own warp, servant or scotty
server, beside your own routes, and every protocol is answered on the one port:
native gRPC, gRPC-Web, Connect, REST and JSON, over HTTP/1.1 and HTTP/2. The
library also brings a server of its own for when you have no other, but it is
a convenience over the same value, not a requirement.

Native gRPC is answered by the package itself, and a translation layer in
front of it lets browsers and Connect clients reach the same handlers without
Envoy, `grpcwebproxy` or any other sidecar. Unary calls skip even that: the
layer answers them directly in whichever protocol they arrived. On top of that
sits a typed layer derived from your `.proto` files: each service becomes a
record with one field per RPC, and the record's type decides what a handler for
each method looks like.

An agent asked to add peculiar-rpc to a product should read
[AGENT.md](AGENT.md) first.

**Passes the official suites.** The server and the client both pass the official
[Connect conformance suite](https://github.com/connectrpc/conformance) in full,
and the official [gRPC interop tests](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md)
against grpc-go, on every commit. The server passes 10204 cases across gRPC, gRPC-Web and
Connect; the protobuf and JSON codecs; HTTP/1.1, h2c and HTTP/2 over TLS; mutual
TLS; identity, gzip, deflate, snappy, zstd and brotli; every streaming kind;
Connect GET; and receive limits. The client passes 13038: all three protocols
and both codecs over HTTP/2, h2c and TLS, and Connect and gRPC-Web over
HTTP/1.1, with client certificates on every one and every compression. Both
suites run in `nix flake check`, and so does protobuf's own JSON conformance
suite against the codec.

So do the [gRPC interop cases](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md),
against grpc-go in both directions: its interop client against this server, and
this client against its interop server, over h2c and TLS. All 16 cases pass
each way.

## What a service looks like

```proto
service Echo {
  rpc Say(SayRequest) returns (SayResponse) {
    option idempotency_level = NO_SIDE_EFFECTS;
  }
  rpc Count(CountRequest) returns (stream CountResponse);
  rpc Sum(stream SumRequest) returns (SumResponse);
  rpc Chat(stream ChatRequest) returns (stream ChatResponse);
}
```

Everything a consumer needs is re-exported from `Peculiar.Rpc`, meant to be
imported qualified. So the common names stay unambiguous (`Rpc.Context`,
`Rpc.Options`, `Rpc.Code`).

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Peculiar.Rpc qualified as Rpc
import Proto.Example.V1.Echo

Rpc.deriveService ''Echo

echo :: EchoServer
echo = EchoServer{say, count, sum = total, chat}

say :: Rpc.Context -> SayRequest -> IO SayResponse
count :: Rpc.Context -> CountRequest -> (CountResponse -> IO ()) -> IO ()
total :: Rpc.Context -> IO (Maybe SumRequest) -> IO SumResponse
chat :: Rpc.Context -> IO (Maybe ChatRequest) -> (ChatResponse -> IO ()) -> IO ()
```

A missing method is a missing field. Renaming an RPC in the `.proto` is a
compile error. A handler for a streaming method cannot be given where a unary
one is expected. Handlers receive plain proto-lens messages.

Mount it beside the rest of your application:

```haskell
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp

main :: IO ()
main = Rpc.withServer Rpc.defaultCalls [Rpc.endpoint echo] \rpc ->
  Warp.run 8080 (Rpc.middleware rpc app)

app :: Wai.Application
app = ...
```

`endpoint` takes the service's routes from its method list, and its Connect GET
permissions from the `NO_SIDE_EFFECTS` option in its own descriptor. Neither
needs to be written down.

### Mounting

```haskell
withServer :: Calls -> [Endpoint] -> (Server -> IO a) -> IO a
application :: Server -> Wai.Application
middleware :: Server -> Wai.Middleware
```

A `Server` is a value, not a process: every endpoint, and the `Calls` settings
that concern calls (interceptor, error reporting, CORS, receive and header
limits, OpenAPI). `withServer` brackets it because it holds the deadline
service. Nothing listens until you run it in a server of your own.

`middleware rpc app` answers the server's own routes, the
`/package.Service/Method` paths, the REST paths its `google.api.http` options
declare, and `/openapi.json` when `Calls.openApi` is set, and hands every other
request to `app`. The RPC routes win: a catch-all route in `app` never sees a
call. A gRPC request for a method no service has goes to `app` as well, so
servers mount side by side. `application rpc` answers everything itself: a path
no service has is `UNIMPLEMENTED` in each protocol's own form, or a 404.

In servant, end the API with a `Raw` endpoint served by
`Tagged (Rpc.application rpc)`, or wrap the whole servant application in
`Rpc.middleware rpc`. Anything else that takes a WAI `Application` or
`Middleware` works the same way.

### Serving

When there is no other server to mount it in, `serveEndpoints` runs one:

```haskell
main :: IO ()
main = Rpc.serveEndpoints Rpc.defaultSettings{Rpc.port = 8080, Rpc.beside = Just app} [Rpc.endpoint echo]
```

It is `withServer` and `middleware` over `serve`, which runs any WAI
application: one listener that sniffs h2c, chooses by ALPN under TLS, serves a
Unix socket, keeps HTTP/2 connections alive with PINGs, closes idle and old
ones with GOAWAY, caps connections, and drains on shutdown. `beside` is the
application for everything that is not RPC; left `Nothing`, the server answers
alone, as `application` does. Its HTTP/2 is tuned for gRPC load, see
[Rate limits](#rate-limits).

The same splice writes a client:

```haskell
Rpc.withNativeClient (Rpc.Secure Rpc.systemTls) "api.example.org" 443 \transport -> do
  let options = Rpc.defaultOptions{Rpc.deadline = Just (Rpc.seconds 5), Rpc.requestMetadata = Rpc.header "authorization" token}
      remote = Rpc.client options transport :: EchoClient
  reply <- remote.say (defMessage & #text .~ "hello")
  remote.count (defMessage & #upto .~ 3) print
  total <- remote.sum \send -> for_ [1 .. 10] \n -> send (defMessage & #n .~ n)
  pure ()
```

`deriveServer` and `deriveClient` write one half each.

### Handlers

The `Context` carries the request's metadata, its timeout, the address of
the connection it arrived on, the method's path, and, for unary and
server-streaming methods, the request message exactly as it arrived, for
checking a signature over it. Through it a handler
can set response headers, add trailers, or flush the headers early. Read
metadata with `Rpc.lookupHeader "authorization" context.metadata`, and build it
with `Rpc.header name value`. Use `Rpc.Secure tls` in place of `Rpc.Plaintext`
for TLS.
An `RpcError` thrown by a handler reaches the client as the same typed error,
with its code, message, `google.rpc.Status` details and trailers.
`Rpc.pushback (Rpc.seconds 2)` in its metadata tells a retrying client when
to try again, and `Rpc.noRetry` tells it not to. A unary call
that carries zero messages, or more than one, is `UNIMPLEMENTED`.

A browser can only make unary and server-streaming calls. Adding
`BrowserReachable Echo` to a signature turns any other streaming kind into a
type error that names the method.

### Health

`grpc.health.v1` ships with the library:

```haskell
statuses <- newStatuses
setServing statuses "example.v1.Echo" NotServing
withServer calls [endpoint echo, health statuses] \rpc -> ...
```

The empty service name stands for the whole server and starts out
`SERVING`. `Watch` streams each change as it happens.

### Reflection

```haskell
withServer calls (withReflection [endpoint echo, health statuses]) \rpc -> ...
```

This serves `grpc.reflection.v1` and `v1alpha`, so `grpcurl list`,
`grpcurl describe` and `grpcurl -d '{...}' host Service/Method` work with no
proto files on hand. The descriptors are derived:

- `deriveServer` collects every message type defined in the proto modules the
  service's module imports, and each message carries its whole `.proto` file.
- Every file reachable from the service is served, with its source comments.
- The service's own file is served too. If proto-lens attaches it to no message,
  it is synthesised from the service descriptor.

proto-lens gives a file that declares only enums no message, so its descriptor
cannot be reached from Haskell. When a service imports one, pass protoc's own
descriptor set (`--include_imports --descriptor_set_out`) to
`withReflectionFrom`. Files in a set override derived ones.

### Clients

A program that only makes calls, an SDK for instance, depends on
`peculiar-rpc-client` instead: the client without the server, so without warp
or wai. `Peculiar.Rpc.Client` holds everything it needs under the same names
as `Peculiar.Rpc`, `deriveClient` included, so it too is imported qualified as
`Rpc`. It still uses the http2 and http-semantics releases the flake builds.

The generated client speaks all three protocols. Over HTTP/2 it uses its own
client on the `http2` package: one connection carries any number of concurrent,
full-duplex calls in any protocol, and opens again on the next call if the
server closed it.

```haskell
Rpc.withConnection (Rpc.Secure Rpc.systemTls) "api.example.org" 443 \connection -> do
  let native = Rpc.client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Grpc) :: EchoClient
      connect = Rpc.client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Connect) :: EchoClient
  ...
```

Over HTTP/1.1 it goes through `http-client`, for Connect and gRPC-Web; client
and bidi streams are then half-duplex.

```haskell
manager <- newManager defaultManagerSettings
let Right target = Rpc.web manager Rpc.Connect "https://api.example.org"
    remote = Rpc.client Rpc.defaultOptions (Rpc.Http1 target) :: EchoClient
```

`Options` carries the deadline and request metadata, `onHeaders` and
`onTrailers` callbacks for the response metadata, and:

- `compression`: gzip, deflate, snappy, zstd or brotli for requests. Every call
  accepts all five in responses.
- `retry`: `Rpc.defaultRetry` retries `UNAVAILABLE` up to four times with
  jittered exponential backoff inside the call's one deadline, and honours
  `grpc-retry-pushback-ms`. Unary calls, and server streams that have
  delivered nothing, are retried; client and bidi streams never are.
- `clientInterceptor`: wraps every call, seeing its route and options.

A `ClientTls` names what to trust, the system store or a PEM, and optionally a
client certificate and key. It works on both transports, so mutual TLS works
with every protocol.

### Interceptors and shutdown

`Calls.interceptor` wraps every call the server takes, with its route and
`Context`: it can refuse the call with an `RpcError`, change the `Context`,
time it or log it. Interceptors compose with `<>`.

`serve` and `serveEndpoints` return when `Settings.stopWhen` does, which by
default waits for SIGTERM or SIGINT. They then answer new calls `UNAVAILABLE` in
each protocol's own form, and other requests 503, close the listener, give
running requests up to `Settings.grace` to finish, and close what is left.
Mounted in a server of your own, shutdown is that server's.

## What the translation layer does

- **gRPC-Web**, binary and base64 text, with trailers in the final frame.
- **Connect unary**: the bare message; errors as JSON under the mapped HTTP
  status; trailers as `trailer-` headers. **Connect GET** for idempotent
  methods. **Connect streams**, ending in an end-stream message.
- **Compression**: gzip, deflate, snappy, zstd and brotli, decompressed in the
  layer for every protocol and passed on uncompressed.
- **Receive limits** for every protocol, native included, checked against the
  decompressed message. Messages are inflated with the output capped one byte
  past the limit, so a compression bomb costs at most the limit.
- **Deadlines** enforced from the request's arrival, not from when the handler
  starts. A stream that ends without a status gets `DEADLINE_EXCEEDED` if its
  deadline has passed, and `UNKNOWN` otherwise.
- **CORS** from a pure policy, exposing the headers gRPC-Web and Connect
  clients need.
- **One port** under `serve`: plaintext connections are sniffed (the HTTP/2
  preface goes to http2, everything else to warp); TLS connections choose by
  ALPN. Certificates can be reloaded per connection, and client certificates
  can be required and validated against a trust store.

## JSON

Every protocol takes the canonical proto3 JSON mapping as well as binary
protobuf, chosen by content type; a client picks it with
`Rpc.defaultOptions{Rpc.codec = Rpc.Json}`. The codec is its own package,
`peculiar-rpc-json`, and passes protobuf's JSON conformance cases apart from 77
that proto-lens's binary parser or its proto2 extensions fail before JSON is
involved; they are listed in `json/conformance/failing.txt`.

## REST and OpenAPI

A method annotated with `google.api.http` is served at its path as JSON too,
with nothing to register:

```proto
rpc GetBook(GetBookRequest) returns (Book) {
  option (google.api.http) = {get: "/v1/{name=shelves/*/books/*}"};
}
```

```shell
curl localhost:8080/v1/shelves/poetry/books/odes
```

Path variables, a body of `*` or one field, the query, `response_body` and
`additional_bindings` all work as Google's transcoding specifies. A failure is a
`google.rpc.Status` under the HTTP status its code maps to.

With `Calls.openApi` set, `GET /openapi.json` serves an OpenAPI 3.1 document
built from the same descriptors: operations, parameters, schemas, comments as
descriptions, `field_behavior` as `required`, `readOnly` and `writeOnly`, and
validation rules as schema constraints.

## Validation

Requests are checked against [protovalidate](https://protovalidate.com) rules in
the `.proto` file before a handler runs, on every protocol and for every message
of a stream. A violation is `INVALID_ARGUMENT` with a `google.rpc.BadRequest`
naming each field and rule. Every protovalidate rule is checked, with
protovalidate's semantics: scalar, string, bytes, enum, repeated, map, `Any`,
timestamp, duration and field mask rules, `required`, `ignore` and oneof rules,
`pattern` on strings and bytes with RE2, CEL expressions on fields and messages,
predefined rules, and `lt_now`, `gt_now` and `within`, which compare against the
server's clock when the message is checked. protovalidate's conformance suite
runs against the validator in `nix flake check`.

## Documentation

The `.proto` files document the API through established tools: the OpenAPI
document in Scalar, Swagger UI or Redoc for REST, `protoc-gen-doc` for gRPC,
and `buf breaking` in CI for breaking changes. `peculiar-rpc-openapi` prints
the OpenAPI document a server serves, from a descriptor set, so a static site
needs no server:

```shell
peculiar-rpc-openapi --title Shop --version 1.1.0 api.binpb > openapi.json
```

## Layout

| Package | What it holds |
|---|---|
| `core/` `peculiar-rpc-core` | The wire formats as values. Framing, status codes and their Connect and HTTP mappings, `google.rpc.Status`, timeouts, base64, Connect error and end-stream JSON, CORS, and the translation of each protocol to and from native gRPC. Pure, property-tested. |
| `json/` `peculiar-rpc-json` | The proto3 JSON mapping for any proto-lens message, with the well-known types, and protobuf's JSON conformance testee. |
| `openapi/` `peculiar-rpc-openapi` | The OpenAPI document from a descriptor set, as a library and a command. |
| `apis/` `peculiar-rpc-apis` | `google.api`, `google.rpc` and `buf.validate`, generated from pinned googleapis and protovalidate sources. |
| `client/` `peculiar-rpc-client` | The client alone: the three protocols over HTTP/2 and HTTP/1.1, TLS, retries, interceptors and `deriveClient`. |
| `rpc/` `peculiar-rpc` | The server, with the client re-exported. The service as a WAI application and middleware: native gRPC, the translation layer for gRPC-Web and Connect, REST transcoding, OpenAPI and validation. `serve`: the listener, TLS, keepalive and graceful shutdown. The derived typed layer. Benchmarks in `rpc/bench`. |
| `conformance/` | The conformance server and client, written against the derived layer. |
| `interop/` | The gRPC interop server and client, written against `Peculiar.Rpc` alone. |

## grapesy

Native gRPC came from [grapesy](https://github.com/well-typed/grapesy) until the
package outgrew it. First the client was replaced, to present client
certificates and read malformed responses correctly; then unary calls, which
cost about 170µs each through it; then streams, where a 1000-message server
stream took 51ms through grapesy and takes 6.4ms now. Along the way the
conformance suite found its duplicate metadata arriving reversed, an hour-long
timeout read as 24 minutes, and streams reset rather than trailed at their
deadline.

## HTTP/2

### Trailers through WAI

Native gRPC ends every response with trailers, and WAI has no field for them.
Warp does: a request's vault carries `HTTP2Data`, whose `http2dataTrailers` is
the http2 package's own trailers maker, and warp's HTTP/2 hands it to the
stream. The service sets it before it answers, so a WAI response carries its
`grpc-status` as real HTTP/2 trailers, after the last message. That is all a
native call needs, so native gRPC runs through WAI, in `serve` and in any warp
server alike: `serve` runs HTTP/2 connections through warp's own
`http2server`, the bridge warp uses itself.

WAI cannot end a stream on its HEADERS frame, which is what a gRPC
Trailers-Only response is. The service sends one as headers with the status
and content type, no messages, and every other field as trailers, which the
gRPC specification defines as the same response.

The http2, http-semantics and warp releases in nixpkgs predate fixes a server
facing the network needs, among them a 33-byte frame that crashed the process
and a reset that let a peer exceed its stream limit. The flake builds http2
5.4.6 with `nix/http2-settings-window.patch`, http-semantics 0.4.1 and warp
3.4.15. Build with them: later http-semantics and warp releases will do, but
http2 needs that patch.

Unpatched, http2 applies a peer's SETTINGS_INITIAL_WINDOW_SIZE on its sending
thread, while its receiving thread may already be opening the next stream with
the old window of 65,535 bytes. A stream opened in between keeps that window.
nginx's `grpc_pass` advertises a window of 2^31-1 and never sends a stream
WINDOW_UPDATE, so a response over 64 KiB on such a stream stalls for good. The
patch applies the setting on the receiving thread, before it reads the next
frame, so every stream starts from the latest SETTINGS received before it
opened. No http2 release carries the fix yet; 5.4.7 does not.

### Rate limits

The http2 package guards against floods with per-connection rate limits,
and its defaults are far below what a busy, well-behaved client sends:
grpc-go ends each call with an empty DATA frame and keeps a bandwidth-probing
PING in flight, so at thousands of calls a second it passes four empty frames,
ten pings and four SETTINGS a second, and the connection is closed with a
GOAWAY. `defaultHttp2`, which `serve` uses, raises those limits, and the reset
limit that guards against rapid reset, to levels that load from grpc-go does
not reach. Warp's own HTTP/2 runs with the http2 defaults and offers no way to
change them, so a service mounted in a plain warp server answers every
protocol, but a heavily loaded grpc-go client may see its connection closed
and reopened; run it under `serve` where that load is expected. The
client takes the same settings through `Dialing.http2`: grpc-go's server
probes bandwidth with PINGs while it streams a large response too.

## Development

Everything is a Nix task, and `nix flake check` is the only gate.

```
nix run .#tasks-help     every task
nix run .#codegen        materialise generated protobuf code for editors
nix run .#conformance    run the Connect conformance suite against the server and the client
nix run .#json-conformance  run protobuf's JSON conformance suite against the codec
nix run .#validate-conformance  run protovalidate's conformance suite against the validator
nix run .#interop        run the gRPC interop cases between peculiar-rpc and grpc-go, both ways
nix run .#bench          benchmark the wire formats, the codecs and round trips
nix run .#bench-compare  load peculiar-rpc, connect-go and grpc-go with ghz
nix run .#bench-streams  load peculiar-rpc and grpc-go with unary calls and streams
nix run .#fmt            format Haskell and Nix
nix run .#lint           hlint with the shared ruleset
nix flake check          build, test, format, lint, conformance
```
