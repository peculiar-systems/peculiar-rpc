---
title: Serving
section: Guide
order: 3
---

# Serving

The service is a WAI application. Mount it in the server you already run, beside your own routes, or let the library run one for you.

## Mounting

```haskell
withServer :: Calls -> [Endpoint] -> (Server -> IO a) -> IO a
application :: Server -> Wai.Application
middleware :: Server -> Wai.Middleware
```

A `Server` is every endpoint together with the settings that concern calls. It is a value, not a process: nothing listens until you hand it to a server. `withServer` is a bracket because the value holds the deadline service.

```haskell app/Main.hs
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Peculiar.Rpc qualified as Rpc

main :: IO ()
main = Rpc.withServer Rpc.defaultCalls [Rpc.endpoint echo] \rpc ->
  Warp.run 8080 (Rpc.middleware rpc app)

app :: Wai.Application
app _ respond = respond (Wai.responseLBS status200 [] "the rest of the product")
```

Port 8080 now answers native gRPC, gRPC-Web, Connect, REST and JSON for `echo`, and everything else from `app`, over HTTP/1.1 and h2c alike.

- **`middleware rpc app`** answers the server's own routes and hands every other request to `app`: the `/package.Service/Method` paths, the REST paths its `google.api.http` options declare, their CORS preflights, and `/openapi.json` when `openApi` is set. The RPC routes win, so a catch-all in `app` never sees a call. A call for a method no service has goes to `app` too, which lets two servers mount side by side.
- **`application rpc`** answers everything itself. A path no service has is `UNIMPLEMENTED` in each protocol's own form: trailers for gRPC, a 404 with a Connect error for Connect, a 404 for anything else.

In servant, end the API with a `Raw` endpoint served by `Tagged (Rpc.application rpc)`, or wrap the whole servant application in `Rpc.middleware rpc`. Scotty, Yesod and anything else that takes a WAI `Application` or `Middleware` work the same way.

### Calls

```haskell
data Calls = Calls
  { interceptor :: Interceptor
  , revealErrors :: Bool
  , report :: SomeException -> IO ()
  , cors :: Maybe Cors
  , receiveLimit :: Maybe Int
  , headerLimit :: Maybe Int
  , getAllowed :: ByteString -> Bool
  , openApi :: Maybe Info
  }
```

| Field          | Default         | What it does                                                                                       |
| -------------- | --------------- | -------------------------------------------------------------------------------------------------- |
| `interceptor`  | `mempty`        | Wraps every call; [below](#interceptors).                                                          |
| `revealErrors` | `True`          | Whether an exception other than `RpcError` reaches the client with its text, or only as `UNKNOWN`. |
| `report`       | print to stderr | Receives exceptions from handlers other than `RpcError`, and, under `serve`, failed connections.   |
| `cors`         | `Nothing`       | Answers preflights and exposes the headers browser clients read; [below](#browsers-and-limits).    |
| `receiveLimit` | `Nothing`       | The largest message a call may send, after decompression.                                          |
| `headerLimit`  | 16 KiB          | The largest request headers, in total.                                                             |
| `getAllowed`   | nothing         | Paths that take Connect GET beyond the methods marked `NO_SIDE_EFFECTS`.                           |
| `openApi`      | `Nothing`       | Serve an OpenAPI 3.1 document at `/openapi.json`; see [REST and OpenAPI](/docs/rest).              |

Routes and Connect GET permissions are read from the endpoints' own descriptors, and are never written down.

### Native gRPC through WAI

Native gRPC ends every response with trailers, and WAI has no field for them. Warp does: a request's vault carries `HTTP2Data`, whose `http2dataTrailers` is the http2 package's own trailers maker, and warp's HTTP/2 hands it to the stream. The service sets it before it answers, so a WAI response carries its `grpc-status` as real HTTP/2 trailers after the last message.

WAI can't end a stream on its HEADERS frame, which is what a gRPC Trailers-Only response is. The service sends it as headers with the status and content type, no messages, and every other field as trailers: the same response, in the specification's other form.

Warp serves HTTP/2 over TLS through `warp-tls`, and over plaintext when the client opens with the HTTP/2 preface. Its HTTP/2 runs with the http2 package's default rate limits, which a heavily loaded grpc-go client can exceed; see [Serving it alone](#serving-it-alone).

## Serving it alone

When there is no other server, `serveEndpoints` runs one:

```haskell
main :: IO ()
main = Rpc.serveEndpoints Rpc.defaultSettings{Rpc.port = 8080, Rpc.beside = Just app} [Rpc.endpoint echo]
```

It is `withServer` and `middleware` over `serve :: Settings -> Wai.Application -> IO ()`, which runs any WAI application. `beside` is the application for everything that isn't RPC; left `Nothing`, the service answers alone, as `application` does.

```haskell
data Settings = Settings
  { host :: Text
  , port :: Int
  , http2 :: Http2Settings
  , onListening :: Int -> IO ()
  , tls :: Maybe Tls
  , stopWhen :: IO ()
  , grace :: Timeout
  , unixSocket :: Maybe FilePath
  , keepalive :: Maybe Keepalive
  , idleTimeout :: Maybe Timeout
  , maxAge :: Maybe Timeout
  , maxConnections :: Maybe Int
  , calls :: Calls
  , beside :: Maybe Wai.Application
  }
```

| Field          | Default              | What it does                                                                                                                           |
| -------------- | -------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| `host`, `port` | `127.0.0.1`, `50051` | Where to listen. Port `0` picks a free one.                                                                                            |
| `onListening`  | nothing              | Called with the bound port once the socket is listening. Useful with port `0`.                                                         |
| `tls`          | `Nothing`            | Serve TLS on the same port; [below](#tls).                                                                                             |
| `http2`        | `defaultHttp2`       | Streams per connection, flow-control windows, and the rate limits against floods; the defaults allow what a busy grpc-go client sends. |
| `calls`        | `defaultCalls`       | The [settings for calls](#calls), when `serveEndpoints` builds the `Server`.                                                           |
| `beside`       | `Nothing`            | The application for every request that isn't RPC.                                                                                      |

The rest are below: [connections](#connections), [Unix sockets](#unix-sockets) and [shutting down](#shutting-down).

### One port

A plaintext connection is sniffed. One that opens with the HTTP/2 preface is served as HTTP/2, anything else by warp as HTTP/1.1. A TLS connection chooses by ALPN: `h2` or `http/1.1`. Both reach the same WAI application: HTTP/2 through warp's own bridge from the http2 package to WAI, with `serve`'s HTTP/2 settings.

Native gRPC needs HTTP/2: over HTTP/1.1 its trailers, and so its status, can't be delivered. Use gRPC-Web or Connect where HTTP/1.1 is all there is.

## Browsers and limits

- **CORS.** `Just Rpc.permissive` answers preflights from any origin and exposes the headers gRPC-Web and Connect clients read (`grpc-status`, `grpc-message`, `grpc-status-details-bin`, the encodings). Use `Rpc.Cors{origins = Rpc.OnlyOrigins [...], credentials, maxAgeSeconds, expose}` to narrow it. Without it, a browser on another origin can't read a single status.
- **Receive limit.** Applies to every protocol, native gRPC included, and is checked against the _decompressed_ size. gzip and deflate messages are inflated with the output capped one byte past the limit, so a compression bomb costs at most the limit. A message over it ends the call with `RESOURCE_EXHAUSTED`.
- **Header limit.** `headerLimit`, 16 KiB by default, refuses a request whose headers are larger with `RESOURCE_EXHAUSTED` in its own protocol. Past 50 KiB warp and the http2 package close the connection before the request reaches the service, and HTTP/2 also refuses any single value over 4 KiB.
- **Connect GET** is allowed for methods marked `NO_SIDE_EFFECTS`, read from the service descriptors.

## Interceptors

An `Interceptor` wraps every call the server takes, health and reflection included. It receives the route and the request's `Context`, and decides whether and how the handler runs:

```haskell
authenticated :: Rpc.Interceptor
authenticated = Rpc.Interceptor \route context continue ->
  case Rpc.lookupHeader "authorization" context.metadata of
    Just token | valid token -> continue context
    _ | route.service == "grpc.health.v1.Health" -> continue context
    _ -> Rpc.throwRpc Rpc.Unauthenticated "sign in first"
```

An `RpcError` thrown here reaches the client like one from a handler, in whichever protocol it speaks. The interceptor can also pass the handler a changed `Context`, time the call, or log it. Interceptors compose with `<>`, the left one outermost.

## Deadlines

Every call's deadline is measured from the moment the request arrives. When it passes, the call ends with `DEADLINE_EXCEEDED` without waiting on the handler, and the handler is cancelled. Streams end a few milliseconds early (at most 10, or 1/16 of the timeout), because the client's clock started before the request reached the server.

A stream that ends without any status gets one: `DEADLINE_EXCEEDED` if its deadline has passed, `UNKNOWN` otherwise.

## Connections

Under `serve`, every HTTP/2 connection has a supervisor. Any bytes received count as the peer being alive; after `keepalive.every` of quiet it sends a PING, and if nothing at all comes back within `keepalive.within` the connection is closed. When a connection has been idle for `idleTimeout`, or has lived past `maxAge`, the supervisor sends GOAWAY: the client opens a new connection for its next call, calls already running finish, and the connection closes once they have or `grace` runs out. HTTP/1.1 connections are left to warp's own timeouts.

## Runtime flags

Build the server with `-threaded -rtsopts "-with-rtsopts=-N -A64m"`. `-N` runs it on every core. `-A64m` gives each core a 64MiB allocation area instead of 4MiB, so the collector stops all of them far less often: in the stream benchmarks it cut collection time from 14% of the run to 8%, and raised throughput by 10–25%, unary calls under 50 callers from 31,700 to 35,100 a second.

Every call on one HTTP/2 connection passes through that connection's one reader and one writer, and each frame they hand to a handler on another core costs a wake-up there. A process that serves a few connections carrying many small streams each can do better with fewer cores: on one connection with 50 callers, `-N2` streamed about half as fast again as `-N`. A process that serves many connections wants them all.

## Unix sockets

```haskell
Rpc.serveEndpoints Rpc.defaultSettings{Rpc.unixSocket = Just "/run/example/rpc.sock"} [Rpc.endpoint echo]
```

A sidecar or a proxy on the same machine skips TCP entirely, and file permissions decide who may connect. Every protocol works over it, and a stale socket file from an earlier run is replaced.

## Shutting down

`serve` and `serveEndpoints` return when `stopWhen` does, after draining:

1. Every HTTP/2 connection is sent GOAWAY, new calls are answered `UNAVAILABLE` in each protocol's own form, so a client with retries goes elsewhere, and other requests get a 503. The listener closes.
2. Requests already running get up to `grace` to finish.
3. The remaining connections are closed, and `serve` returns.

Mounted in a server of your own, shutting down is that server's business.

By default `stopWhen` waits for SIGTERM or SIGINT, which is what systemd and Kubernetes send. Pass any `IO ()` instead: `takeMVar stop` in a test, or `Rpc.terminated` raced against something of your own.

## TLS

```haskell
main :: IO ()
main = do
  Right tls <- Rpc.tlsFromFiles "cert.pem" "key.pem"
  Rpc.serveEndpoints Rpc.defaultSettings{Rpc.port = 443, Rpc.tls = Just tls} [Rpc.endpoint echo]
```

`Tls` holds an `IO Credentials` that runs for every new connection, so replacing a certificate needs no restart. Build one yourself if the certificate comes from somewhere other than files.

For mutual TLS, `Rpc.requireClients authorities tls` takes a PEM of the certificate authorities to trust, and makes every connection present a client certificate that validates against them.

## Behind a terminator

If something in front of the server terminates TLS, point it at the plaintext port. For native gRPC, the terminator has to speak HTTP/2 to the server (for nginx, `grpc_pass grpc://…`). gRPC-Web and Connect work over plain HTTP/1.1 proxying too.

nginx's `grpc_pass` advertises a stream window of 2^31-1 and never sends a stream WINDOW_UPDATE, so the server must start every stream from the window in the latest SETTINGS it received. Released http2, 5.4.7 included, applies that setting on its sending thread while its receiving thread may already be opening the next stream with the old 65,535 bytes, and a response over 64 KiB on that stream stalls for good. The flake builds http2 5.4.6 with `nix/http2-settings-window.patch`, which applies the setting on the receiving thread before it reads the next frame; build with that http2.
