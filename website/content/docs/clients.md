---
title: Clients
section: Guide
order: 4
---

# Clients

The same generated record calls a service over gRPC, Connect or gRPC-Web, on HTTP/2 or HTTP/1.1.

## Only the client

A program that only makes calls can depend on `peculiar-rpc-client` alone. It is the whole client, without the server: no warp, no wai. Its `Peculiar.Rpc.Client` module carries every name a caller uses, under the same names as `Peculiar.Rpc`:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Peculiar.Rpc.Client qualified as Rpc
import Proto.Example.V1.Echo

Rpc.deriveClient ''Echo
```

It still depends on the http2 and http-semantics releases the flake builds, since the client runs on them.

## Transports

| Transport                   | Built with                     | Speaks                                                                     |
| --------------------------- | ------------------------------ | -------------------------------------------------------------------------- |
| `Http2 connection protocol` | `Rpc.withConnection`           | gRPC, Connect or gRPC-Web over HTTP/2, plaintext (h2c) or TLS, full-duplex |
| `Http1 target`              | `Rpc.web manager protocol url` | Connect or gRPC-Web over HTTP/1.1, through http-client                     |

```haskell
Rpc.withConnection Rpc.Plaintext "localhost" 8080 \connection -> do
  let native = Rpc.client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Grpc) :: EchoClient
      connect = Rpc.client Rpc.defaultOptions (Rpc.Http2 connection Rpc.Connect) :: EchoClient
  native.say (defMessage & #text .~ "native")
  connect.say (defMessage & #text .~ "connect")
```

One connection carries any number of concurrent calls, in any of the three protocols. It opens on the first call and opens again on the next one if the server closed it, so a retry after a server restart reaches the new process. `Rpc.withNativeClient security host port` is the same connection with gRPC already chosen.

`Rpc.connect` takes more say over how it dials:

```haskell
let dialing = Rpc.defaultDialing{Rpc.pool = 4, Rpc.pinging = Just Rpc.Keepalive{Rpc.every = Rpc.seconds 30, Rpc.within = Rpc.seconds 10}}
Rpc.connect dialing (Rpc.Tcp "api.example.org" 443) \connection -> ...
Rpc.connect Rpc.defaultDialing (Rpc.Unix "/run/example/rpc.sock") \connection -> ...
```

- `pool`: calls rotate across this many connections. Each starts from a different address the name resolves to and fails over to the next one it can reach, and names are resolved again whenever a connection is opened, so a pool spreads load across every address a server has.
- `pinging`: off by default, as in other gRPC clients, because servers may refuse clients that ping too often. Turn it on for mobile and long-lived streams: after `every` of silence the client pings, and a connection that stays silent for `within` more is closed and its calls fail `UNAVAILABLE`, instead of waiting for a deadline that may never come.
- `http2`: the same `Http2Settings` a server takes, `defaultHttp2` by default. Its flow-control windows let a large response arrive without waiting on window updates, and its rate limits let a server keep bandwidth-probing PINGs in flight, as grpc-go does, without the client closing the connection.
- A server's GOAWAY is honoured without cutting anything off: the connection takes no new calls, finishes the ones it has, and the next call opens a new one.

```haskell
manager <- newManager defaultManagerSettings
let Right target = Rpc.web manager Rpc.Connect "http://localhost:8080"
    remote = Rpc.client Rpc.defaultOptions (Rpc.Http1 target) :: EchoClient
remote.say (defMessage & #text .~ "connect")
```

Use HTTP/1.1 where that is all there is between you and the server. Client and bidi streams over it are half-duplex: every request is sent before the first response is read.

## Options

```haskell
data Options = Options
  { deadline :: Maybe Timeout
  , requestMetadata :: Metadata
  , onHeaders :: Metadata -> IO ()
  , onTrailers :: Metadata -> IO ()
  , compression :: Maybe Compression
  , compressAbove :: Int
  , retry :: Maybe Retry
  , clientInterceptor :: ClientInterceptor
  , httpGet :: Bool
  }
```

- `deadline`: `Rpc.milliseconds 250`, `Rpc.seconds 5`. Sent to the server, which enforces it, and enforced by the client too, so a server that has gone quiet still ends the call with `DEADLINE_EXCEEDED`.
- `requestMetadata`: `Rpc.header "authorization" token`, combined with `<>`. Names are lowercased; a `-bin` header's value is raw bytes and is base64-encoded on the wire for you.
- `onHeaders`, `onTrailers`: receive the response metadata, whatever the protocol put it in: HTTP/2 trailers, Connect's `trailer-` headers or end-stream message, or gRPC-Web's trailer frame. On a failed call, the trailers arrive here and on the error.
- `compression`: compresses requests with `Rpc.Gzip`, `Rpc.Deflate`, `Rpc.Snappy`, `Rpc.Zstd` or `Rpc.Brotli`, but only messages of at least `compressAbove` bytes, 1 KiB by default: below that, compressing costs more than it saves. Every call accepts all five in responses, whatever it sends. zstd is the best general choice; brotli compresses at level 4.
- `httpGet`: sends Connect unary calls to methods marked `NO_SIDE_EFFECTS` as GET, with the message in the URL, so a CDN or a browser cache can answer them. A URL that would pass 8 KiB goes as POST instead.

Build a record per set of options; they're cheap.

```haskell
let authed = Rpc.defaultOptions{Rpc.deadline = Just (Rpc.seconds 5), Rpc.requestMetadata = Rpc.header "authorization" token}
    remote = Rpc.client authed transport :: EchoClient
```

## Retries

```haskell
let retrying = Rpc.defaultOptions{Rpc.retry = Just Rpc.defaultRetry}
```

`defaultRetry` makes up to four attempts on `UNAVAILABLE`, backing off from 100 milliseconds, doubling up to 5 seconds, with jitter. Every field of `Rpc.Retry` can be changed: `attempts`, `initialBackoff`, `maxBackoff`, `multiplier` and `retryOn`, the codes worth another attempt.

- All attempts share the call's one deadline. A backoff that would outlast it isn't taken, and the last failure is returned.
- A server's `grpc-retry-pushback-ms` trailer replaces the backoff, and a negative one stops retrying. The failure's metadata carries it, and a peculiar-rpc server sets it with `Rpc.pushback` or `Rpc.noRetry`.
- Unary calls are retried, and so are server streams that have delivered nothing yet. Once a message has reached your handler, the call is never repeated behind its back. Client and bidi streams are never retried: the producer may not be able to run twice.

Retrying something that isn't idempotent is your decision to make, which is why retries are off until you set them.

## Interceptors

A `ClientInterceptor` wraps every call made with its options. It sees the route and the options, and can change them before the call runs, time it, log it, or refuse it:

```haskell
stamping :: Rpc.ClientInterceptor
stamping = Rpc.ClientInterceptor \route options continue -> do
  token <- currentToken
  continue options{Rpc.requestMetadata = options.requestMetadata <> Rpc.header "authorization" token}
```

Interceptors compose with `<>`, the left one outermost. A retrying call passes through its interceptor once, around all its attempts.

## Errors

Every failure arrives as an `RpcError`, over every transport: a status from the server, a deadline, a lost connection (`UNAVAILABLE`), a response of the wrong content type (`UNKNOWN`, or `INTERNAL` when only the codec differs), a compressed message whose compression was never declared (`INTERNAL`), or a unary reply with zero messages or several (`UNIMPLEMENTED`).

```haskell
result <- try @Rpc.RpcError (remote.say request)
case result of
  Left failure | failure.status.code == Rpc.Unauthenticated -> signIn
  Left failure -> throwIO failure
  Right reply -> use reply
```

## TLS

A `Rpc.ClientTls` says what to trust and, optionally, who you are:

```haskell
data ClientTls = ClientTls
  { trust :: Trust
  , identity :: Maybe (ByteString, ByteString)
  }

data Trust = SystemTrust | TrustPem ByteString
```

- HTTP/2: `Rpc.withConnection (Rpc.Secure tls) host port`. The connection negotiates `h2` by ALPN and checks the certificate against `host`. `Rpc.systemTls` trusts the system store.
- HTTP/1.1: `Rpc.webManager tls` gives an http-client `Manager` for an `https://` URL.

With an `identity`, both present the client certificate when the server asks, so mutual TLS works on every transport and protocol.
