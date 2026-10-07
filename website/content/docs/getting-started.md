---
title: Getting started
section: Start
order: 1
---

# Getting started

A service, a server and a client, from a `.proto` file.

> **Preview.** peculiar-rpc isn't on Hackage yet; take it from GitHub as a flake. Everything below works as written. The package names are `peculiar-rpc` and, for the pure wire formats, `peculiar-rpc-core`.

## 1. Write the contract

The contract is the single source of truth. Everything else is generated from it or derived from what was generated.

```proto proto/example/v1/echo.proto
syntax = "proto3";

package example.v1;

message SayRequest { string text = 1; }
message SayResponse { string text = 1; }
message CountRequest { int32 upto = 1; }
message CountResponse { int32 n = 1; }

service Echo {
  rpc Say(SayRequest) returns (SayResponse) {
    option idempotency_level = NO_SIDE_EFFECTS;
  }
  rpc Count(CountRequest) returns (stream CountResponse);
}
```

## 2. Generate the messages

Messages come from [proto-lens](https://github.com/google/proto-lens). Run `protoc` with its plugin and compile the output as its own library, since generated code is written for plain GHC2021 rather than your project's extensions.

```shell
protoc --plugin=protoc-gen-haskell=$(which proto-lens-protoc) \
  --haskell_out=gen -I proto example/v1/echo.proto
```

## 3. Derive the server and client

One splice writes an `EchoServer` record with one field per RPC, and an `EchoClient` record to call it with. Import the library qualified, so its common names read as `Rpc.Context`, `Rpc.Options` and so on.

```haskell src/Echo.hs
{-# LANGUAGE TemplateHaskell #-}

module Echo (EchoServer (..), EchoClient (..), echo) where

import Data.Foldable (for_)
import Data.ProtoLens (defMessage)
import Data.ProtoLens.Labels ()
import Lens.Family2 ((&), (.~), (^.))
import Peculiar.Rpc qualified as Rpc
import Proto.Example.V1.Echo

Rpc.deriveService ''Echo

echo :: EchoServer
echo = EchoServer{count, say}

say :: Rpc.Context -> SayRequest -> IO SayResponse
say _ request = pure (defMessage & #text .~ request ^. #text)

count :: Rpc.Context -> CountRequest -> (CountResponse -> IO ()) -> IO ()
count _ request send = for_ [1 .. request ^. #upto] \n -> send (defMessage & #n .~ n)
```

Leave out `count` and the record doesn't build. Rename `Say` in the `.proto` and the field moves with it. Give `count` a unary type and it won't match the streaming kind.

## 4. Serve it

The service is a WAI application. Mount it in your own server, beside whatever else the product serves:

```haskell app/Main.hs
import Echo (echo)
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Peculiar.Rpc qualified as Rpc

main :: IO ()
main = Rpc.withServer Rpc.defaultCalls (Rpc.withReflection [Rpc.endpoint echo]) \rpc ->
  Warp.run 8080 (Rpc.middleware rpc app)

app :: Wai.Application
app _ respond = respond (Wai.responseLBS status200 [] "hello from the rest of the product")
```

Port 8080 now answers native gRPC over h2c, gRPC-Web and Connect over HTTP/1.1 and HTTP/2, and hands every other path to `app`. `endpoint` reads the routes from the service's method list, and allows Connect GET for `Say` because its descriptor says it has no side effects.

```shell
grpcurl -plaintext -d '{"text": "hello"}' localhost:8080 example.v1.Echo/Say
curl localhost:8080/
```

With nothing else to serve, one call runs a server of the library's own, with TLS, keepalive and graceful shutdown:

```haskell
main :: IO ()
main = Rpc.serveEndpoints Rpc.defaultSettings{Rpc.port = 8080} (Rpc.withReflection [Rpc.endpoint echo])
```

## 5. Call it

The generated client works over native gRPC, or over Connect and gRPC-Web for servers that only speak those.

```haskell
import Echo (EchoClient (..))
import Peculiar.Rpc qualified as Rpc

hello :: IO ()
hello =
  Rpc.withNativeClient Rpc.Plaintext "localhost" 8080 \transport -> do
    let remote = Rpc.client Rpc.defaultOptions{Rpc.deadline = Just (Rpc.seconds 5)} transport :: EchoClient
    reply <- remote.say (defMessage & #text .~ "hello")
    remote.count (defMessage & #upto .~ 3) print
    print (reply ^. #text)
```

## Where next

- [Services and handlers](/docs/services): the handler shapes, the `Context`, typed errors.
- [Serving](/docs/serving): mounting, CORS, receive limits, TLS, and serving it alone.
- [Clients](/docs/clients): transports, options, metadata, deadlines.
