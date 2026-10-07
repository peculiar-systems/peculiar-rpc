---
title: Services and handlers
section: Guide
order: 2
---

# Services and handlers

One record per service, one field per method, and a type for each field that the streaming kind decides.

## What the splice writes

`Rpc.deriveService ''Echo` reads the method list proto-lens generated for `Echo` and writes two records and their instances. `deriveServer` and `deriveClient` write one half each.

| Declaration                          | What it is                                                                                 |
| ------------------------------------ | ------------------------------------------------------------------------------------------ |
| `EchoServer`                         | A record with one handler field per RPC, named after it (`say`, `count`, …)                |
| `instance Implementation EchoServer` | Turns the record into the server's handlers, and lists the message types reflection serves |
| `EchoClient`                         | A record with one calling field per RPC                                                    |
| `instance Client EchoClient`         | `Rpc.client options transport` builds one                                                  |

Field names are the method names, so a module deriving services should enable `DuplicateRecordFields` (both records share them) and `NoFieldSelectors` (so a method called `sum` doesn't clash with `Prelude.sum`).

## The four handler shapes

A handler's type is a type family of the method's streaming kind, so a streaming handler can't be given where a unary one is expected.

| Kind          | Server field                                       | Client field                                       |
| ------------- | -------------------------------------------------- | -------------------------------------------------- |
| Unary         | `Context -> i -> IO o`                             | `i -> IO o`                                        |
| Server stream | `Context -> i -> (o -> IO ()) -> IO ()`            | `i -> (o -> IO ()) -> IO ()`                       |
| Client stream | `Context -> IO (Maybe i) -> IO o`                  | `((i -> IO ()) -> IO ()) -> IO o`                  |
| Bidi stream   | `Context -> IO (Maybe i) -> (o -> IO ()) -> IO ()` | `((i -> IO ()) -> IO ()) -> (o -> IO ()) -> IO ()` |

Messages are plain proto-lens messages, not wrappers. On the server, `IO (Maybe i)` yields each request in turn and `Nothing` once the client has finished. On the client, the producer is handed a `send` and returns when it's done sending.

A unary method that receives no request message, or more than one, answers `UNIMPLEMENTED` before your handler runs.

## The Context

```haskell
data Context = Context
  { metadata :: Metadata
  , timeout :: Maybe Timeout
  , respondWith :: Metadata -> IO ()
  , trailWith :: Metadata -> IO ()
  , flush :: IO ()
  , peer :: Maybe SockAddr
  , requestPath :: ByteString
  , requestCodec :: Codec
  , requestBytes :: Maybe ByteString
  }
```

- `metadata`: the request's custom metadata. Read it with `Rpc.lookupHeader "authorization" context.metadata`; names are matched lowercased, and `-bin` values come back decoded.
- `timeout`: the deadline the caller set, if any. The server enforces it on its own; this is for handlers that want to budget work.
- `respondWith`: sets the response headers. It must run before the first message is sent.
- `trailWith`: adds trailers. They go out with the final status, including on errors.
- `peer`: the address of the connection the call arrived on, as the socket reports it: the TCP peer, under TLS too, or a Unix socket's address. It is the connection's own address and nothing else: `X-Forwarded-For` and `Forwarded` reach the handler as metadata, and whether to trust them is the application's decision. Two calls on one HTTP/2 connection see the same peer.
- `flush`: sends the headers now, before any message, so a client waiting on them isn't held up by a slow first response.
- `requestPath`: the method the call was made to, as `/package.Service/Method`, whichever protocol carried it. A REST call has the path of the method its route binds to.
- `requestCodec`: `Proto` or `Json`, the encoding the request arrived in.
- `requestBytes`: for unary and server-streaming methods, the request message exactly as it arrived: decompressed and unframed, in `requestCodec`, before it is decoded or validated. A REST call has the JSON built from its path, query and body. Client and bidi streams have `Nothing`.

The request is read before the server's interceptor runs, so an interceptor sees `requestBytes` too. A request signature over the path and the bytes can then be checked once, for every method:

```haskell
verified :: Rpc.Interceptor
verified = Rpc.Interceptor \_ context continue ->
  case (Rpc.lookupHeader "x-signature" context.metadata, context.requestBytes) of
    (Just signature, Just bytes) | valid signature (context.requestPath <> bytes) -> continue context
    _ -> Rpc.throwRpc Rpc.Unauthenticated "bad signature"
```

Checking against the bytes as sent matters: re-encoding a decoded message need not reproduce them, since field order and unknown fields are not preserved.

## Errors

A handler fails by throwing an `RpcError`. The client, whichever protocol it speaks, receives the same code, message, `google.rpc.Status` details and trailing metadata.

```haskell
lookupUser :: Rpc.Context -> LookupRequest -> IO LookupResponse
lookupUser _ request = do
  found <- findUser (request ^. #id)
  maybe (Rpc.throwRpc Rpc.NotFound "no such user") pure found
```

For details, build the error directly: `Rpc.RpcError{status = Rpc.Status{code, message, details}, metadata}`, where each `Rpc.Detail` is a type URL and the packed message.

To tell a retrying client when to come back, put `Rpc.pushback (Rpc.seconds 2)` in the error's metadata; `Rpc.noRetry` tells it not to. Both set `grpc-retry-pushback-ms`, the one `grpc-` name metadata may carry. A REST caller gets it as a `Retry-After` header, in whole seconds. Any other exception becomes `UNKNOWN` and goes to `Calls.report`. Its text is the message unless `Calls.revealErrors` is off.

## Browser reachability

A browser's `fetch` can't stream a request body, so client-streaming and bidi methods are out of a browser's reach. Put `Rpc.BrowserReachable Echo` in a signature and the compiler rejects any such method, naming it:

```haskell
webFacing :: (Rpc.BrowserReachable Echo) => EchoServer -> Rpc.Endpoint
webFacing = Rpc.endpoint
```

Connect over HTTP/2 does support client and bidi streaming for non-browser clients, so the check is something you choose to apply, not a rule of the server.
