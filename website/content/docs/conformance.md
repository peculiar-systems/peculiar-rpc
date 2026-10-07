---
title: Conformance
section: Internals
order: 10
---

# Conformance

Three suites run in every `nix flake check`. The [Connect conformance suite](https://github.com/connectrpc/conformance) is the test runner the Connect team holds its own implementations to, across all three protocols. The [gRPC interop cases](https://github.com/grpc/grpc/blob/master/doc/interop-test-descriptions.md) are the ones every gRPC implementation runs against every other, played here against grpc-go in both directions. protobuf's own conformance runner checks the JSON codec.

## Server: 10,204 of 10,204

| Axis        | Covered                                                                                       |
| ----------- | --------------------------------------------------------------------------------------------- |
| Protocols   | gRPC, gRPC-Web, Connect                                                                       |
| Codecs      | protobuf and JSON                                                                             |
| HTTP        | HTTP/1.1 and HTTP/2, plaintext (h2c) and TLS                                                  |
| TLS         | server certificates and mutual TLS                                                            |
| Streams     | unary, server, client, half- and full-duplex bidi                                             |
| Compression | identity, gzip, deflate, snappy, zstd and brotli                                              |
| Also        | Connect GET, receive limits, deadlines, error details, duplicate metadata, malformed requests |

One case is marked flaky rather than required: a native gRPC stream that runs past its deadline can, under heavy machine load, lose the race against the client's own clock. The server's status is correct, and occasionally arrives after the client has already given up.

## Client: 13,038 of 13,038

gRPC, gRPC-Web and Connect, with both codecs, each over HTTP/2 (h2c and TLS), and gRPC-Web and Connect over HTTP/1.1 too. Mutual TLS on every one of them. All six compressions, every stream type, and cancellation before close, after close and after a number of responses. Nothing is excluded and nothing is marked as failing.

The client used to run on grapesy's, which read three kinds of malformed response wrongly: a status in both headers and trailers, a status in the headers of a response that also had a body, and a compressed message whose compression was never declared. The client that replaced it reads all three as the protocol says.

## gRPC interop: 64 of 64

grpc-go's own interop client, built from grpc-go v1.84.0, runs its cases against peculiar-rpc's server, and peculiar-rpc's client runs the same cases against grpc-go's interop server. Each direction runs over h2c and over TLS, so 16 cases make 64 runs:

`empty_unary`, `large_unary`, `client_streaming`, `server_streaming`, `ping_pong`, `empty_stream`, `timeout_on_sleeping_server`, `cancel_after_begin`, `cancel_after_first_response`, `status_code_and_message`, `special_status_message`, `custom_metadata`, `unimplemented_method`, `unimplemented_service`, `rpc_soak` and `channel_soak`.

Both sides are written against the public `Peculiar.Rpc` module only, in `interop/`. The cases grpc-go runs and these leave out need Google Cloud credentials, ALTS, several backends or ORCA load reports.

On the client side, cancellation is Haskell's own: a call cancelled from another thread ends there, and the case then checks that the same connection still carries a call.

## JSON: 2,740 of 2,817

protobuf's `conformance_test_runner`, built from the protobuf release in nixpkgs, sends the codec every JSON case it has, with `--enforce_recommended`: proto3 and proto2 test messages, every well-known type, field names in both spellings, duplicate keys, nesting limits, and malformed input of every kind.

The 77 it fails are listed in `json/conformance/failing.txt`, and none of them is a fault in the JSON mapping. In some the runner hands over binary protobuf that proto-lens's parser accepts or rejects differently, in the rest proto2 extensions, which proto-lens doesn't model. A case that starts passing fails the check too, so the list can only shrink.

## What it found

In the translation layer:

- receive limits have to be checked against the decompressed message, so gzip and deflate frames are inflated with the output capped just past the limit, and a small bomb costs no more than the limit itself;
- a cut body is drained on its own thread, so a full-duplex client waiting for an answer is never blocked by the server reading the rest, and a kept-alive HTTP/1.1 connection never has two readers;
- a stream that ends without `grpc-status` gets one: `DEADLINE_EXCEEDED` once the deadline has passed, `UNKNOWN` otherwise;
- deadlines run from the request's arrival, not from when the handler starts;
- the layer decompresses every codec itself and hands the server plain messages, which grapesy, with no snappy, zstd or brotli, needed and the native server keeps.

In the client:

- response metadata was out of reach, which is why `onHeaders` and `onTrailers` exist;
- metadata names given in mixed case were dropped, and are now lowercased;
- a producer that failed left the request body waiting forever, and now ends the call;
- http-client's failures escaped as `HttpException`, and are now gRPC codes;
- a server that compresses its end-stream message or trailer frame, which the Connect and gRPC-Web specifications allow, has both inflated before they are read.

The interop cases found two more:

- the client ran on the http2 package's default settings, whose limit of ten PINGs a second made it close the connection while grpc-go's server streamed a large response with bandwidth-probing PINGs in flight. The client now takes the server's `Http2Settings`;
- in `http-semantics`, a message that does not fit in what is left of a DATA frame keeps its remainder outside the stream's queue, and the sender waits for that queue before sending it. The last message before a pause then waits for the next one, which a ping-pong exchange never sends. Both sides flush whenever they have nothing more queued, which sends it, and lets messages that arrive together share frames and writes.

Upgrading to http2 5.4.6 for its security fixes brought one more: it waits for the connection's flow-control window to reopen before flushing what it has already buffered, though those bytes are what the window was spent on. A peer that grants more window only once it has received nearly all of it would wait for ever. grpc-go, nghttp2, Go's net/http and this package's own client all grant it well before that, so nothing here works around it.

Load from grpc-go found one more thing, in the http2 package's defaults: it closes a connection that sends more than four empty DATA frames, ten PINGs or four SETTINGS frames a second. grpc-go ends every call with an empty DATA frame and keeps a PING in flight to measure bandwidth, so under load about one call in a hundred failed on the GOAWAY. `defaultHttp2`, which `serve` uses, raises those limits well past what such a client sends.

And in grapesy 1.1.1 and grpc-spec 1.0.0, which the server used to run on, worked around in the layer until the package served native gRPC itself:

| Behaviour                                             | Handled by                                                             |
| ----------------------------------------------------- | ---------------------------------------------------------------------- |
| Repeated metadata values arrive reversed              | `Metadata` joins duplicates in order first                             |
| A timeout in hours is read as 24 minutes              | the layer never sends the hour unit, and converts incoming ones itself |
| A stream killed at its deadline is reset, not trailed | the layer absorbs the reset and settles the status itself              |

## Against other servers

`nix run .#bench-compare` starts the conformance servers of peculiar-rpc, connect-go and grpc-go the way the suite does, and loads each with ghz over h2c with the same unary call. On one laptop:

| Server       | 1 caller             | 50 callers     |
| ------------ | -------------------- | -------------- |
| peculiar-rpc | 5,300 calls/s, 133µs | 30,100 calls/s |
| connect-go   | 3,900 calls/s, 203µs | 20,100 calls/s |
| grpc-go      | 6,500 calls/s, 100µs | 38,600 calls/s |

`nix run .#bench-streams` does the same for streams, through the gRPC interop TestService that both peculiar-rpc and grpc-go serve: 1000 messages of 100 bytes a call, with one caller, 50 callers on one connection, and 50 callers on ten. Messages a second, with the server built with `-A64m` in brackets (see [runtime flags](/docs/serving#runtime-flags)):

| Shape         | Callers  | peculiar-rpc          | grpc-go   |
| ------------- | -------- | --------------------- | --------- |
| unary         | 1        | 5,000                 | 6,500     |
| unary         | 50 on 1  | 31,700 (35,100)       | 35,900    |
| unary         | 50 on 10 | 29,100 (32,700)       | 38,100    |
| server stream | 1        | 267,000               | 349,000   |
| server stream | 50 on 1  | 479,000 (548,000)     | 641,000   |
| server stream | 50 on 10 | 533,000 (638,000)     | 1,335,000 |
| client stream | 1        | 678,000               | 695,000   |
| client stream | 50 on 1  | 224,000 (235,000)     | 614,000   |
| client stream | 50 on 10 | 1,150,000 (1,339,000) | 2,434,000 |
| bidi stream   | 1        | 236,000               | 317,000   |
| bidi stream   | 50 on 1  | 247,000 (299,000)     | 454,000   |
| bidi stream   | 50 on 10 | 372,000 (429,000)     | 897,000   |

One caller at a time, peculiar-rpc is at 75–100% of grpc-go. Under load it is at 40–90%, and the gap is widest where many small messages share one connection: http2 reads each connection's frames on one thread and hands every one to its stream's thread, where grpc-go's scheduler makes that hand-off far cheaper. Most of what is left in the handlers is proto-lens itself, which encodes each nested message into a fresh 4KiB buffer to learn its length.

What profiling found on the way here:

- every native gRPC message was framed, unframed, framed again and unframed again between the translation layer and the server. Handlers now take messages straight from it;
- encoding ran on the connection's sender thread, because a queued frame was an unevaluated thunk, so one thread encoded every stream's replies. Messages are evaluated on their own threads before they are queued;
- deadlines went through the process's one timer manager, and every stream forked a thread to race its own. With a 20-second `grpc-timeout`, as ghz sends, unary calls on ten connections ran at 14,000 a second instead of 32,000. `Peculiar.Rpc.Deadline` keeps a list of deadlines per core, and hands only those less than 100ms away to the timer manager;
- proto-lens's `encodeMessage` built each message into a 4KiB buffer and copied it twice. Messages are now built into a 512-byte first chunk;
- the write buffer was 4KiB, so every DATA frame and every write was capped at a quarter of a frame; a 1MiB reply took 23ms and now takes 10.

Serving native gRPC without grapesy made a 1000-message server stream eight times faster, 51ms to 6.4ms. Queueing the client's messages and letting each connection's writer flush only when it has nothing left to send made a 1000-message client stream four times faster, 37ms to 8.5ms, and a bidi stream three times, 67ms to 20ms.

## Running it

```shell
nix run .#conformance    # the Connect suite, server and client
nix run .#interop        # the gRPC interop cases, both directions
nix run .#json-conformance  # protobuf's JSON conformance suite
nix run .#bench-compare  # unary calls against connect-go and grpc-go
nix run .#bench-streams  # unary calls and streams against grpc-go
nix flake check          # all of it, with everything else
```

The runner is `connectconformance` v1.0.5, built from source by the flake. The configurations are `conformance/server.yaml` and `conformance/client.yaml`, and the one accepted exception, the flaky server case, sits beside them in `known-flaky.txt`. The conformance client runs cases four per core at a time, the pool the reference clients use.
