---
title: How the protocols work
section: Internals
order: 9
---

# How the protocols work

Three protocols, one set of handlers. The differences are small, and they're all on this page.

## The shared frame

gRPC, gRPC-Web and Connect streams all carry messages in the same envelope: one flag byte, four bytes of big-endian length, then the message.

| Flag bit | Meaning                                                                         |
| -------- | ------------------------------------------------------------------------------- |
| `0x01`   | The message is compressed with the call's declared encoding                     |
| `0x02`   | Connect: the end-stream message, JSON carrying the status and trailers          |
| `0x80`   | gRPC-Web: the trailer frame, HTTP header lines carrying the status and trailers |

A flag byte outside these is refused rather than guessed at.

## gRPC

HTTP/2 only, content type `application/grpc` or `application/grpc+proto`. The status travels in HTTP trailers: `grpc-status`, a percent-encoded `grpc-message`, and a `grpc-status-details-bin` holding a `google.rpc.Status`. A reply with no messages can put all of that in the headers instead ("trailers-only"). The package serves it itself. The layer in front enforces the receive limit and deadlines, and supplies a status when a stream would otherwise end without one.

## gRPC-Web

A browser can't read HTTP trailers, so gRPC-Web moves them into the body as a final `0x80` frame. Everything else is gRPC's: the same framing and the same status headers. The content type is `application/grpc-web+proto`, or `application/grpc-web-text+proto` with every byte base64-encoded for old XHR clients. The text form is decoded in chunks split anywhere, including between padded segments.

The translation rewrites the request's content type to native gRPC and passes the frames through. On the way back, it drops any `Trailer` announcement (a browser counts it as trailers that must be empty) and appends the trailer frame.

## Connect

### Unary

`POST` with content type `application/proto`, or `application/json` for the [JSON codec](/docs/rest#json-on-every-protocol), and the bare message as the body: no frame at all. Success is HTTP 200 with the bare response, and trailers become headers prefixed `trailer-`. Failure is a JSON body under a mapped HTTP status:

```shell
HTTP/1.1 404 Not Found
content-type: application/json

{"code":"not_found","message":"no such user"}
```

| Code                                                | HTTP | Code               | HTTP |
| --------------------------------------------------- | ---- | ------------------ | ---- |
| invalid_argument, failed_precondition, out_of_range | 400  | unauthenticated    | 401  |
| permission_denied                                   | 403  | not_found          | 404  |
| already_exists, aborted                             | 409  | resource_exhausted | 429  |
| canceled                                            | 499  | unimplemented      | 501  |
| unavailable                                         | 503  | deadline_exceeded  | 504  |

`unknown`, `internal` and `data_loss` are 500. Details travel as base64 inside the JSON, with the type URL's `type.googleapis.com/` prefix stripped.

### GET

For methods whose descriptor says `NO_SIDE_EFFECTS`, the request can be a `GET` with the message in the query (`?encoding=proto&base64=1&message=…`), so browsers and CDNs can cache it. `endpoint` reads that option from each service's descriptor.

### Streams

Content type `application/connect+proto`, framed like gRPC, always HTTP 200. Instead of trailers, a final `0x02` frame carries `{"error":{…},"metadata":{…}}`. Timeouts arrive as `connect-timeout-ms` and compression as `connect-content-encoding`, both renamed to their gRPC equivalents on the way in.

## Compression

Requests may use gzip, deflate, snappy, zstd or brotli (`br`), in any protocol. The layer in front decompresses every message itself, with the output capped one byte past the receive limit. Any other encoding is `UNIMPLEMENTED`, answered with the `grpc-accept-encoding` the server takes, and a compressed frame with no encoding declared is `INTERNAL`. Streamed replies of 1 KiB or more are compressed with the best codec the client accepts: zstd, then gzip, deflate and snappy.

## Unary calls skip the translation

A unary method is also reachable directly. When a unary call arrives in any of the three protocols, the layer decodes its one message, builds the `Context` from the headers, runs the handler inside the server's interceptor under the call's deadline, and writes the reply in that protocol itself: a Connect body or error JSON, a gRPC-Web body and trailer frame, or an HTTP/2 response with trailers. Streams go through the native server, with gRPC-Web and Connect translated to and from it.

## Every reply has a status

Every answer carries a gRPC status, including the ones that go wrong. Over gRPC and gRPC-Web, an unknown method is `UNIMPLEMENTED`, not a bare 404. Over Connect it's a 404 carrying Connect's `unimplemented` error, as that protocol specifies. An unsupported codec is 415 with an `Accept-Post` header. The reason is practical: a gRPC-Web client that gets a reply without trailers reports "potential CORS issue", which sends whoever is debugging a simple version mismatch off in the wrong direction.
