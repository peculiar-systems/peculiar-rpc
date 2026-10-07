---
title: JSON, REST and OpenAPI
section: Guide
order: 5
---

# JSON, REST and OpenAPI

One `.proto` file gives a service gRPC, gRPC-Web and Connect, a JSON codec on all three, a REST API and an OpenAPI 3.1 document. The handlers stay the same.

## JSON on every protocol

Every protocol takes JSON as well as binary protobuf. The server picks the codec from the content type: `application/json` and `application/connect+json` for Connect, `application/grpc+json` and `application/grpc-web+json` for the other two, and `encoding=json` for Connect GET. Replies use the codec the request used.

A client picks JSON through its options:

```haskell
let remote = Rpc.client Rpc.defaultOptions{Rpc.codec = Rpc.Json} transport :: EchoClient
```

The mapping is the canonical [proto3 JSON mapping](https://protobuf.dev/programming-guides/json/): lowerCamelCase names, 64-bit integers as strings, bytes as base64, enums by name, and the well-known types in their own forms (`Timestamp` as RFC 3339, `Duration` as `"1.5s"`, `FieldMask`, `Struct`, `Value`, the wrappers). It reads either field name, rejects duplicate keys, and limits nesting to 100 levels. A client ignores unknown fields, so an older client can read a newer server's replies.

An `Any` needs to know the types it may hold. The server registers every message its services reach. A client registers the well-known types by default, and takes more through `Options.registry`:

```haskell
Rpc.defaultOptions{Rpc.codec = Rpc.Json, Rpc.registry = Rpc.wellKnown <> Rpc.register (defMessage :: Book)}
```

`peculiar-rpc-json` holds the codec on its own: `toJson`, `fromJson`, `encodeJson` and `decodeJson` work on any proto-lens message.

## REST from google.api.http

A method with a [`google.api.http`](https://cloud.google.com/endpoints/docs/grpc/transcoding) annotation is served at its path too:

```proto
import "google/api/annotations.proto";

service Library {
  rpc GetBook(GetBookRequest) returns (Book) {
    option (google.api.http) = {get: "/v1/{name=shelves/*/books/*}"};
  }

  rpc CreateBook(CreateBookRequest) returns (Book) {
    option (google.api.http) = {
      post: "/v1/shelves/{shelf}/books"
      body: "book"
    };
  }

  rpc RenameBook(RenameBookRequest) returns (Book) {
    option (google.api.http) = {
      patch: "/v1/{name=shelves/*/books/*}:rename"
      body: "*"
    };
  }
}
```

```shell
curl localhost:8080/v1/shelves/poetry/books/odes
curl -X POST localhost:8080/v1/shelves/poetry/books -d '{"title": "Odes", "pages": 80}'
curl 'localhost:8080/v1/shelves/poetry/books?pageSize=10&tags=verse&tags=latin'
```

Nothing is registered by hand. `endpoint` reads the annotations from the service's embedded descriptor. `peculiar-rpc-apis` provides the generated `google.api` modules. Pass its protos to `protoc` alongside your own; the flake exposes them as `packages.api-protos`.

How a request is assembled:

- **Path variables** fill the fields they name, including nested ones (`{book.name}`), and `**` matches several segments. A variable is converted by its field's type.
- **The body** is the whole request with `body: "*"`, a single field with `body: "book"`, or nothing.
- **The query** fills every field the path and body don't cover. It takes scalars, enums and repeated fields, with dotted names for nested fields, under either the JSON or the proto name. A query value of the wrong type is 400.
- **`response_body`** narrows the reply to one field.
- **`additional_bindings`** are served too.

The reply is the response message as JSON. A failure is a `google.rpc.Status`, under the HTTP status Google maps its code to: the same table Connect uses, so `NOT_FOUND` is 404 and `INVALID_ARGUMENT` is 400.

```json
{ "code": 5, "message": "no book shelves/poetry/books/missing" }
```

Mounted with `Rpc.middleware`, a path that matches no binding goes on to your own application, and so does one that matches only under another verb, so your routes can share a path with a REST binding. Served alone with `Rpc.application`, a path that matches under another verb is 405 with an `Allow` header. REST routes and RPC paths share the port either way.

## OpenAPI

```haskell
Rpc.defaultCalls
  { Rpc.openApi = Just Rpc.Info{title = "Library", version = "1.0.0", description = Just "Books on shelves."}
  }
```

With `openApi` set, `GET /openapi.json` serves an OpenAPI 3.1 document for every annotated method the server holds. It is built from the same descriptors as the routes, so it can't drift from them:

- one tag per service, one operation per binding, with an `operationId` of `Service_Method`;
- path and query parameters, the request body and the response, typed as the JSON mapping writes them (`int64` as a string, `bytes` as base64, `Timestamp` as `date-time`);
- a schema per message and enum under `components`;
- descriptions from the comments in the `.proto` file;
- `google.api.field_behavior`: `REQUIRED` marks the property required, `OUTPUT_ONLY` makes it `readOnly` and `INPUT_ONLY` makes it `writeOnly`;
- [validation rules](/docs/validation) as schema constraints: lengths, bounds, item counts, `uniqueItems` and `format`;
- the error response as `google.rpc.Status`.

To build a reference site from it, and from the descriptors, see [documenting an API](/docs/documenting).
