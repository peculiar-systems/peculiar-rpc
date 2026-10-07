---
title: Validation and errors
section: Guide
order: 6
---

# Validation and errors

Requests are checked against [protovalidate](https://protovalidate.com) rules written in the `.proto` file, before a handler sees them.

```proto
import "buf/validate/validate.proto";

message Book {
  string title = 2 [(buf.validate.field).string = {min_len: 1, max_len: 200}];
  int64 pages = 3 [(buf.validate.field).int64 = {gte: 0, lte: 10000}];
  repeated string tags = 5 [(buf.validate.field).repeated = {
    max_items: 5
    items: {string: {min_len: 1}}
  }];
  string contact = 6 [
    (buf.validate.field).string.email = true,
    (buf.validate.field).ignore = IGNORE_IF_ZERO_VALUE
  ];
}

message CreateBookRequest {
  string shelf = 1;
  Book book = 2 [(buf.validate.field).required = true];
}
```

Nothing is switched on. The validator for each method is built from the request type's descriptor when the endpoint is made. It runs on every protocol and on each message of a client or bidi stream.

## A violation

A request that breaks a rule fails with `INVALID_ARGUMENT` and a [`google.rpc.BadRequest`](https://github.com/googleapis/googleapis/blob/master/google/rpc/error_details.proto) with one entry per broken rule. Each entry names the field by its path, such as `book.tags[1]`, gives the rule's id as its reason and protovalidate's description of what was broken. A rule written on a message names the field the message sits in, or none at the top level.

Every protocol carries the same status in its own form:

| Protocol | Status                  | Details                                                    |
| -------- | ----------------------- | ---------------------------------------------------------- |
| gRPC     | `grpc-status: 3`        | `grpc-status-details-bin`, a binary `google.rpc.Status`    |
| gRPC-Web | `grpc-status: 3`        | the same, in the trailer frame                             |
| Connect  | 400, `invalid_argument` | `details` in the error JSON, base64                        |
| REST     | 400                     | `details` in the `google.rpc.Status` JSON, with an `@type` |

```json
{
  "code": 3,
  "message": "invalid request: book.title: must be at least 1 characters",
  "details": [
    {
      "@type": "type.googleapis.com/google.rpc.BadRequest",
      "fieldViolations": [
        {
          "field": "book.title",
          "description": "must be at least 1 characters",
          "reason": "string.min_len"
        }
      ]
    }
  ]
}
```

Any `RpcError` a handler throws travels the same way, with its details. Over REST, details are rendered by their JSON mapping. That covers every `google.rpc` detail (`BadRequest`, `ErrorInfo`, `RetryInfo`, `QuotaFailure`, `PreconditionFailure`, `ResourceInfo`, `RequestInfo`, `Help`, `LocalizedMessage`, `DebugInfo`) and every message the server's services reach. A detail of any other type is left out of the REST reply; the RPC protocols carry it as bytes.

## What is checked

Every protovalidate rule, with protovalidate's semantics.

| Kind                  | Rules                                                                                                                                                                                                                                                                                  |
| --------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Every field           | `required`, `ignore`, `cel`, `cel_expression`                                                                                                                                                                                                                                          |
| Messages              | `cel`, `cel_expression`, `oneof`                                                                                                                                                                                                                                                       |
| Numbers               | `const`, `lt`, `lte`, `gt`, `gte`, `in`, `not_in`, and `finite` for floats                                                                                                                                                                                                             |
| Strings               | `const`, `len`, `min_len`, `max_len`, their `_bytes` forms, `pattern`, `prefix`, `suffix`, `contains`, `not_contains`, `in`, `not_in`                                                                                                                                                  |
| String formats        | `email`, `hostname`, `ip`, `ipv4`, `ipv6`, `uri`, `uri_ref`, `address`, `uuid`, `tuuid`, `ulid`, `ip_with_prefixlen`, `ipv4_with_prefixlen`, `ipv6_with_prefixlen`, `ip_prefix`, `ipv4_prefix`, `ipv6_prefix`, `host_and_port`, `protobuf_fqn`, `protobuf_dot_fqn`, `well_known_regex` |
| Bytes                 | `const`, `len`, `min_len`, `max_len`, `pattern`, `prefix`, `suffix`, `contains`, `in`, `not_in`, `ip`, `ipv4`, `ipv6`, `uuid`                                                                                                                                                          |
| Booleans and enums    | `const`, and `defined_only`, `in`, `not_in` for enums                                                                                                                                                                                                                                  |
| Repeated fields       | `min_items`, `max_items`, `unique`, and `items` for each element                                                                                                                                                                                                                       |
| Maps                  | `min_pairs`, `max_pairs`, `keys`, `values`                                                                                                                                                                                                                                             |
| Timestamps            | `const`, `lt`, `lte`, `gt`, `gte`, `lt_now`, `gt_now`, `within`                                                                                                                                                                                                                        |
| Durations             | `const`, `lt`, `lte`, `gt`, `gte`, `in`, `not_in`                                                                                                                                                                                                                                      |
| `Any` and field masks | `in`, `not_in`, and `const` for field masks                                                                                                                                                                                                                                            |
| Oneofs                | `required`                                                                                                                                                                                                                                                                             |
| Predefined rules      | rules of your own, declared with `(buf.validate.predefined).cel` on an extension of a rule message                                                                                                                                                                                     |

A proto3 field without `optional` is checked even when it holds its zero value. Add `ignore = IGNORE_IF_ZERO_VALUE` to leave an empty field alone.

`pattern` and `matches()` use RE2. CEL expressions are evaluated with CEL's standard library, its string extensions and protovalidate's functions (`isEmail`, `isHostname`, `isIp`, `isIpPrefix`, `isUri`, `isUriRef`, `isHostAndPort`, `isNan`, `isInf`, `unique`, `getField`). `lt_now`, `gt_now`, `within` and `now` in an expression compare against the server's clock when the message is checked.

```proto
message Book {
  option (buf.validate.message).cel = {
    id: "book.tags_within_pages"
    message: "a book cannot carry more tags than it has pages"
    expression: "this.pages == 0 || size(this.tags) <= this.pages"
  };

  int64 pages = 3;
  repeated string tags = 5;
  string isbn = 7 [(buf.validate.field).string.pattern = "^[0-9]{13}$"];
  google.protobuf.Timestamp due = 8 [(buf.validate.field).timestamp.gt_now = true];
}
```

protovalidate's conformance suite runs against the validator in `nix flake check`, every case passing.
