---
title: Health and reflection
section: Guide
order: 8
---

# Health and reflection

The two standard services operators and tools expect, ready to add to any server.

## Health

```haskell
main :: IO ()
main = do
  statuses <- Rpc.newStatuses
  Rpc.withServer Rpc.defaultCalls [Rpc.endpoint echo, Rpc.health statuses] \rpc ->
    Warp.run 8080 (Rpc.middleware rpc app)
```

This serves `grpc.health.v1.Health`. The empty service name stands for the server as a whole and starts out `SERVING`. Report on a single service with `Rpc.setServing statuses "example.v1.Echo" Rpc.NotServing`.

- `Check` answers the current status, or `NOT_FOUND` for a name it was never told about.
- `Watch` sends the current status, then each change as it happens. It waits in STM rather than polling, and reports a name it doesn't know as `SERVICE_UNKNOWN`.

Load balancers and orchestrators that speak the gRPC health protocol can use it as is, for example Kubernetes' `grpc` probes.

## Reflection

```haskell
Rpc.withServer calls (Rpc.withReflection [Rpc.endpoint echo, Rpc.health statuses]) \rpc -> ...
```

`withReflection` adds `grpc.reflection.v1`, and `v1alpha` for older tools, covering every service it's given and itself. With it, `grpcurl` works against the server with no `.proto` files at hand:

```shell
grpcurl -plaintext localhost:8080 list
grpcurl -plaintext localhost:8080 describe example.v1.SayRequest
grpcurl -plaintext -d '{"text": "hi"}' localhost:8080 example.v1.Echo/Say
```

> **Reflection publishes your schema**, comments included, to anyone who can reach the port. That's why it's something you add, not a default. On a public server, leave it off or serve it only where operators can reach it.

### Where the descriptors come from

Nothing is written down. proto-lens packs each `.proto` file's descriptor into every message it generates from that file. `deriveServer` collects every message type in the proto modules your service's module imports, following imports transitively, and the endpoint walks message-typed fields for anything further. Every file reached is served, source comments included. A service whose own file carries no messages gets that file synthesised from its service descriptor.

A file that declares only enums can't be derived: proto-lens gives it no message and doesn't export its descriptor. If a service imports one, pass protoc's own descriptor set to `withReflectionFrom`:

```shell
protoc --include_imports --descriptor_set_out=descriptors.pb -I proto example/v1/echo.proto
```

```haskell
descriptors <- ByteString.readFile "descriptors.pb"
Rpc.withServer calls (Rpc.withReflectionFrom [descriptors] [Rpc.endpoint echo]) \rpc -> ...
```

Files in a given set take precedence over the derived ones.
