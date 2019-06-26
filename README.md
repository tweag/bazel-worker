# bazel-ghc-worker

A prototype implementation of a Bazel persistent worker wrapping GHC. Using GHC API for serving compilation requests from Bazel.

## Testing

```shell
cabal new-test
```

should show off a simple example of builnd `hello.hs` (project root).

## Building

Because `cabal new-build` still have rough corners, you have to install stuff manually before calling it. There are two steps.

First, get `proto-lens-protoc`:
```shell
cabal new-install proto-lens-protoc
```
(`~/.cabal/bin` should be in your path)


Second, [get `protoc`](https://google.github.io/proto-lens/installing-protoc.html).
