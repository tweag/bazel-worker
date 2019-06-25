# bazel-worker

A prototype implementation of a Bazel persistent worker — essentially, a compiler-as-a-service, but without a compiler yet. The plan is to use (or extend) GHC as the compiler part.

## Building

Because `cabal new-build` still have rough corners, you have to install stuff manually before doing calling it. There are two steps.

First, get `proto-lens-protoc`:
```shell
cabal new-install proto-lens-protoc
```
(`~/.cabal/bin` should be in your path)


Second, [get `protoc`](https://google.github.io/proto-lens/installing-protoc.html).
