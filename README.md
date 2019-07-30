# bazel-ghc-worker

A prototype implementation of a Bazel [persistent worker](https://blog.bazel.build/2015/12/10/java-workers.html) wrapping GHC.
It uses GHC API for serving compilation requests from Bazel.
Subject of merge into [`rules_haskell`](https://github.com/tweag/bazel-worker/) (now it's in the [`worker`](https://github.com/tweag/rules_haskell/tree/worker) branch).

## Performance

Current implementation does the simplest thing possible: it restarts GHC to serve every request; technically: every request implies a separate call to `runGhc`. Note that a request here means building one Bazel target, that is, a Haskell binary or Haskell library.

Even simple “full restart” approach brings 10–15% speedup on a certain simple benchmark project. We should do better though. There are several possible avenues for improvement.

### Idea 1: Caching (Parts of) GHC Startup

There should be a way to profit from the warm startup vs the cold one. One idea I've been pursuing specifically: cache loading package databases. It requires GHC to export some more internals in the API. Subject to a GHC ticket/proposal.

### Idea 2: Incremental Builds

A worker should be able to cache resulting artifacts. Later on, when it recieves a request to build an input with a known `digest` (hash), it should return the corresponding artifact straight away.

This idea implemented in several other workers (Swift, Kotlin, `rules_scala_anex`), but not in all (or majority) of them.

## Testing

```shell
cabal new-test
```

should show off a simple example of building `tests/hello.hs`.

## Building

Because `cabal new-build` still have rough corners, you have to install stuff manually before calling it. There are two steps.

First, get `proto-lens-protoc`:
```shell
cabal new-install proto-lens-protoc
```
(`~/.cabal/bin` should be in your path)


Second, [get `protoc`](https://google.github.io/proto-lens/installing-protoc.html).
