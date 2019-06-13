#!/bin/bash
# Can't use cabal b/c of its "Up to date" message polluting stdout
./dist-newstyle/build/x86_64-linux/ghc-8.6.4/worker-proto-0.1.0.0/build/worker-proto/worker-proto --persistent-worker
