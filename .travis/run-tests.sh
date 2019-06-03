#!/usr/bin/env bash
set -evuo pipefail

mkdir -p /build
cd /build
cmake /src
make -j2
/build/libfive/test/libfive-test --use-colour yes
