#!/usr/bin/env bash
set -evuo pipefail

export CLICOLOR_FORCE=1
mkdir -p /build
cd /build
cmake /src
make -j2
/build/libfive/test/libfive-test --use-colour yes
