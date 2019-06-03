#!/usr/bin/env bash
set -evuo pipefail

mkdir -p /root/libfive/build
cd /root/libfive/build
cmake ..
make -j2
./libfive/test/libfive-test
