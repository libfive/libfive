#!/usr/bin/env bash
set -evuo pipefail

cd $(dirname $0)
docker build -t libfive:1 .
