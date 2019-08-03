#!/bin/sh

set -e

BASE=$(cd $(dirname $0) && pwd)
CXX=clang++

cd "$BASE/build" && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .. && make
