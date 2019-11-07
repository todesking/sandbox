#!/bin/sh

set -e

BASE=$(cd $(dirname $0) && pwd)
CXX=clang++

case "$1" in
    1) debug=1 ;;
    *) debug=0 ;;
esac

cd "$BASE/build" && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DLOCAL_DEBUG=${debug} .. && make clangformat && make
