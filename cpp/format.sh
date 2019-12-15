#!/bin/sh

BASE=$(cd $(dirname $0) && pwd)

cd "$BASE/build" && make clangformat
