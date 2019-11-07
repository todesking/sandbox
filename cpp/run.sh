#!/bin/sh

BASE=$(cd $(dirname $0) && pwd)

"$BASE/make.sh" "$1" && "$BASE/build/a.out" 
ret=$?
echo
echo ^D to stop
cat - > /dev/null
exit $ret
