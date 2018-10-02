#!/bin/sh

set -e

BASE=$(cd $(dirname $0) && pwd)

PYENV_ROOT="$BASE/pyenv"

PYV35=3.5.6
PYV36=3.6.6

for v in $PYV35 $PYV36; do
	if [ ! -e "$PYENV_ROOT/versions/$v" ]; then
		./pyenv/bin/pyenv install "$v"
		"$PYENV_ROOT/versions/$v/bin/pip" install --upgrade pip
	fi
done

PY35="$PYENV_ROOT/versions/$PYV35/bin"
PY36="$PYENV_ROOT/versions/$PYV36/bin"

$PY35/pip install -r "$BASE/requirements35.txt"
$PY36/pip install -r "$BASE/requirements36.txt"

