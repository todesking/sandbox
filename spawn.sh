#!/bin/sh

set -e

function print_usage() {
  cat <<EOS
Usage: $0 sub_dir_name new_repo_dir
EOS
}

function work() {
  subdir="$1"
  repo="$2"
  base=$(cd $(dirname "$0") && pwd)

  echo "New branch spawn_tmp will appear in $repo"

  git branch spawn_tmp
  git filter-branch -f --prune-empty --subdirectory-filter "$subdir" spawn_tmp
  git push "$repo" spawn_tmp
  git branch -D spawn_tmp
}

case "$#" in
  2) work "$1" "$2" ;;
  *) print_usage; exit 1 ;;
esac
