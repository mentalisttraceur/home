#!/bin/sh -
file1=$1
file2=$2
shift 2
exec git diff --no-index "$@" "$file1" "$file2"
