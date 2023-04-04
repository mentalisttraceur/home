#!/bin/sh -
current_directory=`pwd` || return 2
cd / || return 2
case $1 in /*) file1=$1;; *) file1=$current_directory/$1; esac
case $2 in /*) file2=$2;; *) file2=$current_directory/$2; esac
shift 2
exec git diff "$@" "$file1" "$file2"
