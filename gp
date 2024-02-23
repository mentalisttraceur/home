#!/bin/sh -
cleanup()
{
    case ${directory+x} in x)
        rm -rf "$directory"
    esac
}
output=${3-"$1"}
directory=`mktemp -d` &&
trap cleanup EXIT TERM INT HUP &&
(cd "$directory" && git init --quiet) &&
name=${1##*/} &&
cp "$1" "$directory/$name" &&
(cd "$directory" && git add "$name") &&
cp "$2" "$directory/$name" &&
(cd "$directory" && git add -p "$name") &&
(cd "$directory" && git checkout --quiet "$name") &&
cp "$directory/$name" "$output"
