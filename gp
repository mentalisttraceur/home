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
cp "$1" "$directory"/contents &&
(cd "$directory" && git add contents) &&
cp "$2" "$directory"/contents &&
(cd "$directory" && git add -p contents) &&
(cd "$directory" && git checkout --quiet contents) &&
cp "$directory"/contents "$output"
