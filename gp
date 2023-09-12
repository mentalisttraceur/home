#!/bin/sh -
cleanup()
{
    case ${directory+x} in x)
        rm -rf "$directory"
    esac
}
directory=`mktemp -d` &&
trap cleanup TERM INT HUP &&
(cd "$directory" && git init --quiet) &&
cp "$1" "$directory"/contents &&
(cd "$directory" && git add contents) &&
cp "$2" "$directory"/contents &&
(cd "$directory" && git add -p contents) &&
(cd "$directory" && git checkout contents) &&
cp "$directory"/contents "$1" &&
rm -rf "$directory"
