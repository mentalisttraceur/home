#!/bin/sh -
if unused=`command -v batcat`
then
    exec /usr/bin/batcat --color=always "$@"
fi
exec /usr/bin/bat --color=always "$@"
