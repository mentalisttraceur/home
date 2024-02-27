#!/bin/sh -
case ${EDITOR+x} in x)
    eval "exec $EDITOR" "$@"
esac
exec emacsclient --alternate-editor= --create-frame "$@"
