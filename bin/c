#!/bin/sh -
if unused=`command -v termux-clipboard-set`
then
    exec termux-clipboard-set
fi
exec xclip -in -selection clipboard
