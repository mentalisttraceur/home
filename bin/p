#!/bin/sh -
if unused=`command -v termux-clipboard-get`
then
    exec termux-clipboard-get
fi
exec xclip -out -selection clipboard
