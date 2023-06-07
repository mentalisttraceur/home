#!/bin/sh -
cmd=/mnt/c/Windows/system32/cmd.exe
if command -v "$cmd" >/dev/null
then
    cd /mnt/c
    windows_home_directory=`"$cmd" /c echo '%UserProfile%' | tr -d '\r'`
    windows_home_directory=`wslpath "$windows_home_directory"`
    cd "$windows_home_directory"
    exec "$cmd" /c start "$1"
fi
exec xdg-open "$1"
