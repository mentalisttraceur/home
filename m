#!/bin/bash -
escape=`printf '\e'`
while true
do
    if IFS= read -r -n 1 -t 0.1 character
    then
        case $character in q | "$escape")
            break
        esac
    else
        p >~/.clipboard-fix-temp || return 1
        if test -s ~/.clipboard-fix-temp
        then
            mv ~/.clipboard-fix-temp ~/.clipboard-fix || return 1
        fi
    fi
done
