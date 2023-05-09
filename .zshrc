PS1='$ '
bindkey -v
source ~/.zsh-vi-ps1
source ~/.zsh-vi-search
source ~/.zsh-vi-P
source ~/.zsh-standard-keys
KEYTIMEOUT=1
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt sharehistory interactivecomments histignorealldups histignorespace
bindkey '^I' expand-or-complete-prefix
bindkey 'U' redo

case ${TERMUX_VERSION+x} in x)
    PATH=$PREFIX/local/bin:$PATH
    export XDG_RUNTIME_DIR=$PREFIX/var/run

    function m()
    {
        while sleep 0.1
        do
            p >~/.clipboard-fix-temp || return 1
            if test -s ~/.clipboard-fix-temp
            then
                cp ~/.clipboard-fix-temp ~/.clipboard-fix || return 1
            fi
        done
    }
esac
