PS1='%(!.#.$) '
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
bindkey -M vicmd 'U' redo
bindkey -M vicmd 'H' vi-join
bindkey -M vicmd 'J' end-of-buffer
bindkey -M vicmd 'K' beginning-of-buffer

bindkey '^A' beginning-of-line
bindkey '^E' end-of-line
bindkey "^K" kill-line

case ${TERMUX_VERSION+x} in x)
    PATH=$PREFIX/local/bin:$PATH
    export XDG_RUNTIME_DIR=$PREFIX/var/run
esac
