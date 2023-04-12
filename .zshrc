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

case ${TERMUX_VERSION+x} in x)
    PATH=$PREFIX/local/bin:$PATH
esac
