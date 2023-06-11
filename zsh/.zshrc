HISTFILE=$HOME/.local/state/zsh/history
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
unsetopt beep

# The following lines were added by compinstall
zstyle :compinstall filename '/home/st/.zshrc'
autoload -Uz compinit
compinit

# Better Vim mode
KEYTIMEOUT=1
bindkey -v
eval "$(starship init zsh)"

# Added by ghcup
[ -f "/home/st/.ghcup/env" ] && source "/home/st/.ghcup/env"
