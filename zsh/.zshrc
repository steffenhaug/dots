HISTFILE=$HOME/.local/state/zsh/history
HISTSIZE=1000
SAVEHIST=1000
setopt autocd
unsetopt beep

# The following lines were added by compinstall
zstyle :compinstall filename '/home/st/.zshrc'
autoload -Uz compinit
compinit

# Starfish + Emacs Vterm Integration
eval "$(starship init zsh)"
source "/home/st/.config/zsh/emacs-vterm-zsh.sh"







# Added by ghcup
[ -f "/home/st/.ghcup/env" ] && source "/home/st/.ghcup/env"

# opam configuration
[[ ! -r /home/st/.opam/opam-init/init.zsh ]] || source /home/st/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null
