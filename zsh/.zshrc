HISTFILE=$XDG_STATE_HOME/zsh/history
HISTSIZE=1000
SAVEHIST=1000

setopt autocd
unsetopt beep

PS1="%(?..%B%?%b )%F{red}%n@%m%f %B%3~%b %# "

# Emacs Integration (with Eat)
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"

