# Path to your oh-my-zsh installation.
export ZSH="/home/neon/.oh-my-zsh"

ZSH_THEME=""

# CASE_SENSITIVE="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh

# https://github.com/sindresorhus/pure
fpath+=("$HOME/.oh-my-zsh/pure")
autoload -U promptinit; promptinit
prompt pure

# Remove annoying beep
unsetopt BEEP