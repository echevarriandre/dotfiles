#
# ~/.bashrc
#

path+=($HOME/.local/bin)
path+=($HOME/.dotnet/tools)
export PATH

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
	  exec startx
fi

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
