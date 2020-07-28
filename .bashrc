#
# ~/.bashrc
#

PATH="$HOME/.local/bin${PATH:+:${PATH}}"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
	  exec startx
fi
