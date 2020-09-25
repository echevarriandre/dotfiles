#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

(sleep 2; run $HOME/.config/polybar/launch.sh) &

#cursor active at boot
xsetroot -cursor_name left_ptr &
numlockx on &
picom -CG &

#starting user applications at boot time
nitrogen --restore &
xinput --set-prop $(xinput list | grep -w 'Logitech G403 Prodigy Gaming Mouse' | head -n 1 | awk '{print $8}' | cut -d'=' -f2) 'libinput Accel Speed' -0.65 &
redshift &
setxkbmap -option compose:ralt &
autorandr -c &