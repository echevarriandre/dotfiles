# <i># Dotfiles</i>

### # List of programs used so I don't forget

| Program name                                                       |                                                                                                                                                                                                                                                       |
| :----------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: |
| zsh ([pretzo](https://github.com/sorin-ionescu/prezto))                               |                                                                                                                                                           Hands down best shell using [powerline10k (lean)](https://github.com/romkatv/powerlevel10k) |
| [picom](https://wiki.archlinux.org/index.php/Picom)                |                                                                                                                                                                                                                           Compositor for transparency |
| [alacritty](https://github.com/alacritty/alacritty)                |                                                                                                                                                                                                                                              Terminal |
| lxappearance                                                       | Change gtk theme, icon and cursor ([Ant Dracula theme](https://www.gnome-look.org/p/1099856), [Papirus Icon](https://github.com/PapirusDevelopmentTeam/papirus-icon-theme#installation), [McMojave Cursor](https://www.gnome-look.org/p/1355701/), [Capitaine Cursors](https://www.gnome-look.org/p/1148692)) |
| arandr                                                             |                                                                                                                                                                                                                         Xrandr GUI to manage monitors |
| autorandr                                                          |                                                                                                                                                                                                             Set xrandr settings based on save profile |
| [nitrogen](https://wiki.archlinux.org/index.php/Nitrogen)          |                                                                                                                                                                                                                               Wallpapers Dual monitor |
| [feh](https://wiki.archlinux.org/index.php/Feh)                    |                                                                                                                                                                                                                                      Image visualizer |
| thunar                                                             |                                                                                                                                                                                                                                          File manager |
| ranger                                                             |                                                                                                                                                                                                                                 Terminal file manager |
| [scrot](https://wiki.archlinux.org/index.php/Screen_capture#scrot) |                                                                                                                                                                                                                                          Window print |
| [redshift](https://wiki.archlinux.org/index.php/Redshift)          |                                                                                                                                                                                                                                           Night light |
| numlockx                                                           |                                                                                                                                                                                                                              Activate numlock on boot |

### # Useful commands

| Command                                                       |               Purpose               |
| :------------------------------------------------------------ | :---------------------------------: |
| `setxkbmap -option compose:ralt`                              |    Set compose key to right alt     |
| `chsh -s $(which zsh)`                                        |      Set zsh as default shell       |
| `xsetroot -cursor_name left_ptr`                              |           Remove X cursor           |
| `xinput list`                                                 |       View connected devices        |
| `xinput list-props {MOUSE_ID}`                                | View details about specified device |
| `xinput --set-prop {MOUSE_ID} 'libinput Accel Speed' [-1, 1]` |         Change mouse speed          |

### # Dependencies

qutebrowser -> [dracula theme](https://draculatheme.com/qutebrowser/)

vim -> [dracula theme](https://draculatheme.com/vim)

### # Fonts

Source: https://github.com/ryanoasis/nerd-fonts

[Mononoki Nerd Font](https://aur.archlinux.org/packages/nerd-fonts-mononoki/)

Emojis: noto-fonts-emoji

Chinese, Japanese, etc: noto-fonts-cjk

Icons: ttf-font-awesome
