# <i># Dotfiles</i>
## Current installation XMonad in Arch Linux

### # List of programs used so I don't forget
| Program name | Description |
| :------------- | :----------: |
| zsh ([oh-my-zsh](https://ohmyz.sh/)) | hands down best shell using [powerline10k (lean)](https://github.com/romkatv/powerlevel10k) |
| [picom](https://wiki.archlinux.org/index.php/Picom) | compositor for transparency |
| [alacritty](https://github.com/alacritty/alacritty) | terminal |
| lxappearance | change gtk theme, icon and cursor ([Arc theme](https://wiki.archlinux.org/index.php/GTK#Themes), [Papirus Dark](https://github.com/PapirusDevelopmentTeam/papirus-icon-theme#installation), [McMojave Cursor](https://www.gnome-look.org/p/1355701/)) |
| arandr | xrandr GUI to manage monitors |
| autorandr | set xrandr settings based on save profile |
| [nitrogen](https://wiki.archlinux.org/index.php/Nitrogen) | Wallpapers Dual monitor |
| [feh](https://wiki.archlinux.org/index.php/Feh) | image visualizer |
| thunar | file manager |
| ranger | terminal file manager |
| [scrot](https://wiki.archlinux.org/index.php/Screen_capture#scrot) | window print |
| [redshift](https://wiki.archlinux.org/index.php/Redshift) | night light |

### # Useful commands
| Command | Purpose |
| :------------- | :----------: |
| `setxkbmap -option compose:ralt` | set compose key to right alt |
| `chsh -s $(which zsh)` | set zsh as default shell |
| `xsetroot -cursor_name left_ptr` | remove X cursor |
| `xinput list` | view connected devices |
| `xinput list-props {MOUSE_ID}` | view details about specified device |
| `xinput --set-prop {MOUSE_ID} 'libinput Accel Speed' [-1, 1]` | change mouse speed |

### # Dependencies

qutebrowser -> [dracula theme](https://draculatheme.com/qutebrowser/)

vim -> [dracula theme](https://draculatheme.com/vim)

### # Links used to install xmonad and arch
Install arch: https://www.youtube.com/watch?v=UiYS8xWFXLY

Install xmonad: https://www.youtube.com/watch?v=JmPLbZQRgas

### # Fonts
Source: https://github.com/ryanoasis/nerd-fonts

[Mononoki Nerd Font](https://aur.archlinux.org/packages/nerd-fonts-mononoki/)

[DroidSansMono Nerd Font](https://aur.archlinux.org/packages/nerd-fonts-droid-sans-mono/)

[MesloLGLDZ Nerd Font](https://aur.archlinux.org/packages/nerd-fonts-meslo/)

Emojis: noto-fonts-emoji

Chinese, Japanese, etc: noto-fonts-cjk
