# <i># Dotfiles</i>
## Current installation XMonad in Arch Linux

### # List of programs used so I don't forget
| Program name | Description |
| :--: | :-- |
| zsh ([oh-my-zsh](https://ohmyz.sh/)) | Hands down best shell using [powerline10k (lean)](https://github.com/romkatv/powerlevel10k) |
| [picom](https://wiki.archlinux.org/index.php/Picom) | Compositor for transparency |
| [alacritty](https://github.com/alacritty/alacritty) | Terminal |
| lxappearance | Change gtk theme, icon and cursor ([Arc theme](https://wiki.archlinux.org/index.php/GTK#Themes), [Papirus Dark](https://github.com/PapirusDevelopmentTeam/papirus-icon-theme#installation), [McMojave Cursor](https://www.gnome-look.org/p/1355701/)) |
| arandr | Xrandr GUI to manage monitors |
| autorandr | Set xrandr settings based on save profile |
| [nitrogen](https://wiki.archlinux.org/index.php/Nitrogen) | Wallpapers Dual monitor |
| [feh](https://wiki.archlinux.org/index.php/Feh) | Image visualizer |
| thunar | File manager |
| ranger | Terminal file manager |
| [scrot](https://wiki.archlinux.org/index.php/Screen_capture#scrot) | Window print |
| [redshift](https://wiki.archlinux.org/index.php/Redshift) | Night light |

### # Keyboard Shortcuts

##### Xmonad
| Key binding | Action |
| --: | :--: |
| <kbd>super</kbd>+<kbd>lctrl</kbd>+<kbd>r</kbd> | Recompile xmonad |
| <kbd>super</kbd>+<kbd>lshift</kbd>+<kbd>r</kbd> | Restart xmonad |
| <kbd>super</kbd>+<kbd>lshift</kbd>+<kbd>lalt</kbd>+<kbd>q</kbd> | Quit xmonad |

##### Windows
| Key binding | Action |
| --: | :--: |
| <kbd>super</kbd>+<kbd>lctrl</kbd>+<kbd>c</kbd> | Kill active window |
| <kbd>super</kbd>+<kbd>lshift</kbd>+<kbd>a</kbd> | Kill all windows in the active workspace |

##### Windows Navigation
| Key binding | Action |
| --: | :--: |
| <kbd>super</kbd>+<kbd>m</kbd> | Quit xmonad |
| <kbd>super</kbd>+<kbd>k</kbd> | Move focus to the next window |
| <kbd>super</kbd>+<kbd>j</kbd> | Move focus to the prev window |
| <kbd>super</kbd>+<kbd>shift</kbd>+<kbd>k</kbd> | Swap focused window with next window |
| <kbd>super</kbd>+<kbd>shift</kbd>+<kbd>j</kbd> | Swap focused window with prev window |
| <kbd>super</kbd>+<kbd>backspace</kbd> | Moves focused window to master |

##### Workspaces
| Key binding | Action |
| --: | :--: |
| <kbd>super</kbd>+<kbd>,</kbd> | Switch focus to prev monitor |
| <kbd>super</kbd>+<kbd>.</kbd> | Switch focus to next monitor |

##### System
| Key binding | Action |
| --: | :--: |
| <kbd>super</kbd>+<kbd>lshift</kbd>+<kbd>escape</kbd> | reboot |
| <kbd>super</kbd>+<kbd>escape</kbd> | shutdown |

##### System
| Key binding | Action |
| --: | :--: |
| <kbd>super</kbd>+<kbd>lshift</kbd>+<kbd>tab</kbd> | Rotate all windows except master |
| <kbd>super</kbd>+<kbd>lctrl</kbd>+<kbd>tab</kbd> | Rotate all windows including master |
| <kbd>super</kbd>+<kbd>kb_multiply</kbd> | Increase number of windows in master |
| <kbd>super</kbd>+<kbd>kb_divide</kbd> | Decrease number of windows in master |



##### Applications
| Key binding | Action |
| --: | :--: |
| <kbd>super</kbd>+<kbd>lalt</kbd>+<kbd>f</kbd> | Firefox |
| <kbd>super</kbd>+<kbd>lalt</kbd>+<kbd>e</kbd> | Thunar |
| <kbd>super</kbd>+<kbd>space</kbd> | Rofi aplications |
| <kbd>super</kbd>+<kbd>lctrl</kbd>+<kbd>space</kbd> | Rofi windows |
| <kbd>super</kbd>+<kbd>lalt</kbd>+<kbd>c</kbd> | Visual Studio Code |
| <kbd>super</kbd>+<kbd>lalt</kbd>+<kbd>a</kbd> | Keepassxc |
| <kbd>super</kbd>+<kbd>lalt</kbd>+<kbd>d</kbd> | Discord |

### # Useful commands
| Command | Purpose |
| :------------- | :----------: |
| `setxkbmap -option compose:ralt` | Set compose key to right alt |
| `chsh -s $(which zsh)` | Set zsh as default shell |
| `xsetroot -cursor_name left_ptr` | Remove X cursor |
| `xinput list` | View connected devices |
| `xinput list-props {MOUSE_ID}` | View details about specified device |
| `xinput --set-prop {MOUSE_ID} 'libinput Accel Speed' [-1, 1]` | Change mouse speed |

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

Icons: ttf-font-awesome
