# Code from: https://github.com/michaeljsmalley/dotfiles/blob/master/makesymlinks.sh
dir=~/dotfiles
olddir=~/dotfiles_old
files="zshrc config/i3/config config/kitty/kitty.conf compton.conf"

echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir -p $olddir

echo -n "Changing to the $dir directory ..."
cd $dir

for file in $files; do
    echo "Moving any existing dotfiles from ~ to $olddir"
    mv ~/.$file $olddir
    echo "Creating symlink to $file in home directory"
    ln -s $dir/$file ~/.$file
done

# Install:
# zsh and set zsh default with chsh -s $(which zsh)
# oh-my-zsh
# pure: https://github.com/sindresorhus/pure