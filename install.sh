#!/bin/bash

DIR=~/code/dotfiles
OLD_DIR=~/code/dotfiles_old
FILES="emacs emacs.d oh-my-zsh oh-my-zsh-custom zsh zshrc"

mkdir -p $OLD_DIR

cd $DIR
for FILE in $FILES; do
    HOMEFILE=~/.$FILE
    if [ ! -L $HOMEFILE ]; then
        echo "Moving non-symlink $HOMEFILE to $OLD_DIR"
        rm -rf $OLD_DIR/.$FILE
        mv $HOMEFILE $OLD_DIR/
    else
        rm $HOMEFILE
    fi
    echo "Symlinked $DIR/.$FILE to $HOMEFILE."
    ln -s $DIR/.$FILE $HOMEFILE
done
