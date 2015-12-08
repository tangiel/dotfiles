# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Solarized dark with some fancy stuff.
ZSH_THEME="saiyr"

# Enable command auto-correction.
ENABLE_CORRECTION="true"

# Custom "custom" folder to make dotfiles work better.
ZSH_CUSTOM=$HOME/.oh-my-zsh-custom

# oh-my-zsh plugins.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Emacs!
export EDITOR=emacs

# Word movement that I'm more used to (I guess)
bindkey '\ef' emacs-forward-word
bindkey '\eb' emacs-backward-word

if [[ -d "${HOME}/.zsh" ]]; then
  for file in "${HOME}"/.zsh/*; do
    source $file
  done
fi
