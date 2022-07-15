# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

DISABLE_MAGIC_FUNCTIONS=true
export ZSH="$HOME/.oh-my-zsh"
export EDITOR="nvim"

source $HOME/Programming/Git/zsh-git-prompt/zshrc.sh
PROMPT='%B%m%~%b$(git_super_status) %# '

ZSH_THEME="powerlevel10k/powerlevel10k"

plugins=( 
	git
	zsh-autosuggestions
)

source $ZSH/oh-my-zsh.sh

set -o vi

export PATH=/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/homebrew/bin:/opt/local/bin

function vi-yank-xclip {
    zle vi-yank
   echo "$CUTBUFFER" | pbcopy -i
}

zle -N vi-yank-xclip
bindkey -M vicmd 'y' vi-yank-xclip

alias v="CC=/opt/homebrew/bin/gcc-11 nvim"
alias python="python3"
alias ls="exa --icons"
alias dotfiles="/usr/bin/git --git-dir=$HOME/.dotfiles --work-tree=$HOME"
alias lg="lazygit"

source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.cargo/env

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
