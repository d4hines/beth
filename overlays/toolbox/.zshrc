HISTFILE="$HOME/.zsh_history"
mkdir -p "$(dirname "$HISTFILE")"

setopt HIST_FCNTL_LOCK
setopt HIST_IGNORE_DUPS
unsetopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
unsetopt HIST_EXPIRE_DUPS_FIRST
setopt SHARE_HISTORY
unsetopt EXTENDED_HISTORY

export HISTSIZE=1000000000
export HISTFILESIZE=1000000000

bindkey "^[OA" history-beginning-search-backward
bindkey "^[OB" history-beginning-search-forward

export PATH=~/.cargo/bin:~/.npm-global/bin:$PATH

eval "$($$$zoxide/bin/zoxide init zsh )"
eval "$($$$direnv/bin/direnv hook zsh)"

# Aliases
alias fzf_preview='fzf --preview "preview {}" --preview-window left:40%'
alias icat='kitty +kitten icat'
alias turn_off_warnings='export OCAMLPARAM="_,w=-27-26-32-33-20-21-37-34"'
alias watchexec='watchexec --shell='\''bash --login -O expand_aliases'\'''

alias anger='$HOME/repos/anger/result/bin/anger' # sloppy but IDK

#alias git="git -c 'include.path=$$$gitconfig/share/.gitconfig'"

alias gc="git commit -v"
alias gca="git commit --amend"

alias direnv="XDG_CONFIG_HOME=$$$direnv/share direnv"
alias gitui="XDG_CONFIG_HOME=$$$gituiconfig/share gitui"

# uhhhh I'm not sure if all these FO0=$FOO are needed
alias tmux="tmux -f $$$tmuxconfig/share/tmux.conf"

export ZSH="$$$oh-my-zsh/share/oh-my-zsh"

if [[ -e "$HOME/.zshextra" ]]; then
    source "$HOME/.zshextra"
fi

export ZSH_THEME="agnoster"
source $ZSH/oh-my-zsh.sh
