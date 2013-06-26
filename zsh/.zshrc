# Lines configured by zsh-newuser-install
HISTFILE=~/.histories/.zsh_history
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd beep notify
bindkey -e
# End of lines configured by zsh-newuser-install

# bind forward delete key
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char

# add completions and corrections
autoload -U compinit
compinit -d ~/.config/zsh/.zcompdump
setopt correct

# source config files
for file in ~/.config/zsh/.zsh_*
do
    [ -r "$file" ] && source "$file"
done

# load my functions
export FPATH=~/.config/zsh/functions:$FPATH
autoload -Uz catl fl md mva sw

