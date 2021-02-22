alias em "emacs -nw"
alias emw "emacs"

# interactive
alias cp "cp -i"
alias mv "mv -i"
alias rm "rm -i"

# navigation
alias marco "hostname -s | tr '\n' ' ' ; pwd"
alias polo "cd -P ."
alias cd.. "cd .."
alias .. "cd .."
alias ... "cd ../.."

# shortcuts
alias g "git"
alias o "open"
alias oo "open ."
alias gits "git s"

# enable aliases to be sudo'ed
alias sudo 'sudo '

function fish_user_key_bindings
         bind -k right nextd-or-forward-word
         bind -k left prevd-or-backward-word
end

# setup default colors
set -g fish_color_cwd f56dca
set -g fish_color_user blue
set -g fish_color_host blue

source ~/.extra.fish
