alias em "emacs -nw"
alias emw "emacs"

# interactive
alias cp "cp -i"
alias mv "mv -i"
alias rm "rm -i"

# navigation
alias marco "pwd"
alias polo "cd -P ."
alias cd.. "cd .."
alias .. "cd .."
alias ... "cd ../.."

# shortcuts
alias g "git"
alias o "open"
alias oo "open ."
alias gits "git s"

# http://defunkt.io/hub/
alias git hub

# enable aliases to be sudo'ed
alias sudo 'sudo '

function fish_user_key_bindings
         bind -k right nextd-or-forward-word
         bind -k left prevd-or-backward-word
end
