# .bashrc

# source common shell settings
[ -r ~/.shrc ] && . ~/.shrc

PS1="[\u@\h:\W]\\$ "

# http://www.gnu.org/software/bash/manual/html_node/The-Shopt-Builtin.html
shopt -s cdspell
shopt -s histappend

if CommandAvailable direnv; then
    eval "$(direnv hook bash)"
fi
