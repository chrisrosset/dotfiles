# ctr's zshrc file v0.1 based on:
# kcbanner's zshrc file v0.1, jdong's zshrc file v0.2.1 and mako's zshrc file v0.1

# {{{ Basic Setup

# source common shell settings
if [ -e ~/.shrc ]; then
    . ~/.shrc
else
    echo "WARN: Common shell setup file missing."
fi

ZSHDIR="$HOME/.zsh"
[ -d "$ZSHDIR" ] || mkdir "$ZSHDIR"

HISTFILE="$ZSHDIR/zhistory"
HISTSIZE=100000
SAVEHIST=100000
PAGER='less'
EDITOR="vim"

# stop backwards-delete-word at '=', '.', '/', '_' and '-' characters
WORDCHARS=$(echo "$WORDCHARS" | sed "s/[=./_-]//g")

# Say how long a command took, if it took more seconds than specified
REPORTTIME=10

# }}}
# {{{ Functions

function BackupZshHistoryFile()
{
    if [ ! -f "$HISTFILE" ]; then
        echo "The zhistory file does not exist."
        exit 1
    fi

    cp "$HISTFILE" "$HISTFILE-`date +%Y-%m-%d_%H-%M-%S`.bak"
}

# }}}
# {{{ Keybindings

if [[ "$TERM" != emacs ]]; then
[[ -z "$terminfo[kdch1]" ]] || bindkey -M emacs "$terminfo[kdch1]" delete-char
[[ -z "$terminfo[khome]" ]] || bindkey -M emacs "$terminfo[khome]" beginning-of-line
[[ -z "$terminfo[kend]" ]] || bindkey -M emacs "$terminfo[kend]" end-of-line
[[ -z "$terminfo[kich1]" ]] || bindkey -M emacs "$terminfo[kich1]" overwrite-mode
[[ -z "$terminfo[kdch1]" ]] || bindkey -M vicmd "$terminfo[kdch1]" vi-delete-char
[[ -z "$terminfo[khome]" ]] || bindkey -M vicmd "$terminfo[khome]" vi-beginning-of-line
[[ -z "$terminfo[kend]" ]] || bindkey -M vicmd "$terminfo[kend]" vi-end-of-line
[[ -z "$terminfo[kich1]" ]] || bindkey -M vicmd "$terminfo[kich1]" overwrite-mode

[[ -z "$terminfo[cuu1]" ]] || bindkey -M viins "$terminfo[cuu1]" vi-up-line-or-history
[[ -z "$terminfo[cuf1]" ]] || bindkey -M viins "$terminfo[cuf1]" vi-forward-char
[[ -z "$terminfo[kcuu1]" ]] || bindkey -M viins "$terminfo[kcuu1]" vi-up-line-or-history
[[ -z "$terminfo[kcud1]" ]] || bindkey -M viins "$terminfo[kcud1]" vi-down-line-or-history
[[ -z "$terminfo[kcuf1]" ]] || bindkey -M viins "$terminfo[kcuf1]" vi-forward-char
[[ -z "$terminfo[kcub1]" ]] || bindkey -M viins "$terminfo[kcub1]" vi-backward-char

# ncurses fogyatekos
[[ "$terminfo[kcuu1]" == "^[O"* ]] && bindkey -M viins "${terminfo[kcuu1]/O/[}" vi-up-line-or-history
[[ "$terminfo[kcud1]" == "^[O"* ]] && bindkey -M viins "${terminfo[kcud1]/O/[}" vi-down-line-or-history
[[ "$terminfo[kcuf1]" == "^[O"* ]] && bindkey -M viins "${terminfo[kcuf1]/O/[}" vi-forward-char
[[ "$terminfo[kcub1]" == "^[O"* ]] && bindkey -M viins "${terminfo[kcub1]/O/[}" vi-backward-char
[[ "$terminfo[khome]" == "^[O"* ]] && bindkey -M viins "${terminfo[khome]/O/[}" beginning-of-line
[[ "$terminfo[kend]" == "^[O"* ]] && bindkey -M viins "${terminfo[kend]/O/[}" end-of-line
[[ "$terminfo[khome]" == "^[O"* ]] && bindkey -M emacs "${terminfo[khome]/O/[}" beginning-of-line
[[ "$terminfo[kend]" == "^[O"* ]] && bindkey -M emacs "${terminfo[kend]/O/[}" end-of-line
fi

bindkey "^r" history-incremental-search-backward
bindkey "^f" history-incremental-search-forward
bindkey '^I' complete-word # complete on tab, leave expansion to _expand
bindkey -e

# }}}
# {{{ Shell Options

# changing directories
setopt autocd
setopt autopushd
setopt pushdminus
setopt pushdsilent
setopt pushdtohome

# completion
setopt autolist
unsetopt autoparamslash

# expansion and globbing
setopt extendedglob
setopt globdots
setopt list_ambiguous
setopt list_packed
setopt recexact

# history
setopt extendedhistory
setopt histignorespace
setopt histignoredups
setopt sharehistory
setopt incappendhistory

# input/output
setopt correct
setopt hash_cmds
setopt mailwarning
setopt rcquotes
setopt rm_star_wait # Demands confirmation after 'rm *' etc (waits 10s)

# job control
setopt autoresume
unsetopt bgnice
setopt checkjobs
setopt nohup
setopt longlistjobs
setopt notify

# }}}
# {{{ Shell Modules

# Autoload zsh modules when they are referenced
zmodload -a zsh/mapfile mapfile
zmodload -a zsh/stat stat
zmodload -a zsh/zprof zprof
zmodload -a zsh/zpty zpty

# }}}

autoload edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

# {{{ Global Variables

if [ -d "/opt/administration" ]; then
	source "/opt/administration/library.sh"
	TZ=`get_host_info 'TZ' 'Europe/London'`
fi


#LC_ALL='en_US.UTF-8'
#LANG='en_US.UTF-8'

#if [ $TERM = "xterm" ]; then
#	infocmp xterm-256color > /dev/null 2>&1
#	if [ $? ]; then
#		TERM=xterm-256color
#	fi
#elif [ $TERM = "screen" ]; then
#	infocmp screen-256color > /dev/null 2>&1
#	if [ $? ]; then
#		TERM=screen-256color
#	fi
#fi


# }}}
# {{{ Colors

autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi

PR_NO_COLOR="%{$terminfo[sgr0]%}"
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
    eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
    (( count = $count + 1 ))
done

# Fish style highlighting
if [ -f ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
	source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

# }}}
# {{{ Prompt

CPS1="[$PR_BLUE%n$PR_WHITE@$PR_GREEN%m%u$PR_NO_COLOR:$PR_RED%2c$PR_NO_COLOR]%(!.#.$) "
CRPS1="%(?..[%?])$PR_LIGHT_YELLOW(%D{%d/%m %H:%M})$PR_NO_COLOR"

case "$TERM" in
    "dumb")
        PS1="$ "
        unsetopt zle
        unsetopt prompt_cr
        unsetopt prompt_subst
        unfunction precmd
        unfunction preexec
        ;;
    "eterm")
        PS1="$CPS1"
        ;;
    "eterm-color")
        PS1="$CPS1"
        ;;
    *)
        PS1="$CPS1"
        #RPS1="$CRPS1"
        ;;
esac

# }}}
# {{{ Autocompletion Settings

autoload -U compinit
compinit

zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'

# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

# show autocomplete even when there's a match for the current input
# http://unix.stackexchange.com/questions/97692/make-zsh-show-autocomplete-choices-even-when-a-possible-match-is-entered
zstyle ':completion:*' accept-exact false

# allow one error for every three characters typed in approximate completer
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'

# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions

# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

## add colors to processes for kill completion
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:*:kill:*:processes' command 'ps --forest -A -o pid,user,cmd'
zstyle ':completion:*:processes-names' command 'ps axho command'

# New completion:
# 1. All /etc/hosts hostnames are in autocomplete
# 2. If you have a comment in /etc/hosts like #%foobar.domain,
# then foobar.domain will show up in autocomplete!
zstyle ':completion:*' hosts $(awk '/^[^#]/ {print $2 $3" "$4" "$5}' /etc/hosts | grep -v ip6- && grep "^#%" /etc/hosts | awk -F% '{print $2}')

# http://www.sourceguru.net/ssh-host-completion-zsh-stylee/
#zstyle -e ':completion::*:*:*:hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'
zstyle -e ':completion::*:*:*:hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm apache bin daemon games gdm halt ident junkbust lp mail mailnull \
        named news nfsnobody nobody nscd ntp operator pcap postgres radvd \
        rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs avahi-autoipd\
        avahi backup messagebus beagleindex debian-tor dhcp dnsmasq fetchmail\
        firebird gnats haldaemon hplip irc klog list man cupsys postfix\
        proxy syslog www-data mldonkey sys snort rtkit

# SSH Completion
#zstyle ':completion:*:scp:*' tag-order files users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
#zstyle ':completion:*:scp:*' group-order files all-files users hosts-domain hosts-host hosts-ipaddr
#zstyle ':completion:*:ssh:*' tag-order users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
#zstyle ':completion:*:ssh:*' group-order hosts-domain hosts-host users hosts-ipaddr
#zstyle '*' single-ignored show

# }}}

# vim: set foldmethod=marker:
