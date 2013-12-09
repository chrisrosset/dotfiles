# rqg's zshrc file v0.1 based on:
# kcbanner's zshrc file v0.1, jdong's zshrc file v0.2.1 and mako's zshrc file v0.1

# Sections (these are searchable comments):
#
# keybindings
# setopts
# modules
# global variables
# colors
# prompt
# aliases
# completion

######################################## KEYBINDINGS (start)

#bindkey "\e[1~" beginning-of-line
#bindkey "e[4~" end-of-line
#bindkey "e[5~" beginning-of-history
#bindkey "e[6~" end-of-history
#bindkey "e[3~" delete-char
#bindkey "e[2~" quoted-insert
#bindkey "e[5C" forward-word
bindkey "5A" history-incremental-search-backward
bindkey "5B" history-incremental-search-forward
bindkey "5C" forward-word
bindkey -e '5C' emacs-forward-word
bindkey "5D" backward-word
bindkey -e '5D' emacs-backward-word
bindkey ";5A" history-incremental-search-backward
bindkey ";5B" history-incremental-search-forward
bindkey ";5C" forward-word
bindkey ";5D" backward-word

#bindkey "eOd" emacs-backward-word
#bindkey "ee[C" forward-word
#bindkey "ee[D" backward-word
#bindkey "OH" beginning-of-line

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

######################################## KEYBINDINGS (end)

######################################## SETOPTS (start)
# Set/unset shell options

setopt ALL_EXPORT # unset later in this file

# doesn't write commands starting with a space to history
setopt histignorespace

setopt histignoredups incappendhistory sharehistory extendedhistory
setopt notify globdots correct cdablevars autolist
setopt autocd recexact longlistjobs nohup
setopt autoresume list_ambiguous
setopt autopushd pushdminus pushdtohome pushdsilent
setopt extendedglob rcquotes mailwarning

# Demands confirmation after 'rm *' etc (waits 10s)
# Helps avoid mistakes like 'rm * o' when 'rm *.o' was intended
setopt RM_STAR_WAIT

unsetopt bgnice autoparamslash

######################################## SETOPTS (end)

######################################## MODULES (start)
# Autoload zsh modules when they are referenced

zmodload -a zsh/mapfile mapfile
zmodload -a zsh/stat stat
zmodload -a zsh/zprof zprof
zmodload -a zsh/zpty zpty

######################################## MODULES (end)

autoload edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

######################################## GLOBAL VARIABLES (start)

# add the personal administration scripts path
if [ -d "/opt/administration/scripts-general" ] ; then
	PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
	# only add it to PATH if it's not already there
	if [ -z "`echo $PATH | sed -e "s/:/\n/g" | grep "^$HOME/bin$"`" ]; then
		PATH="$HOME/bin:$PATH"
	fi
fi

# include user's cabal bin directory
cabal=$HOME/.cabal/bin
if [ -d "$cabal" ] ; then
	# only add it to PATH if it's not already there
	if [ -z "`echo $PATH | sed -e "s/:/\n/g" | grep "^$cabal$"`" ]; then
		PATH="$PATH:$cabal"
	fi
fi
unset cabal

if [ ! -d "$HOME"/.zsh ]; then
	mkdir "$HOME"/.zsh
fi

HISTFILE=$HOME/.zsh/.zhistory
HISTSIZE=100000
SAVEHIST=100000
HOSTNAME="`hostname`"

if [ -d "/opt/administration" ]; then
	source "/opt/administration/library.sh"

	TZ=`get_host_info 'TZ' 'Europe/London'`
fi


PAGER='less'
EDITOR="vim"
LC_ALL='en_US.UTF-8'
LANG='en_US.UTF-8'

if [ $TERM = "xterm" ]; then
	infocmp xterm-256color > /dev/null 2>&1
	if [ $? ]; then
		TERM=xterm-256color
	fi
elif [ $TERM = "screen" ]; then
	infocmp screen-256color > /dev/null 2>&1
	if [ $? ]; then
		TERM=screen-256color
	fi
fi

# stop backwards-delete-word at '=', '.', '/', '_' and '-' characters
WORDCHARS=$(echo "$WORDCHARS" | sed "s/[=./_-]//g")

# Say how long a command took, if it took more than 30 seconds
REPORTTIME=10

######################################## GLOBAL VARIABLES (end)

######################################## COLORS (start)

autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi


# Fish style highlighting
if [ -f ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
	source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fi

######################################## COLORS (end)

######################################## PROMPT (start)
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
    eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
    (( count = $count + 1 ))
done

PR_NO_COLOR="%{$terminfo[sgr0]%}"
PS1="[$PR_BLUE%n$PR_WHITE@$PR_GREEN%m%u$PR_NO_COLOR:$PR_RED%2c$PR_NO_COLOR]%(!.#.$) "
RPS1="%(?..[%?])$PR_LIGHT_YELLOW(%D{%d/%m %H:%M})$PR_NO_COLOR"

######################################## PROMPT (end)

unsetopt ALL_EXPORT

######################################## ALIASES (start)
if [[ $(uname) == "Linux" ]]; then
	LS_COLOR="--color=auto"
fi

alias cl="clear"
alias ls="ls $LS_COLOR"
alias l="ls $LS_COLOR"
alias ll="ls -lh $LS_COLOR"
alias lll="ls -lah $LS_COLOR"
alias ..='cd ..'
alias cd..='cd ..'
alias cal='cal -m'
alias nano='vim'
alias vi='vim'
alias cmus='singleton.sh cmus'
alias rtorrent='singleton.sh rtorrent'
alias cpr='rsync --archive --verbose --progress'

alias ga='git add'
alias gc='git commit'
alias gl='git log'
alias gs='git status'

######################################## ALIASES (end)

autoload -U compinit
compinit


######################################## COMPLETION (start)

zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'

# Completion Styles

# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate

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

######################################## COMPLETION (end)
