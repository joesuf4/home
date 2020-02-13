HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history
setopt share_history extended_history hist_expire_dups_first hist_no_store prompt_subst extendedglob
unsetopt unset
export LESSCHARSET=utf-8
export PAGER=less
export VISUAL="emacsclient -nw -c"
export MOZILLA=firefox
export EDITOR="emacsclient -nw -c"
export GIT_SSH_VARIANT=ssh
export SPR_ROOT=src/magicleap/spr_root
#export XAUTHORITY=/run/user/1401826122/gdm/Xauthority

# commonly used directories

orchestra=~/src/magicleap/spr_root/infrastructure/orchestration
ML2=~/builds-f2fs/magicleap/ML2
ML1=~/builds-ssd/magicleap/ML1
ML19=~/builds-ssd/magicleap/ML19


#if echo $PATH | grep -qv $HOME/bin; then
    PATH=$HOME/bin:$PATH
#fi


# ctrl-(up/down/left/right) bindings

if [[ "`uname`" == "Darwin" ]]; then
    bindkey '^[[A' history-search-backward
    bindkey '^[[B' history-search-forward
    bindkey '^[[C' emacs-forward-word
    bindkey '^[[D' emacs-backward-word
else
    bindkey ';5A' history-search-backward
    bindkey ';5B' history-search-forward
    bindkey '5A' history-search-backward
    bindkey '5B' history-search-forward
fi


bindkey ';5C' emacs-forward-word
bindkey ';5D' emacs-backward-word
bindkey '5C' emacs-forward-word
bindkey '5D' emacs-backward-word

# directory stuff
nd () { export $1=$PWD; : ~$1 }
DIRSTACKSIZE=8
setopt autocd autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'
alias ldif_decode_base64='perl -MMIME::Base64 -ple '\''/^([\w.-]+):: (.*)/ and $_=qq($1: ) . decode_base64($2)'\'
alias solaris_ldflags='perl -ple '\''s/-L(\S+)/-L$1 -R$1/g'\'

tplay () {
    perl -MPOSIX=ctermid -MTerm::ReadKey -e '
        open my $t, "+<", ctermid;
        ReadMode raw => $t;
        my $opt = shift eq "-s";
        while ($opt or ($_=ReadKey(0, $t)) ne "q") {
            if ($opt or $_ eq "s") { while(<>) { print and last if /[#%\$] / }}
            else { print scalar <> }
            last if eof
        }
        ReadMode restore => $t
    ' -- "$@"
}

autoload colors
colors

for COLOR in RED GREEN YELLOW WHITE BLACK CYAN BLUE MAGENTA; do
    eval PR_$COLOR='%{$fg[${(L)COLOR}]%}'
    eval PR_BRIGHT_$COLOR='%{$fg_bold[${(L)COLOR}]%}'
done
PR_RESET="%{${reset_color}%}";

title () {
    case $TERM in
        screen)
#            print -Pn "\ek%n@%m: "
            print -Pn "\ek$1\e\\"
#            print -Pn " [%j]\e\\"

            print -Pn "\e]0;%n@%m: "
            print -rn $1
            print -Pn " [%j]\a"
            ;;
        xterm*)
            print -Pn "\e]0;%n@%m: "
            print -rn $1
            print -Pn " [%j]\a"
            ;;

    esac
}

precmd () {
    title "zsh"

    if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
        zstyle ':vcs_info:*' formats "[${PR_BLACK}%b${PR_BRIGHT_GREEN}%c${PR_BRIGHT_YELLOW}%u${PR_RESET}]"
    } else {
        zstyle ':vcs_info:*' formats "[${PR_BLACK}%b${PR_BRIGHT_GREEN}%c${PR_BRIGHT_YELLOW}%u${PR_BRIGHT_RED}?${PR_RESET}]"
    }

    vcs_info 2>/dev/null
}
preexec () { title $2 }

autoload -U compinit
compinit

autoload -Uz vcs_info

zstyle ':vcs_info:*' stagedstr 'S'
zstyle ':vcs_info:*' unstagedstr 'M'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
zstyle ':vcs_info:*' enable svn git

RPROMPT='$vcs_info_msg_0_'

_ssh_zsh_config_hosts=($(grep '^Host' ~/.ssh/config 2>/dev/null | sed -e 's/^Host//'))

_ssh_zsh () {
    local state

    _arguments '1: :->ssh_host'

    case $state in
        (ssh_host) _arguments "1:ssh_hosts:($_ssh_zsh_config_hosts)" ;;
    esac
}

ssh_zsh () {
    /usr/bin/ssh -Y -t $@ -- zsh
}

compdef _ssh_zsh ssh_zsh

_ssh_host_completion () {
    local h
    h=($_ssh_zsh_config_hosts $(grep -v '^#' /etc/hosts | awk '{print $2}'))
    if [[ $#h -gt 0 ]]; then
        for x in ssh scp sftp rsync; do
            zstyle ":completion:*:$x:*" hosts $h
        done
    fi
}

_ssh_host_completion

if [[ ${EMACS+} == t ]]; then
    unsetopt zle
    PROMPT=$'%n@%m:%~%(?..(%?%))%# '
    unset RPROMPT
elif [[ "`uname`" == "FreeBSD" ]]; then
    alias ls='ls -G'
    alias grep='grep --color=auto'
    PROMPT=$'$PR_BLACK%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))$PR_BLACK%#$PR_RESET '
elif [[ "`uname`" == "Darwin" ]]; then
    alias ls='ls -G'
    alias grep='grep --color=auto'
    PROMPT=$'$PR_BLACK%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))$PR_BLACK%#$PR_RESET '
    PATH=/opt/local/bin:$PATH
elif [[ "`uname`" == "Linux" ]]; then
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    PROMPT=$'$PR_BLACK%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))%# '
elif [[ "`uname`" == "SunOS" ]]; then
    PROMPT=$'$PR_YELLOW%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))$PR_YELLOW%#$PR_RESET '
    PATH=$PATH:/opt/sfw/bin:/usr/sfw/bin:/sbin:/usr/sbin:/usr/local/bin:/usr/ccs/bin
fi

alias zfs >/dev/null && unalias zfs
alias zpool > /dev/null && unalias zpool

alias my_mplayer='mplayer tv:// driver=v4l2:device=/dev/video0:width=640:height=480 -vo jpeg'
alias flash_ml2_build='(cd ~/builds/magicleap/ML2/out/target/product/acamas && ./flashall_amd.sh)'
alias flash_ml1_build='(cd ~/builds-ssd/magicleap/ML1/out/target/product/phaedra && ./flashall.sh)'
alias flash_ml19_build='(cd ~/builds-ssd/magicleap/ML19/out/target/product/phaedra && ./flashall.sh)'
alias rev_hex32='perl -ple "s/([a-f\\d]{8})/join q(), reverse \$1 =~ m!..!g/ige"'
alias toggle='echo toggle'
alias git_ml_push='git push origin HEAD:refs/for/$(git branch --show-current)'
alias uart_console='screen cu --parity=none --speed=115200 --line=/dev/ttyUSB0'
alias emacsd='bash -c "exec emacs --daemon"'

emac () {
    local args=()
    local nw=false
    local running=false
    # check if emacsclient is already running
    pgrep -U $(id -u) emacsclient > /dev/null && running=true

    # check if the user wants TUI mode
    local arg;
    for arg; do
    	if [ "$arg" = "-nw" ] || [ "$arg" = "-t" ] || [ "$arg" = "--tty" ]
        then
            nw=true
    	fi
    done

    # if called without arguments - open a new gui instance
    if [ "$#" -eq "0" ] || [ "$running" != true ]; then
        args+=(-c) # open emacsclient in a new window
    fi
    if [ "$#" -gt "0" ]; then
        # if 'emac -' open standard input (e.g. pipe)
        if [[ "$1" == "-" ]]; then
    	    local TMP="$(mktemp /tmp/$0-stdin-XXXX)"
    	    cat >$TMP
	    args+=(--eval '(let ((b (generate-new-buffer "*stdin*"))) (switch-to-buffer b) (insert-file-contents "'${TMP}'") (delete-file "'${TMP}'"))')
        else
            args+=("$@")
        fi
    fi

    # emacsclient $args
    if $nw; then
	emacsclient "${args[@]}"
    else
        local display="MLLW3993:0.0"
        if [ "${DISPLAY:-}" != "localhost:10.0" ]; then
            display="$DISPLAY"
        fi
	(nohup emacsclient "${args[@]}" --display $display </dev/null >/dev/null 2>&1 &) > /dev/null 2>&1
    fi
}

true
