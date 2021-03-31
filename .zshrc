HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history
setopt share_history extended_history hist_expire_dups_first hist_no_store \
       prompt_subst extendedglob
unsetopt unset


# ctrl-(up/down/left/right) bindings

if [[ "$(uname)" == "Darwin" ]]; then
    bindkey '^[[A' history-search-backward
    bindkey '^[[B' history-search-forward
fi
bindkey ';5A' history-search-backward
bindkey ';5B' history-search-forward
bindkey '5A' history-search-backward
bindkey '5B' history-search-forward
bindkey ';5C' emacs-forward-word
bindkey ';5D' emacs-backward-word
bindkey '5C' emacs-forward-word
bindkey '5D' emacs-backward-word


# directory stuff

nd () { export $1=$PWD; : ~$1 }
DIRSTACKSIZE=8
setopt autocd autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'


# typescript file walker

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


# color vars

autoload colors
colors

for COLOR in RED GREEN YELLOW WHITE BLACK CYAN BLUE MAGENTA; do
    eval PR_$COLOR='%{$fg[${(L)COLOR}]%}'
    eval PR_BRIGHT_$COLOR='%{$fg_bold[${(L)COLOR}]%}'
done
PR_RESET="%{${reset_color}%}";


# window titles

title () {
    setopt unset
    # screen title
    echo "$TERM" | grep -q "screen" || [[ "$(uname)" == "SunOS" ]] && print -Pn "\ek$1\e\\"
    # xterm title
    print -Pn "\e]0;%n@%m: "
    print -rn $1
    print -Pn " [%j]\a"
    unsetopt unset
}

precmd () {
    title "zsh"

    if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
        zstyle ':vcs_info:*' formats "[${PR_CYAN}%b${PR_BRIGHT_GREEN}%c${PR_BRIGHT_YELLOW}%u${PR_RESET}]"
    } else {
        zstyle ':vcs_info:*' formats "[${PR_CYAN}%b${PR_BRIGHT_GREEN}%c${PR_BRIGHT_YELLOW}%u${PR_BRIGHT_RED}?${PR_RESET}]"
    }

    vcs_info 2>/dev/null
}

preexec () { title $2 }


# VCS status RPROMPT

autoload -Uz vcs_info

zstyle ':vcs_info:*' stagedstr 'S'
zstyle ':vcs_info:*' unstagedstr 'M'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
zstyle ':vcs_info:*' enable svn git

RPROMPT='$vcs_info_msg_0_'


# custom tab-completion

autoload -U compinit
compinit

_bcs_account=($(grep -P '[\w-]+\s*\).*;;'  ~/.bcsrc | tr -dc 'a-z -' | sed -e 's/echo//g'))
_bcs_role=(bx_root bx_super bx_admin bx_readonly tfe)
_bcs_region=(us-east-1 us-east-2)

_bcs_assume_role () {
    local state

    _arguments '1: :->bcs_account' '2: :->bcs_role' '3: :->bcs_region'

    case $state in
        (bcs_account)
            _arguments "1:bcs_account:($_bcs_account)"
            ;;

        (bcs_role)
            _arguments "2:bsc_role:($_bcs_role)"
            ;;

        (bcs_region)
            _arguments "3:bsc_region:($_bcs_region)"
            ;;
    esac
}


function _ec2_reload () {
    _ec2_hosts=(${(k)EC2_ID})

    for fcn in ec2_list_inventory_filter ec2_push_ssh_public_key ec2_terminal_filter_exec ec2_batch_filter_remote_shell ec2_screen_filter_terminal_exec ec2_htop_ship_config_filter_bg; do
        eval "
          _$fcn () {
            local state;
            _arguments '1: :->ec2_host'
            case \$state in
                (ec2_host)
                    _arguments \"1:ec2_host:(\$_ec2_hosts)\";;
            esac
         }
         compdef _$fcn $fcn
         "
    done
}

compdef _bcs_assume_role bcs_assume_role

# various platform colorized prompts (and basic utils)

if [[ ${EMACS+} == t ]]; then
    unsetopt zle
    PROMPT=$'%n@%m:%~%(?..(%?%))%# '
    unset RPROMPT
else
    case "$(uname)" in

        FreeBSD|Darwin)
            alias ls='ls -G'
            alias grep='grep --color=auto'
            PROMPT=$'$PR_BLACK%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))$PR_BLACK%#$PR_RESET '
            ;;

        Linux)
            alias ls='ls --color=auto'
            alias grep='grep --color=auto'
            PROMPT=$'$PR_GREEN$BCS_PROFILE$PR_RESET:$PR_CYAN%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))%#$PR_RESET '
            ;;

        SunOS)
            alias ls='ls -F'
            alias grep='ggrep --color=auto'
            PROMPT=$'$PR_YELLOW%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))$PR_YELLOW%#$PR_RESET '
            ;;
        *)
            PROMPT=$'%n@%m:%~%(?..(%?%))%# '
            ;;
    esac
fi


# utilities

# translate between big-endian and little-endian objdumps.
alias rev_hex32='perl -ple "s/([a-f\\d]{8})/join q(), reverse \$1 =~ m!..!g/ige"'

alias gerrit_push='git push origin HEAD:refs/for/$(git branch --show-current)'

alias ldif_decode_base64='perl -MMIME::Base64 -ple '\''/^([\w.-]+):: (.*)/ and $_=qq($1: ) . decode_base64($2)'\'

alias solaris_ldflags='perl -ple '\''s/-L(\S+)/-L$1 -R$1/g'\'

alias htop='sudo -E htop'

alias pty_screen='pty -d pty-driver.pl ssh-agent screen'

alias top_10='perl -nale "END{ print \"\$_\\t\" . (\"x\" x ${LOG-}(\$h{\$_}/${DIVISOR-1}) . \" \$h{\$_}\" for sort {\$h{\$b} <=> \$h{\$a}} keys %h} \$h{\$F[0]} = \$F[1]" | head'

alias set_date='sudo date -s "$(date.exe)"'

# presumes a running emacs-server

emac () {
    local args; args=()
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
        args+=(-c) # open emacsclient in a new frame
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

    if $nw; then
	emacsclient "${args[@]}"
    else
	(nohup emacsclient "${args[@]}" </dev/null >/dev/null 2>&1 &) > /dev/null 2>&1
    fi
}

# aws

. /usr/share/zsh/vendor-completions/_awscli

. ~/.ec2rc

. ~/.bcsrc

# return

:
