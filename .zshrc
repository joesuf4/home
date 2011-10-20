HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history
setopt share_history extended_history hist_expire_dups_first hist_no_store prompt_subst
unsetopt unset
export LESSCHARSET=utf-8
export PAGER=less
export VISUAL=emacs
export MOZILLA=firefox

if echo $PATH | grep -v $HOME/bin >/dev/null; then
    PATH=$PATH:$HOME/bin
fi

# ctrl-(up/down/left/right) bindings
bindkey ';5A' history-search-backward
bindkey ';5B' history-search-forward
bindkey ';5C' emacs-forward-word
bindkey ';5D' emacs-backward-word

# directory stuff
nd () { export $1=$PWD; : ~$1 }
DIRSTACKSIZE=8
setopt autocd autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'
alias ldif_decode_base64='perl -MMIME::Base64 -ple '\''/^([\w.-]+):: (.*)/ and $_=qq($1: ) . decode_base64($2)'\'
alias asf_pw_driver='TERM=vt220 ~/src/apache/infra-trunk/machines/root/bin/apue/pty -d ~/src/apache/infra-trunk/machines/root/bin/apue/pw-driver.pl --'
alias wbs_pw_driver='TERM=vt220 ~/src/apache/infra-trunk/machines/root/bin/apue/pty -d ~/src/vdio/wbs/sysops/puppet/modules/mod_base/files/root/bin/apue/pw-driver.pl --'
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
            print -Pn "\ek%n@%m: "
            print -rn $1
            print -Pn " [%j]\e\\"

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

if [[ ${EMACS+} == t ]]; then
    unsetopt zle
    PROMPT=$'%n@%m:%~%(?..(%?%))%# '
    unset RPROMPT
elif [[ "`uname`" == "FreeBSD" ]]; then
    alias ls='ls -G'
    alias grep='grep --color=auto'
    PROMPT=$'$PR_BLACK%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))$PR_BLACK%#$PR_RESET '
elif [[ "`uname`" == "Linux" ]]; then
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    PROMPT=$'$PR_BLACK%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))%# '
elif [[ "`uname`" == "SunOS" ]]; then
    PROMPT=$'$PR_YELLOW%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))$PR_YELLOW%#$PR_RESET '
    PATH=$PATH:/opt/sfw/bin:/usr/sfw/bin:/opt/subversion-current/bin:/sbin:/usr/sbin:/usr/local/bin
    unset RPROMPT
fi

alias zfs >/dev/null && unalias zfs
alias zpool > /dev/null && unalias zpool

true

