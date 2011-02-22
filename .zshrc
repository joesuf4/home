HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history
setopt share_history extended_history hist_expire_dups_first hist_no_store prompt_subst
export LESSCHARSET=utf-8
export VISUAL=emacs
export MOZILLA=mozilla-firefox
export JAVA_HOME=~/java/jre1.5.0_06
export ANT_HOME=~/java/ant/apache-ant-1.6.1
SCREENCAP='SC|screen|VT 100/ANSI X3.64 virtual terminal:\
	:DO=\E[%dB:LE=\E[%dD:RI=\E[%dC:UP=\E[%dA:bs:bt=\E[Z:\
	:cd=\E[J:ce=\E[K:cl=\E[H\E[J:cm=\E[%i%d;%dH:ct=\E[3g:\
	:do=^J:nd=\E[C:pt:rc=\E8:rs=\Ec:sc=\E7:st=\EH:up=\EM:\
	:le=^H:bl=^G:cr=^M:it#8:ho=\E[H:nw=\EE:ta=^I:is=\E)0:\
	:li#24:co#80:am:xn:xv:LP:sr=\EM:al=\E[L:AL=\E[%dL:\
	:dl=\E[M:DL=\E[%dM:dc=\E[P:DC=\E[%dP:\
	:im=\E[4h:ei=\E[4l:mi:IC=\E[%d@:ks=\E[?1h\E=:\
	:ke=\E[?1l\E>:vi=\E[?25l:ve=\E[34h\E[?25h:vs=\E[34l:\
	:ti=\E[?1049h:te=\E[?1049l:us=\E[4m:ue=\E[24m:so=\E[3m:\
	:se=\E[23m:mb=\E[5m:md=\E[1m:mr=\E[7m:me=\E[m:ms:\
	:Co#8:pa#64:AF=\E[3%dm:AB=\E[4%dm:op=\E[39;49m:AX:\
	:vb=\Eg:G0:as=\E(0:ae=\E(B:\
	:ac=\140\140aaffggjjkkllmmnnooppqqrrssttuuvvwwxxyyzz{{||}}~~..--++,,hhII00:\
	:po=\E[5i:pf=\E[4i:k0=\E[10~:k1=\EOP:k2=\EOQ:k3=\EOR:\
	:k4=\EOS:k5=\E[15~:k6=\E[17~:k7=\E[18~:k8=\E[19~:\
	:k9=\E[20~:k;=\E[21~:F1=\E[23~:F2=\E[24~:F3=\EO2P:\
	:F4=\EO2Q:F5=\EO2R:F6=\EO2S:F7=\E[15;2~:F8=\E[17;2~:\
	:F9=\E[18;2~:FA=\E[19;2~:kb=:K2=\EOE:kB=\E[Z:\
	:*4=\E[3;2~:*7=\E[1;2F:#2=\E[1;2H:#3=\E[2;2~:#4=\E[1;2D:\
	:%c=\E[6;2~:%e=\E[5;2~:%i=\E[1;2C:kh=\E[1~:@1=\E[1~:\
	:kH=\E[4~:@7=\E[4~:kN=\E[6~:kP=\E[5~:kI=\E[2~:kD=\E[3~:\
	:ku=\EOA:kd=\EOB:kr=\EOC:kl=\EOD:km'

if echo $PATH | grep -v $HOME/bin >/dev/null; then
    PATH=$PATH:$HOME/bin
fi


# directory stuff
nd () { export $1=$PWD; : ~$1 }
DIRSTACKSIZE=8
setopt autocd autopushd pushdminus pushdsilent pushdtohome
alias dh='dirs -v'
alias screen='screen -U'
alias tplay='perl -MPOSIX=ctermid -MTerm::ReadKey -e '\''open my $t, "+<", ctermid; ReadMode raw => $t; my $opt = shift eq "-s"; while ($opt or ($_=ReadKey(0, $t)) ne "q") { if ($opt or $_ eq "s") { while(<>) { print and last if /[$#%] / }}  else { print scalar <> } last if eof } ReadMode restore => $t'\'' -- $1'
alias ldif_decode_base64='perl -MMIME::Base64 -ple '\''/^(\w+):: (.*)/ and $_=qq($1: ) . decode_base64($2)'\'
alias pw_driver='TERM=ansi SCREENCAP="$SCREENCAP" ~/src/apache/infra-trunk/machines/root/bin/apue/pty -d ~/src/apache/infra-trunk/machines/root/bin/apue/pw-driver.pl --'

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
            print -Pn "\ek%n@%m: $1 [%j]\e\\"

            print -Pn "\e]0;%n@%m: $1 [%j]\a"
            ;;
        xterm*)
            print -Pn "\e]0;%n@%m: $1 [%j]\a"
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

[[ $EMACS == t ]] && unsetopt zle

autoload -U compinit
compinit

autoload -Uz vcs_info
 
zstyle ':vcs_info:*' stagedstr 'S'
zstyle ':vcs_info:*' unstagedstr 'M'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
zstyle ':vcs_info:*' enable git svn

setopt prompt_subst
RPROMPT='$vcs_info_msg_0_'

if [[ $EMACS == t ]]; then
    unsetopt zle
    PROMPT=$'%n@%m:%~%(?..(%?%))%# '
    unset RPROMPT
elif [ "`uname`" = "FreeBSD" ]; then
    alias ls='ls -G'
    alias grep='grep --color=auto'
    PROMPT=$'$PR_BLACK%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))$PR_BLACK%#$PR_RESET '
elif [ "`uname`" = "Linux" ]; then
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    PROMPT=$'$PR_BLACK%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))%# '
elif [ "`uname`" = "SunOS" ]; then
    PROMPT=$'$PR_YELLOW%n@%m$PR_RESET:$PR_BLUE%~$PR_RESET%(?..($PR_RED%?$PR_RESET%))$PR_YELLOW%#$PR_RESET '
    PATH=$PATH:/opt/sfw/bin:/usr/sfw/bin:/opt/subversion-current/bin:/sbin:/usr/sbin
    unset RPROMPT
fi
