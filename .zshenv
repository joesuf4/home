export LANG=en_US.UTF-8
export LESSCHARSET=utf-8
export PAGER=less
export VISUAL="emacs"
export MOZILLA=firefox
export EDITOR="emacs"
export DISPLAY="$(awk '/nameserver/ {print $2;exit}' /etc/resolv.conf):0.0"

: ${AWS_BATCH:=4} ${AWS_PROFILE:=honorlock}
export AWS_BATCH AWS_PROFILE
declare -A AWS_ID=($(ssm_honorlock.sh))

for p in /usr/ccs/bin /opt/sfw/bin /usr/sfw/bin /usr/sbin /sbin /usr/local/bin /usr/local/opt/python@3.8/bin \
         /usr/local/opt/findutils/libexec/gnubin /usr/local/opt/llvm/bin $HOME/bin
do
    PATH="$p$(echo ":$PATH" | /usr/bin/sed -e "s|:$p||g")"
done

: ${TERM:=xterm}
export TERM
