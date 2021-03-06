export LANG=en_US.UTF-8
export LESSCHARSET=utf-8
export PAGER=less
export VISUAL="emacs"
export MOZILLA=firefox
export EDITOR="emacs"

for p in /usr/sbin /sbin /usr/local/bin /usr/local/opt/python@3.8/bin \
         /usr/local/opt/findutils/libexec/gnubin /usr/local/opt/llvm/bin $HOME/bin
do
    PATH="$p$(echo ":$PATH" | /usr/bin/sed -e "s|:$p||g")"
done

: ${TERM:=xterm}
export TERM
