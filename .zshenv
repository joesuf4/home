export LANG=en_US.UTF-8
export LESSCHARSET=utf-8
export PAGER=less
export VISUAL="emacsclient -nw -c"
export MOZILLA=firefox
export EDITOR="emacsclient -nw -c"

for p in /usr/local/bin /usr/local/opt/python@3.8/bin /usr/local/opt/findutils/libexec/gnubin \
         /usr/local/opt/llvm/bin /usr/ccs/bin /opt/sfw/bin /usr/sfw/bin $HOME/bin
do
    PATH="$p:$(echo "$PATH" | /usr/bin/sed -e "s|:$p||g")"
done
