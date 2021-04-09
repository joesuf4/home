export GREP_COLORS=fn=36
export LANG=en_US.UTF-8
export LESSCHARSET=utf-8
export PAGER=less
export VISUAL="emacs"
export MOZILLA=firefox
export EDITOR="emacs"
export DOCKER_COMPOSE_VERSION=1.25.4

for p in /usr/sbin /sbin /usr/local/bin $HOME/.local/bin $HOME/bin
do
    [ -d "$p" ] && PATH="$p$(echo ":$PATH" | /usr/bin/sed -e "s|:$p||g")"
done

: ${TERM:=xterm}
export TERM
