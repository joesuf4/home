export GREP_COLORS=fn=36
export LANG=en_US.UTF-8
export LESSCHARSET=utf-8
export PAGER=less
export VISUAL=emacs
export MOZILLA=chrome.exe
export EDITOR=emacs
export DOCKER_COMPOSE_VERSION=1.25.4

. ~/.asdf/asdf.sh
. ~/.asdf/plugins/dotnet-core/set-dotnet-home.zsh

for p in /sbin /usr/sbin /usr/local/bin "$(go env GOPATH)/bin" $HOME/.local/bin $HOME/bin
do
    [ -d "$p" ] && PATH="$p$(echo ":$PATH" | /usr/bin/sed -e "s|:$p||g")"
done

PATH="$(go env GOPATH)/bin:$PATH"

: ${TERM:=xterm}
export TERM
