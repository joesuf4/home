export GREP_COLORS=fn=36
export LANG=en_US.UTF-8
export LESSCHARSET=utf-8
export PAGER=less
export VISUAL=emacsclient
export MOZILLA=chrome.exe
export EDITOR=emacsclient
export DOCKER_COMPOSE_VERSION=1.25.4
export COMPOSE_DOCKET_CLI_BUILD=0
export DOCKER_BUILDKIT=0
export BPFTRACE_NO_USER_SYMBOLS=0
export BPFTRACE_CACHE_USER_SYMBOLS=1

. ~/.asdf/asdf.sh
. ~/.asdf/plugins/dotnet-core/set-dotnet-home.zsh

for p in /sbin /usr/sbin /usr/local/bin "$(go env GOPATH)/bin" $HOME/.local/bin $HOME/bin
do
    [ -d "$p" ] && PATH="$p$(echo ":$PATH" | /usr/bin/sed -e "s|:$p||g")"
done

: ${TERM:=xterm}
export TERM
