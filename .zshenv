export GREP_COLORS=fn=36
export LANG=en_US.UTF-8
export LESSCHARSET=utf-8
export PAGER=less
export VISUAL=emacsclient
export MOZILLA=chrome.exe
export EDITOR=emacsclient
export DOCKER_COMPOSE_VERSION=1.25.4
export COMPOSE_DOCKER_CLI_BUILD=0
export DOCKER_BUILDKIT=0
export MANPATH=/usr/local/share/man:/usr/share/man
export BPFTRACE_VMLINUX=~/src/bcscli/WSL2-Linux-Kernel/vmlinux
export NODE_PATH=/usr/local/lib/node_modules
export KUBECONFIG

. ~/.asdf/asdf.sh
. ~/.asdf/plugins/dotnet-core/set-dotnet-home.zsh

for p in /sbin /usr/sbin /usr/local/bin "$(go env GOPATH)/bin" ~/{.local,.krew}/bin ~/bin
do
  # fyi- this is not going to work if you use it on nested $p paths...
  [ -d "$p" ] && PATH="$p$(echo ":$PATH" | /usr/bin/sed -e "s|:$p||g")"
done

[[ -z "$KUBECONFIG" ]] && for f in ~/.kube/* ~/.kube/config
do
  [ -f "$f" ] && KUBECONFIG="$f$(echo "${KUBECONFIG:+:$KUBECONFIG}" | /usr/bin/sed -e "s|:$f||g")"
done

: "${TERM:=xterm}"
export TERM
