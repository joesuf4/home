# standard
export LANG=en_US.UTF-8
export LESSCHARSET=utf-8
export PAGER=less
export DOCKER_COMPOSE_VERSION=1.25.4
export COMPOSE_DOCKER_CLI_BUILD=0
export DOCKER_BUILDKIT=0
export MANPATH=/usr/local/share/man:/usr/share/man
export BPFTRACE_VMLINUX=~/src/bcscli/WSL2-Linux-Kernel/vmlinux
export KUBECTL_NODE_SHELL_IMAGE=artifactory.blackstone.com/docker/alpine:latest
export NODE_PATH=/usr/local/lib/node_modules
export KUBECONFIG

# WSL cyan for grep filenames
export GREP_COLORS=fn=36

# if you're avoiding a Win10 emacs server, leave this section alone
# otherwise you'll need to build symlinks from ~/bin/emacsclient to your Win10 emacs install location first.
for ed in EDITOR VISUAL; do
  [[ "$USER" == schaefj ]] && export $ed=emacsclient
done

# ptyd's URL engine relies on this setting
export MOZILLA="/mnt/c/Program Files/Google/Chrome/Application/chrome.exe"

# top_10() customizations
export ANSI_COLOR_ID=5
export HIST_BLOCK="■"
export COL=75

# report_node_inventory_static() (hi-speed) setting
export PLACEHOLDER=n/a

# don't save commands with SSL URLs on them, to keep `ptyd` from going aggressively bonkers on history completion
export HISTORY_FILTER_EXCLUDE=(secret SECRET pty-driver https://)
export HISTORY_FILTER_SILENT=

# Joe has a debugging libc build installed on his WSL Ubuntu, with a non-default TERMINFO setting
[[ "$USER" == schaefj ]] && export TERMINFO=/lib/terminfo

# asdf setup
. ~/.asdf/asdf.sh
. ~/.asdf/plugins/dotnet-core/set-dotnet-home.zsh

# PATH
for p in /sbin /usr/sbin /usr/local/bin "$(go env GOPATH)/bin" ~/{.local,.krew}/bin ~/.dotnet/tools ~/bin
do
  # fyi- this is not going to work if you use it on nested $p paths...
  [ -d "$p" ] && PATH="$p$(echo ":$PATH" | /usr/bin/sed -e "s|:$p||g")"
done

# KUBECONFIG
[[ -z "$KUBECONFIG" ]] && for f in ~/.kube/* ~/.kube/config
do
  [ -f "$f" ] && KUBECONFIG="$f$(echo "${KUBECONFIG:+:$KUBECONFIG}" | /usr/bin/sed -e "s|:$f||g")"
done
