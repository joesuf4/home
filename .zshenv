setopt unset

# standards
export LANG=en_US.UTF-8
export LESSCHARSET=utf-8
export PAGER=less
export DOCKER_COMPOSE_VERSION=1.25.4
export COMPOSE_DOCKER_CLI_BUILD=0
export DOCKER_BUILDKIT=0
export MANPATH=/usr/local/share/man:/usr/share/man
export BPFTRACE_VMLINUX=~/src/bcscli/WSL2-Linux-Kernel/vmlinux
export KUBECTL_NODE_SHELL_IMAGE=artifactory.blackstone.com/docker/alpine:latest
export NODE_PATH=~/.asdf/installs/nodejs/$(awk '$1 == "nodejs" {print $2}' ~/.tool-versions)/.npm/lib/node_modules
export NODE_OPTIONS=--use-openssl-ca
export MAIL=/var/spool/mail/$USER
export KUBECONFIG

UPGRADE_LOGFILE=/tmp/upgrades
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
HISTORY_FILTER_EXCLUDE=('(?i:secret|passw|_key)')
HISTORY_FILTER_SILENT=
DIRSTACKSIZE=8

# WSL cyan for grep filenames
export GREP_COLORS=fn=36

# if you're avoiding a Win10 emacs server, leave this section alone
# otherwise you'll need to build symlinks from ~/bin/emacsclient to your Win10 emacs install location first.
for ed in EDITOR VISUAL; do
  export $ed=vim
done

# ptyd's URL engine relies on this setting
export MOZILLA="/mnt/c/Program Files/Google/Chrome/Application/chrome.exe"
# whitelist of "password-sensitive" executables for enabling pty-driver.pl (within `sps` or just `ptyd zsh`)
PTYON=(sudo git gpg op ssh ansible-vault)

# top_10() customizations
ANSI_COLOR_ID=3
HIST_BLOCK="♱"
COL=68

# report_node_inventory_static() (hi-speed) setting
PLACEHOLDER=n/a

# asdf setup
. ~/.asdf/asdf.sh
. ~/.asdf/plugins/dotnet-core/set-dotnet-home.zsh >/dev/null 2>&1

# PATH
for p in /sbin /usr/sbin /usr/local/bin "$(go env GOPATH)/bin" ~/{.local,.krew}/bin ~/.dotnet/tools ~/bin
do
  # fyi- this is not going to work if you use it on nested $p paths...
  [[ -d "$p" ]] && PATH="$p$(echo ":$PATH" | sed -e "s|:$p||g")"
done

# KUBECONFIG
[[ -z "$KUBECONFIG" ]] && for f in ~/.kube/* ~/.kube/config
do
  [[ -f "$f" ]] && KUBECONFIG="$f$(echo "${KUBECONFIG:+:$KUBECONFIG}" | sed -e "s|:$f||g")"
done
