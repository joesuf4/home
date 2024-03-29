setopt unset

# standards
export LANG=en_US.UTF-8
export LESSCHARSET=utf-8
export PAGER=less
export MANPATH=/usr/local/share/man:/usr/share/man
export BPFTRACE_VMLINUX=~/src/WSL2-Linux-Kernel/vmlinux
export NODE_PATH=node_modules:~/.asdf/installs/nodejs/$(awk '$1 == "nodejs" {print $2}' ~/.tool-versions)/lib/node_modules
export NODE_OPTIONS=--use-openssl-ca
export MAIL="/var/spool/mail/$USER"
export KUBECONFIG

UPGRADE_LOGFILE="/tmp/upgrades-$USER"
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
PTYON=(sudo git svn gpg op ssh scp ansible-vault ortpasswd otp-sha1)

# top_10() customizations
ANSI_COLOR_ID=2
HIST_BLOCK="🐰"
HIST_ANCESTRY="🐰🍀🌷x✡♱☠❤❄◆▬■●▶"
COL=30

# report_node_inventory_static() (hi-speed) setting
PLACEHOLDER=n/a

# asdf setup
if [[ -d ~/.asdf ]] && ! command -v asdf >/dev/null 2>&1; then
  . ~/.asdf/asdf.sh
  . ~/.asdf/plugins/dotnet-core/set-dotnet-home.zsh >/dev/null 2>&1
fi

# PATH
for p in /sbin /usr/sbin "$(go env GOPATH 2>/dev/null)/bin" /usr/local/bin ~/{.local,.krew}/bin ~/.dotnet/tools ~/bin
do
  # fyi- this is not going to work if you use it on nested $p paths...
  [[ -d "$p" ]] && PATH="$p$(echo ":$PATH" | sed -e "s|:$p||g")"
done

# KUBECONFIG
[[ -z "$KUBECONFIG" ]] && for f in ~/.kube/* ~/.kube/config
do
  [[ -f "$f" ]] && KUBECONFIG="$f$(echo "${KUBECONFIG:+:$KUBECONFIG}" | sed -e "s|:$f||g")"
done

declare -A OCI_AD
# do not edit the next line manually!
OCI_AD=( )

OCI_SITE_SVCS=(http:apache24 markdownd svnwcsub watchdog)
OCI_HOST_PREFIX=HA-fileserver
OCI_ZPOOL=HApool

ZFS_TANK_EXPORTS=(tank/x1/cms tank/x1/svnpubsub tank/x1/httpd)
ZFS_RPOOL_EXPORTS=(rpool/usr/local rpool/etc/letsencrypt rpool/etc/mail rpool/etc/svc/manifest/site)
ZFS_EXPORTS=(${ZFS_TANK_EXPORTS[@]} ${ZFS_RPOOL_EXPORTS[@]})

PKG_REPOS=http://127.0.0.1:9999/
[[ -f ~/.cargo/env ]] && . ~/.cargo/env
: "${TERM:=xterm-256color}"
export GIT_SSL_NO_VERIFY=1
