# -*- sh -*-
cd
echoon

[[ $(~/bin/ttyname) =~ /dev/term/[ab] ]] && export TERM=vt100 USER=joe
export SSH_AGENT_PID="$(pgrep -u $USER -f ssh-agent)"

if [[ "$TERM" == vt100 || "$(uname)" == SunOS ]]; then
  if [[ -n "$SSH_AGENT_PID" ]]; then
    export SSH_AUTH_SOCK="$(command ls -t /tmp/ssh-$USER/agent.* | head -n 1)"
  else
    echo Initializing pty-agent...
    pty-agent
    emacs --daemon
    eval "$(mkdir -m 0700 -p /tmp/ssh-$USER && ssh-agent -a /tmp/ssh-$USER/agent.$$)"
    [[ -d ~/vault ]] && ptyd zfs mount tank/x1/home/joe/vault
    ptyd ssh-add
  fi
  reset

  if [[ "$(hostname)" =~ "^$OCI_HOST_PREFIX" ]]; then
    $SHELL
  else
    ptyd $SHELL
  fi

  exit $?
fi

if [[ -n "$SSH_AGENT_PID" ]]; then
  export SSH_AUTH_SOCK="$(command ls -t /tmp/ssh-$USER/agent.* | head -n 1)"
else
  pty-agent
  ptyd sudo zsh -c 'umount /tmp/.X11-unix;daemonize /usr/bin/unshare --fork --pid --mount-proc /lib/systemd/systemd --system-unit=basic.target;update-binfmts --disable cli;mkdir -p /run/user/1000/dconf;chown -R jschaefer:jschaefer /run/user/1000'
  sleep 3
  mkdir -m 0700 -p /tmp/ptyon-$USER
  ln -s -f /mnt/wslg/.X11-unix/X0 /tmp/.X11-unix/X0
  [[ -f /etc/wsl.conf ]] || ptyd sudo zsh -c "rm /etc/resolv.conf && cp /mnt/wsl/resolv.conf /etc"
  eval "$(mkdir -m 0700 -p /tmp/ssh-$USER && ssh-agent -a /tmp/ssh-$USER/agent.$$)"
  emacs --daemon
  ptyd ssh-add
  ptyd zsh -ic 'echo foo | gpg --clear-sign --armor >/dev/null 2>&1'
#  wsl.exe -d wsl-vpnkit --cd /app service wsl-vpnkit start
fi

reset
ptyd $SHELL
exit $?
