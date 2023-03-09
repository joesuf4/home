# -*- sh -*-
cd
echoon

if [[ "$(uname)" != Linux ]]; then
  ptyd $SHELL
  exit $?
fi

export SSH_AGENT_PID="$(pgrep -u $USER ssh-agent)"
if [[ -n "$SSH_AGENT_PID" ]]; then
  export SSH_AUTH_SOCK="$(ls -t /tmp/ssh-$USER/agent.* | head -n 1)"
else
  pty-agent
  ptyd sudo zsh -c '
umount /tmp/.X11-unix
daemonize /usr/bin/unshare --fork --pid --mount-proc /lib/systemd/systemd --system-unit=basic.target
update-binfmts --disable cli
'
  sleep 3
  mkdir -m 0700 -p /tmp/ptyon-$USER
  ln -s /mnt/wslg/.X11-unix/X0 /tmp/.X11-unix/X0
  [[ -f /etc/wsl.conf ]] || ptyd sudo zsh -c "rm /etc/resolv.conf && cp /mnt/wsl/resolv.conf /etc"
  eval "$(mkdir -m 0700 -p /tmp/ssh-$USER && ssh-agent -a /tmp/ssh-$USER/agent.$$)"
  ptyd ssh-add
  ptyd zsh -ic 'echo foo | gpg --clear-sign --armor >/dev/null 2>&1'
  wsl.exe -d wsl-vpnkit --cd /app service wsl-vpnkit start
fi

reset
ptyd $SHELL
exit $?
