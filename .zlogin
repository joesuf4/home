# -*- sh -*-
cd

echoon

export SSH_AGENT_PID="$(pgrep -u $USER ssh-agent)"
if [[ -n "$SSH_AGENT_PID" ]]; then
  export SSH_AUTH_SOCK="$(ls -t /tmp/ssh-*/agent.* | head -n 1)"
else
  ptyd sudo daemonize /usr/bin/unshare --fork --pid --mount-proc /lib/systemd/systemd --system-unit=basic.target
  ptyd sudo update-binfmts --disable cli
  eval "$(ssh-agent)"
  ptyd ssh-add
  seed_vault_pass
fi

reset
ptyd $SHELL
exit $?
