# -*- sh -*-
cd

export SSH_AGENT_PID="$(pgrep -u $USER ssh-agent)"
if [ -n "$SSH_AGENT_PID" ]; then
  export SSH_AUTH_SOCK="$(ls -t /tmp/ssh-*/agent.* | head -n 1)"
else
  eval "$( (ssh-agent &) )"
  pd ssh-add
fi

pd sudo $SHELL -c 'mkdir -m 0777 -p /run/screen; pgrep gitlab-runner >/dev/null 2>&1 || gitlab-runner start'

emac
