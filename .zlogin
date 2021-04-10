# -*- sh -*-
cd
pgrep -fu $USER pty-agent >/dev/null || pty-agent

export SSH_AGENT_PID="$(pgrep -fu $USER ssh-agent)"
if [ -n "$SSH_AGENT_PID" ]; then
    export SSH_AUTH_SOCK="$(ls -t /tmp/ssh-*/agent.* | head -n 1)"
else
    eval "$( (ssh-agent &) )"
fi
pty -d pty-driver.pl -- ssh-add
pty -d pty-driver.pl -- sudo $SHELL -c 'hwclock -s; mkdir -m 0777 -p /run/screen; pgrep gitlab-runner >/dev/null 2>&1 || gitlab-runner start'
