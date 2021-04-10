# -*- sh -*-
cd
pkill ssh-agent
eval "$( (ssh-agent &) )"
pgrep -fu $USER pty-agent >/dev/null || pty-agent
pty -d pty-driver.pl -- sudo zsh -c 'mkdir -m 0777 -p /run/screen; pgrep gitlab-runner >/dev/null 2>&1 || gitlab-runner start'
pty -d pty-driver.pl ssh-add
