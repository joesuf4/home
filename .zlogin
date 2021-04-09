# -*- sh -*-
cd
[ -d /run/screen ] || sudo mkdir -m 0777 /run/screen
pgrep -fu $USER pty-agent >/dev/null || pty-agent
pty -d pty-driver.pl -- sudo gitlab-runner start
eval "$( (ssh-agent &) )"
pty -d pty-driver.pl ssh-add
