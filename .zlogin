# -*- sh -*-
cd
[ -d /run/screen ] || sudo mkdir -m 0777 /run/screen
pgrep -fu $USER pty-agent >/dev/null || pty-agent
pty -d pty-driver.pl -- sudo gitlab-runner start
