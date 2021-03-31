# -*- sh -*-
cd
[ -d /run/screen ] || sudo mkdir -m 0777 /run/screen
pgrep -fu $USER pty-agent >/dev/null || pty-agent
