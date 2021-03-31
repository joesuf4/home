# -*- sh -*-
cd
[ -d /run/screen ] || sudo mkdir -m 0777 /run/screen
pgrep pty-agent >/dev/null || pty-agent
bcs_assume_role apps-test bx_admin us-east-2 && pty_screen
