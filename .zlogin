# -*- sh -*-
cd
[ -d /run/screen ] || sudo mkdir -m 0777 /run/screen
set_date
bcs_assume_role apps-test bx_admin us-east-2 && pty_screen
