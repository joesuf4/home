#!/usr/bin/zsh
# USAGE: $0 [$1]
# $1 - optional path to public key file (defaults to $HOME/.ssh/$SCAN_USER.pub)

SCAN_USER=gd-nessus-p-app
PUB_KEY="${1-$HOME/.ssh/$SCAN_USER.pub}"
SUDOERS_ADM=/etc/sudoers.d/adm

if ! [[ -f $PUB_KEY ]]; then
  echo "$0: Missing $SCAN_USER Public Key File!" >&2
  exit 1
fi

exec eks-run-global.sh nodes adduser addkey sudoers <<EOT
eks node-batch . 'getent passwd $SCAN_USER || sudo useradd -G adm -m -d /home/$SCAN_USER -s /bin/bash $SCAN_USER'
eks node-batch . 'sudo -u $SCAN_USER mkdir -p ~$SCAN_USER/.ssh && sudo -u $SCAN_USER bash -c "cat > ~$SCAN_USER/.ssh/authorized_keys"' <"$PUB_KEY"
eks node-batch . 'bash -c "echo %adm \"ALL=(ALL) NOPASSWD: ALL\" >$SUDOERS_ADM && chmod 0440 $SUDOERS_ADM"'
EOT
