#!/usr/bin/zsh
# USAGE: $0 [$1]
# $1 - optional path to public key file (defaults to $HOME/.ssh/$SCAN_USER.pub)

export SCAN_USER=gd-nessus-p-app
export PUB_KEY="${1-$HOME/.ssh/$SCAN_USER.pub}"
if ! [[ -f $PUB_KEY ]]; then
  echo "$0: Missing $SCAN_USER Public Key File!" >&2
  exit 1
fi

exec run-global.sh nodes adduser addkey <<'EOT'
eks node-batch . "getent passwd $SCAN_USER || sudo useradd -G adm -m -d /home/$SCAN_USER -s /bin/bash $SCAN_USER"
eks node-batch . "sudo -u $SCAN_USER mkdir -p ~$SCAN_USER/.ssh && sudo -u $SCAN_USER bash -c \"cat > ~$SCAN_USER/.ssh/authorized_keys\"" <$PUB_KEY
EOT
