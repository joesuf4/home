#!/usr/bin/zsh -i
# USAGE: $0 [$1]
# $1 - optional path to public key file (defaults to $HOME/tmp/$SCAN_USER.pubkey)

export SCAN_USER=gd-nessus-p-app
export PUB_KEY="${1-$HOME/tmp/$SCAN_USER.pubkey}"
if ! [[ -f $PUB_KEY ]]; then
  echo "$0: Missing $SCAN_USER Public Key File!" >&2
  exit 1
fi

TMP="$(cat <<'EOT'
_eks_node_report_cmds=(adduser addkey)
alias _eks_report_node_adduser='eks node-batch . "getent passwd $SCAN_USER || sudo useradd -G adm -m -d /home/$SCAN_USER -s /bin/bash $SCAN_USER"'
alias _eks_report_node_addkey='eks node-batch . "sudo -u $SCAN_USER mkdir -p ~$SCAN_USER/.ssh && sudo -u $SCAN_USER bash -c \"cat > ~$SCAN_USER/.ssh/authorized_keys\"" <$PUB_KEY'
EOT
)"

for sig in EXIT TERM INT HUP; do
  trap 'cp ~/src/bcscli/eksrc ~/.eksrc' $sig
done

echo "$TMP" >> ~/.eksrc
eks report global nodes
