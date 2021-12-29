#!/usr/bin/zsh -i

TMP="$(cat <<'EOT'
_eks_node_report_cmds=(adduser addkey)
export SCAN_USER=gd-nessus-p-app
alias _eks_report_node_adduser='eks node-batch . getent passwd $SCAN_USER || sudo useradd -G adm -m -d /home/$SCAN_USER -s /bin/bash $SCAN_USER | top_10'
alias _eks_report_node_addkey='cat ~/tmp/$SCAN_USER.pubkey | eks node-batch . "sudo -u $SCAN_USER mkdir -p ~$SCAN_USER/.ssh && sudo -u $SCAN_USER bash -c \"cat > ~$SCAN_USER/.ssh/authorized_keys\"" | top_10'
EOT
)"

echo "$TMP" >> ~/.eksrc
eks report global nodes -n 1000
cp ~/src/bcscli/eksrc ~/.eksrc
