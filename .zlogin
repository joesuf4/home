# -*- sh -*-
cd

seed_vault_pass() {
  local TMP=$(mktemp)
  (
    touch $TMP;
    bcs assume-role devops-nonprod engineer >/dev/null &&
      PW="$(aws secretsmanager get-secret-value --secret-id service-deployer-ansible-vault-pass |
        jq .SecretString |
        tr -d \")"; printf "%s\n%s\n" $PW $PW | pty -nie -- pty -d pty-driver.pl ansible-vault encrypt $TMP
  )
  rm $TMP
}

export SSH_AGENT_PID="$(pgrep -u $USER ssh-agent)"
if [ -n "$SSH_AGENT_PID" ]; then
  export SSH_AUTH_SOCK="$(ls -t /tmp/ssh-*/agent.* | head -n 1)"
else
  seed_vault_pass
  eval "$( (ssh-agent &) )"
  ptyd ssh-add
fi

[[ "$USER" == schaefj ]] && ptyd sudo $SHELL -c 'mkdir -m 0777 -p /run/screen; pgrep gitlab-runner >/dev/null 2>&1 || gitlab-runner start'

[[ "$USER" == schaefj ]] && emac
