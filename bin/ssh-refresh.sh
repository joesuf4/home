#!/usr/bin/pfzsh

. ~/.zshenv

slice=
export SSH_AGENT_PID="$(pgrep -u $USER -f ssh-agent)"
export SSH_AUTH_SOCK="$(command ls -t /tmp/ssh-$USER/agent.* | head -n 1)"

for region ad in ${(kv)OCI_AD}
do
    for id in {1..$ad}
    do
        [[ -z "$slice" || $slice -eq $id ]] || continue
	(timeout 30 pty -nd pty-driver.pl -- ssh $OCI_HOST_PREFIX-$id.$region netstat -an '|' grep -F 127.0.0.1.4433 || (rm -f ~/.ssh/sockets/$USER@$OCI_HOST_PREFIX-$id.$region:22 ; timeout 20 pty -nie -- pty -nd pty-driver.pl -- ssh $OCI_HOST_PREFIX-$id.$region true)) 2>&1 &
    done
done

pty -nd pty-driver.pl -- ssh 127.0.0.1 true >/dev/null 2>&1
wait
