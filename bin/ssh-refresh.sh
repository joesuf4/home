#!/usr/local/bin/zsh

. ~/.zshenv

slice=

for region ad in ${(kv)OCI_AD}
do
    for id in {1..$ad}
    do
        [[ -z "$slice" || $slice -eq $id ]] || continue
	timeout 10 pty -nd pty-driver.pl -- zsh -ic "ssh $OCI_HOST_PREFIX-$id.$region echo \$(hostname).$region connected." &
    done
done

pty -nd pty-driver.pl -- ssh 127.0.0.1 true
wait

