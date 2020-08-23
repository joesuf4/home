#!/usr/local/bin/zsh

. ~joe/.zshenv

for region ad in ${(kv)OCI_AD}
do
    for id in {1..$ad}
    do
        ssh HA-fileserver-$id.$region true
    done
done

ssh 127.0.0.1 true
