#!/usr/local/bin/zsh

. ~joe/.zshenv
. ~joe/.zshrc

for region ad in ${(kv)OCI_AD}
do
    for id in {1..$ad}
    do
        ssh HA-fileserver-$id.$region true
    done
done
