#!/usr/local/bin/zsh -i

for region ad in ${(kv)OCI_AD}
do
    for id in {1..$ad}
    do
        [[ -z "$slice" || $slice -eq $id ]] || continue
	ssh $OCI_HOST_PREFIX-$id.$region echo $region reconnected
    done
done

ssh 127.0.0.1 true
wait

