#!/usr/local/bin/zsh
PATH=/usr/local/bin:/sbin:/usr/bin

ZULU=$(date -Iseconds | tr '+' 'Z')

for svn_repo in /x1/repos/svn/*
do
    LAST=$(ls -t $svn_repo/.zfs/snapshot | head -n 1)
    zfs snapshot tank$svn_repo@$ZULU
    zfs send -i $LAST tank$svn_repo@$ZULU > /x1/repos/backups/$(basename $svn_repo)-$ZULU
    zfs destroy tank$svn_repo@$LAST
done
