#!/usr/local/bin/zsh
PATH=/usr/local/bin:/sbin:/usr/bin

ZULU=$(date -Iseconds | tr '+' 'Z')

for svn_repo in /x1/repos/svn/*
do
    LAST=$(ls -t $svn_repo/.zfs/snapshot | head -n 1)
    if [ -z "$LAST" ]
    then
        LAST=baseline
        zfs snapshot tank$svn_repo@$LAST
        TMPFILE=/x1/backups/svn/$(basename $svn_repo)-$LAST.zfs
        zfs send -rc tank$svn_repo@$LAST > $TMPFILE
        lzip $TMPFILE
        continue
    fi
    TMPFILE=/x1/backups/svn/$(basename $svn_repo)-$ZULU.zfs
    zfs snapshot tank$svn_repo@$ZULU
    zfs send -rci $LAST tank$svn_repo@$ZULU > $TMPFILE
    lzip $TMPFILE
    zfs destroy tank$svn_repo@$LAST
done
