#!/bin/sh

append=0
quiet=0

while getopts aq o; do
    case "$o" in
        a)append=1;;
        q)quiet=1;;
        ?)exit 1;;
    esac
done

while [ -n "$1" -a "`echo $1 | cut -d- -f1`" == "" ]; do
    shift
done

file=${1:-typescript}
cmd=${2:-"$SHELL"}
shift
shift

if [ $quiet -eq 0 ]; then
    echo Script started, file is $file
    if [ $append -eq 1 ]; then
        echo Script started on `date` >> $file
    else
        echo Script started on `date` > $file
    fi
fi

SCRIPT=$file pty -- $cmd $@ | tee -a $file

if [ $quiet -eq 0 ]; then
    echo                       >> $file
    echo Script done on `date` >> $file
    echo Script done, file is $file
fi
