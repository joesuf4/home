#!/bin/sh
# SPDX License Identifier: Apache License 2.0

append=0
quiet=0

while getopts aq o; do
  case "$o" in
    a) append=1 ;;
    q) quiet=1 ;;
    ?) exit 1 ;;
  esac
done

while [ ! "$1" = "$(echo "$1" | sed -e 's/^-//')" ]; do
  shift
done

file=${1-typescript}
cmd=${2-$SHELL}
shift
[ $# -gt 0 ] && shift

if [ $quiet -eq 0 ]; then
  echo Script started, file is $file
fi
if [ $append -eq 1 ]; then
  echo Script started on $(date) >>$file
else
  echo Script started on $(date) >$file
fi

echo command: $cmd "$@" >>$file

SCRIPT=$file pty -- $cmd "$@" | tee -a $file

echo >>$file
echo "Script done on $(date)" >>$file
if [ $quiet -eq 0 ]; then
  echo Script done, file is $file
fi
echo
