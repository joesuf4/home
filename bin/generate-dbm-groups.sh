#!/usr/bin/zsh
declare -A group

for g in $(grep '^ *@' "$1" | cut -d= -f1 | awk '{print $1}'); do
  : "${group[${g#@}]:=}"
done

for line in $(dbmmanage "$2" view | cut -d: -f1-3); do
  user="$(echo "$line" | cut -d: -f1)"
  groupline="$(echo "$line" | cut -d: -f3)"
  for g in $(echo "$groupline" | tr ',' '\n'); do
    : "${group[$g]:=}"
    group[$g]+="$user,"
  done
done

echo "[groups]"
for k v in "${(@kv)group}"; do
  echo "$k = ${v%,}"
done
