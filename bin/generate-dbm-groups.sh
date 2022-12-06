#!/usr/bin/zsh
declare -A group

for g in $(grep '^ *@' $1 | cut -d= -f1 | awk '{print $1}'); do
  : "${group[${g#@}]:=}"
done

for line in $(dbmmanage $2 view); do
  user=$(echo "$line" | cut -d: -f1 | tr -d '\n')
  groupline=$(echo "$line" | cut -d: -f3 | tr -d '\n')
  for g in $(echo "$groupline" | tr ',' ' ' | tr -d '\n'); do
    : "${group[$g]:=}"
    group[$g]+="$user,"
  done
done

echo "[groups]"
for k v in "${(@kv)group}"; do
  echo "$k = ${v%,}"
done
