#!/bin/bash

if [[ -d /mnt/d ]]; then
  DRIVE=/mnt/d
else
  DRIVE=/mnt/c
fi
df -h $DRIVE | (
  read -r _
  [[ -t 2 ]] && awk '$4 ~ /^[1-4]?[0-9]([.][0-9])?G/ {printf("/mnt/c☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠ ");}
   {print $4 "B"}' || awk '$4 ~ /^[0-9]([.][0-9])?G/ {color="#[fg=brightred,bg=terminal]";warning="/mnt/c☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠ "}
   $4 ~ /^[1-4][0-9]([.][0-9])?G/ {color="#[fg=magenta,bg=terminal]";warning="/mnt/c☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠ "}
   $4 ~ /^[5-9][0-9]G/ {color="#[fg=brightyellow,bg=terminal]"}
   {printf("%s%s%sB\n", color, warning, $4)}'
)
