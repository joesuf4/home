#!/bin/bash

df -h /mnt/c | (
  read -r _
  [[ -t 2 ]] && awk '$4 ~ /^[1-4]?[0-9]G/ {printf("/mnt/c☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠ ");}
   {print $4 "B"}' || awk '$4 ~ /^[0-9]G/ {color="#[fg=red,bright]"}
   $4 ~ /^[1-4][0-9]G/ {color="#[fg=yellow,bright]";warning="/mnt/c☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠ "}
   $4 ~ /^[5-9][0-9]G/ {color="#[fg=white,bright]";warning="/mnt/c☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠ "}
   $4 ~ /^[1-4]?[0-9]G/ {printf("/mnt/c☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠ ")}
   {printf("%s%s%sB\n", color, warning, $4)} BEGIN{color="";warning=""}'
)
