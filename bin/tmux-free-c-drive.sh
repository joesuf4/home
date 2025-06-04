#!/bin/bash
df -h /mnt/c | (
  read -r _
  awk '$4 ~ /^[1-4]?[0-9]G/ {printf("☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠☠ ")}
   {print $4 "B"}'
)
