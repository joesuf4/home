#!/bin/bash
grep SwapFree: /proc/meminfo |
  awk '$2 < 32*(1024**2) {color="#[fg=brightyellow,bg=black]"}
       $2 < 16*(1024**2) {color="#[fg=orange,bright,bg=black]"}
       $2 <  8*(1024**2) {color="#[fg=brightred,bg=black]"}
       {print color $2/1024**2 "GB"}'
