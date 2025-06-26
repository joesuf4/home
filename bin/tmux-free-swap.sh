#!/bin/bash
grep SwapFree: /proc/meminfo |
  awk '$2 < 32*(1024**2) {color="#[fg=brightyellow,bg=terminal]"}
       $2 < 16*(1024**2) {color="#[fg=brightmagenta,bg=terminal]"}
       $2 <  8*(1024**2) {color="#[fg=brightred,bg=terminal]"}
       {printf("%s%dGB\n",color,$2/1024**2)}'
