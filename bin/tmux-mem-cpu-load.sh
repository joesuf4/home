#!/usr/bin/zsh

rv="$(~/bin/tmux-mem-cpu-load "$@")"

if [[ "${rv##* }" -ge 30 ]]; then
  c="fg=brightred,bg=black"
elif [[ "${rv##* }" -ge 20 ]]; then
  c="fg=orange,bright,bg=black"
elif [[ "${rv##* }" -ge 10 ]]; then
  c="fg=brightyellow,bg=black"
else
  c=""
fi

echo "#[$c] ${${rv/  /🐏}/\%/%🏋}"
