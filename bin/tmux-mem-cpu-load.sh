#!/usr/bin/zsh

rv="$(~/bin/tmux-mem-cpu-load "$@")"

if [[ "${rv##* }" -ge $(($(nproc)*2)) ]]; then
  c="fg=brightred,bg=black"
elif [[ "${rv##* }" -ge $(($(nproc))) ]]; then
  c="fg=orange,bright,bg=black"
elif [[ "${rv##* }" -ge $(($(nproc)/2)) ]]; then
  c="fg=brightyellow,bg=black"
else
  c=""
fi

printf "%s%24ls\n" "#[$c]" "${${rv/  /🐏}/\%/%🏋}"
