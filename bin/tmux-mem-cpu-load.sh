#!/usr/bin/zsh

rv="$(~/bin/tmux-mem-cpu-load "$@")"

if [[ "${rv##* }" -ge $(($(nproc)*2)) ]]; then
  c="fg=brightred,bg=terminal"
elif [[ "${rv##* }" -ge $(($(nproc))) ]]; then
  c="fg=brightmagenta,bg=terminal"
elif [[ "${rv##* }" -ge $(($(nproc)/2)) ]]; then
  c="fg=brightyellow,bg=terminal"
else
  c=""
fi

printf "%s%24ls\n" "#[$c]" "${${rv/  /🐏}/\%/%🏋}"
