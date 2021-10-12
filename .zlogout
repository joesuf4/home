unset MOZILLA
(
  nohup pty -nie -- timeout 300 pty -d pty-driver.pl -- $SHELL -ic '
  setopt unset
  asdf update && asdf plugin update --all
  zplug update
  [[ "$USER" == schaefj ]] && sudo gitlab-runner stop
  agud -y
  npmu
  pip3u
  ' >/tmp/upgrades 2>&1 </dev/null &
)
sudo -k
