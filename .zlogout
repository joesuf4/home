unset MOZILLA
(
  nohup pty -nie -- timeout 300 pty -d pty-driver.pl -- $SHELL -ic '
    setopt unset
    asdf update && asdf plugin update --all
    zplug update
    [[ "$USER" == schaefj ]] && sudo gitlab-runner stop
    agud
    npmu
    pip3u
  ' >"$UPGRADE_LOGFILE" 2>&1 </dev/null &
)
sudo -k
