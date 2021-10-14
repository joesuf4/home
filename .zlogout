unset MOZILLA
(
  nohup pty -nie -- timeout 300 pty -d pty-driver.pl -- $SHELL -ic '
    asdfu
    zplugu
    [[ "$USER" == schaefj ]] && sudo gitlab-runner stop
    agud
    npmu
    pip3u
  ' >"$UPGRADE_LOGFILE" 2>&1 </dev/null &
)
sudo -k
