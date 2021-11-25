unset MOZILLA
(
  nohup pty -nie -- timeout 300 pty -d pty-driver.pl -- $SHELL -ic '
    asdfu &
    zplugu &
    agud &
    agar &
    npmu &
    pip3u &
    gpgr &
    wait
  ' >"$UPGRADE_LOGFILE" 2>&1 </dev/null &
)
sudo -k
