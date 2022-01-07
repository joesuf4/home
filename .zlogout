unset MOZILLA
(
  nohup pty -nie -- timeout 300 pty -d pty-driver.pl -- $SHELL -ic '
    asdfu &
    zplugu &
    gpgr &
    sudo -v
    agud && agar &
    npmu &
    pip3u &
    wait
  ' >"$UPGRADE_LOGFILE" 2>&1 </dev/null &
)
sudo -k
