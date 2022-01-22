unset MOZILLA
(
  nohup pty -nie -- timeout 300 pty -d pty-driver.pl -- $SHELL -ic '
    sudo -v
    asdfu &
    zplugu &
    gpgr &
    agud && agar &
    npmu &
    pip3u &
    krewu &
    wait
  ' >"$UPGRADE_LOGFILE" 2>&1 </dev/null &
)
sudo -k
