set -e
unset MOZILLA
touch "$UPGRADE_LOGFILE"
chmod 0600 "$UPGRADE_LOGFILE"
(
  nohup pty -t 3 -nie -- timeout 300 pty -d pty-driver.pl -- $SHELL -ic '
    echoon
    sudo -v
    asdfu &
    zplugu &
    gpgr &
    agu && sdexec apt dist-upgrade && agar &
    npmu &
    pip3u &
    krewu &
    wait
    echo "UPGRADES COMPLETE(wait=$?)."
  ' >"$UPGRADE_LOGFILE" 2>&1 </dev/null &
)
sudo -k
