set -e
unset MOZILLA
if [[ "$(uname)" != Linux ]]; then
   sudo -k
   exit 0
fi
touch "$UPGRADE_LOGFILE"
chmod 0600 "$UPGRADE_LOGFILE"
(
  nohup pty -t 3 -nie -- timeout 300 pty -d pty-driver.pl -- flock -Fn "$UPGRADE_LOGFILE" $SHELL -ic '
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
