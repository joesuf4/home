set -e

if [[ "$(uname)" != Linux ]]; then
   sudo -k
else
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
    wait
    for d in ~/src/*; cd $d && git gc --aggressive &
    echo "UPGRADES COMPLETE(wait=$?)."
  ' >"$UPGRADE_LOGFILE" 2>&1 </dev/null &
  )
  for f in blogs www; [[ -d ~/src/$f ]] && cd ~/src/$f && git svn rebase && git push --force github trunk
  sudo -k
fi
