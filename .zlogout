(asdf update && asdf plugin update --all &) >/dev/null 2>&1
(timeout 300 pty -d pty-driver.pl -- sudo $SHELL -c 'gitlab-runner stop && apt update && apt upgrade' &) >/dev/null 2>&1
sudo -k
