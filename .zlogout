(asdf update && asdf plugin update --all &) >/dev/null 2>&1
(timeout 300 pty -d pty-driver.pl -- $SHELL -ic '[[ "$USER" == schaefj ]] && sudo gitlab-runner stop; agu >/dev/null && agug -yy >/dev/null' &) >/dev/null 2>&1
sudo -k
