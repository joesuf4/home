(asdf update && asdf plugin update --all &) >/dev/null 2>&1
(timeout 300 pty -d pty-driver.pl -- $SHELL -ic '[[ "$USER" == schaefj ]] && sudo gitlab-runner stop; sudo pip3 install -U; sudo -Es npm upgrade -g; agu >/dev/null && agug -yy >/dev/null'; setopt unset; zplug update &) >/dev/null 2>&1
sudo -k
