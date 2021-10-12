sudo -k
(asdf update && asdf plugin update --all &) >/dev/null 2>&1
(setopt unset && zplug update &) >/dev/null 2>&1
(pty -ni -- timeout 300 pty -d pty-driver.pl -- $SHELL -ic '[[ "$USER" == schaefj ]] && sudo gitlab-runner stop; pip3u >/dev/null; npmu >/dev/null; agud -y >/dev/null'; date >> /tmp/upgrades &) >/dev/null 2>&1
