(asdf update --head && asdf plugin update --all &) >/dev/null 2>&1
(timeout 60 pd sudo $SHELL -c 'gitlab-runner stop && apt update && apt upgrade' &) >/dev/null 2>&1
sudo -k
