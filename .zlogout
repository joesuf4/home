(asdf update &)
(timeout 60 pty -i -d pty-driver.pl -- sudo $SHELL -c 'gitlab-runner stop && apt update && apt upgrade' &)
sudo -k
