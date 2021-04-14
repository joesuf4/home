(asdf update &)
(pty -i -d pty-driver.pl -- sudo $SHELL -c 'gitlab-runner stop && apt update && apt upgrade' &)
sudo -k
#pkill emacsclient
