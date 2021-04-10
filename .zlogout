(asdf update &)
(pty -i -d pty-driver.pl -- sudo zsh -c 'gitlab-runner stop && apt update && apt upgrade' &)
sudo -k
pkill ssh-agent
