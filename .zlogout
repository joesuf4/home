(asdf update &)
(pty -nie -- pty -d pty-driver.pl -- sudo zsh -c 'gitlab-runner stop && apt update && apt upgrade' &)
sudo -k
