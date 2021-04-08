(pty -nie -- pty -d pty-driver.pl -- sudo zsh -c 'apt update && apt upgrade' &)
sudo -k
