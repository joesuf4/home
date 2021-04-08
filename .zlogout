(pty -d pty-driver.pl -- sudo $SHELL -c 'apt update && apt upgrade' &)
sudo -k
