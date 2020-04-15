# Home directory git repo

I use this primarily to distribute my home directory's essential
files across several servers.  The sources are found on my laptop, are
pushed to my github account, and from there are cloned to my home directory
across various hosts, typically with ansible.

Most of my worktime is spent in zsh running inside a screen session on
my laptop, and that's reflected in my config.  Obviously I use emacs
a lot, and the .emacs file contains a few essential elpa/melpa packages;
I couldn't get by without some of them.

The configs are messy and not really aimed at public distribution, but
I'd be happy to consider appropriate patches if there's something you'd
like to see me make use of.

The reusable scripts are over in the bin/ dir.  I have an "strace" drop-in
for my MBP for `sudo htop` to use for tracing processes. The only nontrivial
script is `bin/pffxg.sh` - a parallelized recursive grep - targeting *very*
large source trees typical for embedded development work.
