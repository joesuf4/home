#!/usr/bin/env perl
#
# pty driver customized for SYSTEM password and SSH passphrase prompt
# SPDX License Identifier: Apache License 2.0

use strict;
use warnings FATAL => 'all';
use IPC::Open2;

use lib "$ENV{HOME}/bin";
use pty_driver;

# keep 'use strict' happy (these are the two global vars we need
# from pty_driver.pm):

our $PREFIX_RE;
our $NSM;
my $htop;
drive {
    # this is the actual running program where user-customizable
    # code changes (to test $_) go.  Returns true if we handled
    # the contents of $_, false otherwise.

    if (m/^\s*\$/) {
        $htop++ and write_slave "exit\n"
            or do {
                write_slave "cd && sudo -E htop && bash\n";
                sleep 1;
                write_slave "\\php\n";
            };
    }
    elsif (m!\(yes/${NSM}no\)\?! or /'yes' or ${NSM}'no'/) {
        # we always err on the side of caution,
        # but this can be customized differently.
        write_slave "no\n";
    }
    elsif (/^$PREFIX_RE\Q(R)eject, accept (t)emporarily ${NSM}or accept (p)ermanently?/m) {
        # this is a typical ssh unkown-host-key prompt.
        # we do not want to be interrupted for automation,
        # but we also don't want to be connecting without manual
        # verification for our own protection, typically carried
        # out by toggling the driver off temporarily first.
        write_slave "r\n";
    }
    elsif (/^$PREFIX_RE\botp-md5 (\d+) (\w+)/m) {
        echo_enabled or do {
            my $pid = open2 my $out, my $in, "ortcalc $1 $2 2>&-";
            print $in getpw("OTP");
            close $in;
            write_slave <$out>;
            waitpid $pid, 0;
        };
    }
    elsif (/^$PREFIX_RE[Pp]assword(?: for $ENV{USER})?$NSM:/m) {
        # stop here unless echo is off to protect against driver replies
        # on otherwise matching reads or similar. Note: running a bash
        # login shell on a remote host over ssh will always disable
        # echo on the SLAVE terminal, so be careful that you actually
        # trust the foreign host's executables unless you use
        # zsh, which behaves appropriately.
        echo_enabled or write_slave getpw("SYSTEM") . "\n";
    }
    elsif (/^$PREFIX_RE(?:Enter passphrase for|Bad passphrase, try again for)$NSM /m) {
        echo_enabled or write_slave getpw("SSH") . "\n";
    }
    #
    # to extend the functionality of this script, add new elsif blocks
    # here to check for other types of login prompts you receive.
    # Choose the login $type (no spaces!) to mark things appropriately
    # in your getpw($type) call so it will be tracked properly
    # by pty-agent.
    #
    elsif (/^$PREFIX_RE\QSorry,$NSM try again./m) {
        # skip interceding sudo authentication error message
    }
    elsif (m!^$PREFIX_RE\QDo you want$NSM to continue? [Y/n]!m) {
        # accept the default for this apt-get prompt
        write_slave "\n";
    }
    else {
        return 0; # not handled by us
    }

    return 1; # handled successfully
}
