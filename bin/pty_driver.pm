#!/usr/bin/perl
#
# generic pty driver
#

use strict;
use warnings FATAL => 'all';

use POSIX qw/ttyname isatty :termios_h/;
use Term::ReadKey;
use IO::Select;
use IO::Socket::UNIX;
use File::Basename 'basename';


=head2 INTRODUCTION

SOME COMMON SENSE ADVICE: DO NOT RUN UNTRUSTED PROGRAMS, ANYWHERE, IF YOU
USE THIS WITH SHELLS/SCREEN!

STDIN and STDOUT are a socketpaired Unix Socket by a pty child process.
STDIN is read from the SLAVE terminal by that pty child and sent to us.
This represents the combined terminal output of the driven process.

STDOUT is written to the SLAVE terminal by that pty child.  This represents
terminal input for the driven process, typically through its own STDIN.

STDERR is attached to the MASTER terminal, since pty dup2's the controlling
pty process's STDIN to this process's STDERR.  Here's an example scripted
cron job that will run a shell session script:

0 23 * * * pty -d pty-driver.pl <session_script.sh -- bash

Be sure session_script.sh ends with an exit call to terminate the bash shell.

fd 3 is attached directly to the SLAVE terminal, which we inherited
from the parent pty process during our fork+exec. We take advantage of
fd 3 only when we check its ECHO status before driving any password prompts.

=head2 CONSTANTS

=cut

()=<<'=pod'; # copy following code into podlator

=pod

use constant MASTER_TTY_FD    => fileno STDERR;

use constant SLAVE_TTY_FD     => 3;

use constant BUFSIZE          => 4096;

use constant SOCKET_IO_TIMEOUT => 3;

use constant TTY_READKEY_TIMEOUT => 0.01;

use constant PTY_AGENT_SOCKET => "$ENV{HOME}/.pty-agent/socket";

=head2 INITIALIZATION

Intitialize pty-agent if necessary. pty-agent sticks around until reboot, and
is usually the only thing that turns up on a $(pgrep -u $USER python) cmd.
if that's hard to ferret out just use lsof on the socket as below.

=cut

system q(lsof ) . PTY_AGENT_SOCKET . q( >/dev/null 2>&1 || exec pty-agent);

=pod

Initialize rw connection to master terminal, if available.  If it's not
available then any calls to prompt() induced during the driving process will
cause the entire show to end.

=cut

my $mterm;

for ([\$mterm, MASTER_TTY_FD, sub {ReadMode "ultra-raw" => shift}], [\ my $sterm, SLAVE_TTY_FD]) {
    open ${$$_[0]}, "+<&=" . $$_[1]
        or die "Can't open $$_[1]: $!\n";
    isatty ${$$_[0]} or die "Not a tty!";
    $$_[2]->(${$$_[0]}) if @$_ > 2;
}

# Die cleanly if called for
$SIG{__DIE__} = sub {kill INT => getppid; exit 1 };

# pty's typical cleanup signal
$SIG{TERM} = sub { defined $mterm and ReadMode restore => $mterm; exit 255 };

# reset MASTER terminal (invoked on die() and normal exit(), not on signals)
END { defined $mterm and ReadMode restore => $mterm; }


=head2 HELPER FUNCTIONS


=item write_master (;$)

Defaults to writing $_ to the master terminal unless a lone argument is passed.
Silently returns false if SOCKET_IO_TIMEOUT is exceeded.

=cut

sub write_master (;$) {
    local ($_) = (@_, $_);
    my $blen = length or return;
    my $wrote = 0;
    local $@;
    eval {
        alarm SOCKET_IO_TIMEOUT;
        ReadMode "ultra-raw" => $mterm;
        do {
            my $w = syswrite $mterm, $_, $blen - $wrote, $wrote;
            die "syswrite failed: $!" unless $w >= 0;
            $wrote += $w;
        } while $wrote < $blen;
        alarm 0;
    };
    die $@ if $@;
    return $wrote;
}

=item write_slave (;$)

Defaults to writing $_ to the slave terminal unless a single argument is passed.
Will die if the SOCKET_IO_TIMEOUT is exceeded.

=cut

sub write_slave (;$) {
    local ($_) = (@_, $_);
    my $blen = length or return;
    my $wrote = 0;
    local $@;
    eval {
        alarm SOCKET_IO_TIMEOUT;
        do {
            my $w = syswrite STDOUT, $_, $blen - $wrote, $wrote;
            die "syswrite failed: $!" unless $w >= 0;
            $wrote += $w;
        } while $wrote < $blen;
        alarm 0;
    };
    $@ and die $@;
    return $wrote;
}

=item read_input_nb ($)

ReadKey in a (portable non-blocking) loop on the passed filehandle, to $_.  Returns length of $_.

=cut

sub read_input_nb ($) {
    my $r = shift;
    $_ = "";
    while (defined(my $c = ReadKey TTY_READKEY_TIMEOUT, $r)) {
        $_ .= $c;
    }
    return length;
}


=item prompt ($)

Prompt master terminal for a password of a given argument $type and return it.

=cut

sub prompt ($) {
    defined $mterm or die "Terminal not present for prompt()!\n";
    my $type = shift;
    # block these to avoid leaving tty in a non-echo state
    local $SIG{INT} = local $SIG{QUIT} = local $SIG{TSTP} = "IGNORE";

    write_master "\r\n$type Password (^D aborts): "; #aborting will terminate pty
    ReadMode noecho => $mterm;
    no warnings 'uninitialized';
    chomp(my $passwd = ReadLine 0, $mterm);
    write_master "\n";

    defined $passwd or die "Operation aborted\n";
    return $passwd;
}

# monkey-patch timeout wrapper around $socket method calls to pty-agent.

sub IO::Socket::UNIX::timed_call {
    my $obj = shift;
    my $method = shift;
    local $@; # avoid global pollution since our eval block otherwise will
    my @rv;
    my $list_context = wantarray; # eval changes wantarray, which we don't want.
    eval {
        alarm SOCKET_IO_TIMEOUT;
        @rv = $list_context ? $obj->$method(@_) : scalar $obj->$method(@_);
        alarm 0;
    };
    return $list_context ? @rv : $rv[0];
}

my %saw_pw;   # status flags by type to differentiate login success or
              # failure requiring a new prompt() by getpw()
my  %secret;  # non-agent mode of getpw() operation, shouldn't happen in reality.

=item sawpw ()

Takes no arguments.  Just exposes a reference to the internal %saw_pw hash for
outside management of that hash.  This has the effect of determing when getpw()
will prompt() for new credentials due to repetition in credential production.

=cut

sub sawpw { \%saw_pw }

# if you pass this sub a $prompt arg, be sure to manage sawpw()->{$type}
# yourself

=item getpw ($;$)

Takes a $type argument and an optional $prompt argument, which if true forces a
password prompt to acquire new creds for this $type.  Retrieves the password from
$(pty-agent) over its secure Unix domain socket, and returns it.

=cut

sub getpw ($;$) {
    my ($type, $prompt) = @_;
    index($type, ' ') >= 0
        and die "getpw(): invalid type '$type' contains a space char!\n";

    if (-S PTY_AGENT_SOCKET) {

        my $socket = IO::Socket::UNIX->new(
          Domain => AF_UNIX,
          Type => SOCK_STREAM,
          Peer => PTY_AGENT_SOCKET,
          ) or warn "Can't connect to pty-agent socket: $!\n"
              and goto NO_SOCKET;

        if ($prompt or $saw_pw{$type}++) {
            my $newvalue = prompt $type;
            $socket->timed_call(send => "SET $type $newvalue\n");
        }

        $socket->timed_call(send => "GET $type\n");
        my $reply = $socket->timed_call(getline => ());
        defined $reply and chomp $reply;

        if (not defined $reply or not length $reply) {
            # this implies pty-agent requires pw initialization or it timed out.

            goto &getpw; # induce a prompt this time since $saw_pw{$type} >= 1.
                         # also nicely ensures the $socket gets closed first to
                         # not hang pty-agent since it doesn't multiplex.
        }

        return $reply;
    }
    else {
      NO_SOCKET:
        $secret{$type} = prompt $type if $prompt or $saw_pw{$type}++
            or not $secret{$type};
        return $secret{$type};
    }
}

=item echo_enabled ($)

Takes an fd as argument, typically MASTER_PTY_FD or SLAVE_PTY_FD.
Returns true the terminal attached to that fd has echo enabled.

=cut

sub echo_enabled ($) {
    my $termios = POSIX::Termios->new;
    $termios->getattr(shift);
    return +(ECHO & $termios->getlflag) == ECHO;
}


# main:: globals for internal/external drive {} code blocks.

=back

=head2 GLOBAL VARIABLES

=cut

()=<<'=pod'; # copy following code into podlator

=pod

our $PREFIX_RE = qr/[\s\S]*/; # everything

our $NSM       = ''; # include this in regexps so as to not match themselves,
                     # e.g., for 'pty -d $0 -- $SHELL -c "cat $0"'


=head2 DRIVER CORE FUNCTION

=over 4

=item drive (&)

Loops forever - final sub to invoke in custom scripts.  Takes a code block
as argument, which should return true if the code block "handled" $_.

=back

=cut

sub drive (&) {
    my $custom_handler   = shift;
    my $s                = IO::Select->new(\*STDIN, $mterm // \*STDERR);
    my $ss               = IO::Select->new(\*STDIN);
    my $clear            = `clear`; # clears screen

    # toggle to deactivate automatic responses from this script when true
    my $disabled         = 0;
    # adjusts toggle input line, matching unsuffixed $0
    my $script_name      = basename $0, ".pl";

    local $_;

    while (my @readable = $s->can_read) {
        for my $r (@readable) {
            # a normal exit here can happen when the driven process shuts down.
            read_input_nb $r or exit;

            if ($r == \*STDIN) {
                write_master; # writes SLAVE output in $_ to MASTER terminal
                              # so we can see it.

                if (index($_, $clear) >= 0) { # the SLAVE is clearing the screen
                    read_input_nb $r and write_master;
                }
                elsif (/\btoggle $script_name\s*(on|off)?/) {
                    # alias toggle='true' in your shell's rc script to play nice
                    # otherwise the shell will attempt to run a toggle command
                    # as-is since we already sent this input to the SLAVE's
                    # terminal. this isn't required just good hygiene when
                    # working with it.
                    if ($1) {
                        $disabled = $1 eq "off" ? 1 : 0;
                    }
                    else {
                        $disabled = !$disabled;
                    }
                }
                elsif ($disabled) {
                    # prevent any further driver processing
                }
                elsif ($custom_handler->()) {
                    # handled by provided callback
                }
                elsif (/^$PREFIX_RE\S/m) {
                    # this should always be the final elsif block here...
                    # we saw something "printable" yet uninteresting to this
                    # script at the beginning of a line from the driven process.
                    %saw_pw = (); # not handled by anything seeking creds, so
                                  # we flush the consecutive prompt bookkeeeping
                }
            }
            else {
                # this is a MASTER terminal read (aka typical KB input on
                # $mterm), so send $_ to the SLAVE pty for terminal processing.
                write_slave;
            }
        }
    }
}

# check if called from do FILE or require FILE in another script.
# if so stop here and leave invocation of drive {} to caller, since
# we're just being included in perl4-ish library mode.

return 1 if scalar caller;

# Customization can happen in one of two ways: either by directly hacking on
# the drive {} code block below, or by requiring this script into another one
# and writing your own drive {} code block in that other script. The latter is
# the preferred approach if you want to avoid subsequent release installs from
# clobbering your mods to this one.

USER_SERVICEABLE_PARTS_BELOW_THIS_LINE:

drive {
    # this is the actual running program where user-customizable
    # code changes (to test $_) go.  Returns true if we handled
    # the line, false otherwise.

    if (m!\(yes/${NSM}no\)\?!g or /'yes' or ${NSM}'no'/) {
        # we always err on the side of caution,
        # but this can be customized differently.
        write_slave "no\n";
    }
    elsif (m!^$PREFIX_RE\QDo you want to continue? [Y/n]!m) {
        # accept the default for this apt-get prompt
        write_slave "\n";
    }
    else {
        return 0; # not handled by us
    }

    return 1; # handled successfully
}
