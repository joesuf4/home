#!/usr/bin/env perl
#
# generic pty driver
# SPDX License Identifier: Apache License 2.0

use strict;
use warnings FATAL => 'all';

use POSIX qw/ttyname isatty :termios_h _exit/;
use Term::ReadKey;
use IO::Select;
use IO::Socket::UNIX;
use File::Basename 'basename';
use Fcntl qw/O_NONBLOCK F_SETFL F_GETFL/;

=head2 INTRODUCTION

SOME COMMON SENSE ADVICE: DO NOT RUN UNTRUSTED PROGRAMS, ANYWHERE, IF YOU
USE THIS WITH SHELLS/SCREEN!

STDIN and STDOUT are dup2(2)'d to a socketpair(2)'d (and specifically not pipe2(2)'d because
IO::Select's select(2) call below doesn't function well on fifos; that's the purpose of
pselect(2)) Unix Socket by a pty child process. STDIN comes from the SLAVE terminal
attached to pty's driven process and sent to us. STDOUT is written back to the SLAVE
terminal which is delivered to that driven process's STDIN.  This represents the combined
terminal interfaces of the driven process. STDERR is attached to the MASTER terminal
(the one we can interface with directly via hardware devices), since pty dup2's the
controlling pty process's STDIN to STDERR.

Here's an example cron job that will run a shell script:

0 23 * * * pty -d pty-driver.pl -- $script_with_eg_ansible_vaults

Note: fd 3 is attached directly to the SLAVE terminal, which we inherited
from the parent pty process during our fork+exec.

=head2 CONSTANTS

=cut

()=<<'=pod'; # copy following code into podlator

=pod

use constant MASTER_TTY_FD        => fileno STDERR; # 2

use constant SLAVE_TTY_FD         => 3;

use constant BUFSIZE              => 65536;

use constant SOCKET_IO_TIMEOUT    => 3;

use constant TTY_WRITE_TIMEOUT    => 60;

use constant TTY_READKEY_TIMEOUT  => 0.01;

use constant PTY_AGENT_SOCKET     => "$ENV{HOME}/.pty-agent/socket";

use constant IN_BAND_TOGGLER      => 0; # warning: this has security implications if enabled

use constant READ_LOOP_MAX        => 1024;

=head2 INITIALIZATION

Intitialize pty-agent if necessary. pty-agent sticks around until reboot, and
is usually the only thing that turns up on a $(pgrep -u $USER pty-agent) cmd.
if that's hard to ferret out just use lsof on the socket as below.

=cut

system qq(pgrep -u $ENV{USER} -f pty-agent >/dev/null 2>&1 || exec pty-agent);

=pod

Initialize rw connection to master/slave terminals.  If $mterm is
not a tty, then any calls to prompt() induced during the driving
process will cause the entire show to end.  So be sure all the required
passwords are available in the running `pty-agent` before using this
script w/o a controlling master terminal (eg cron apps).

=cut

# adjusts toggle input line, matching unsuffixed $0
my $script_name = basename $0, ".pl";

# better to use this out-of-band toggler in conjunction with the shipped ttyname binary
my $stty_name = basename $ENV{STTY_NAME};
my $ptyon_dir = "/tmp/ptyon-$ENV{USER}";
system "mkdir -m 0700 -p $ptyon_dir && touch $ptyon_dir/$stty_name" and die $!;

# master/slave terminals
my ($mterm, $sterm);

for ([\$mterm, MASTER_TTY_FD, sub {ReadMode "ultra-raw" => $mterm}],
     [\$sterm, SLAVE_TTY_FD,  sub {}])
{
  ## no critic
  open ${$$_[0]}, "+<&=" . $$_[1]
    or die "Can't open $$_[1]: $!";
  isatty ${$$_[0]} or warn "$$_[1] not a tty!";
  $$_[2]->();
}

## no critic (Subroutines::ProhibitSubroutinePrototypes)
sub write_master (;$);

# Die cleanly if called for
$SIG{__DIE__} = sub { write_master shift; sleep 1; unlink "$ptyon_dir/$stty_name"; _exit 255 };

# pty's typical cleanup signal
$SIG{TERM} = sub { defined $mterm and ReadMode restore => $mterm; unlink "$ptyon_dir/$stty_name"; _exit 0 };

# reset MASTER terminal (invoked on die() and normal exit(), not on signals)
END { defined $mterm and ReadMode restore => $mterm; unlink "$ptyon_dir/$stty_name"; sleep 1; }


=head2 HELPER FUNCTIONS

=over 4

=item echo_enabled ()

Returns true if the slave terminal has echo enabled.

=cut

my $stermios = POSIX::Termios->new;

## no critic
sub echo_enabled () {
  $stermios->getattr(SLAVE_TTY_FD);
  my $eflags = (ECHO | ECHOE | ECHONL | ECHOK);
  return $eflags == ($eflags & $stermios->getlflag);
}

# these two subs are here just-in-case they prove useful (not yet so)

sub disable_echo () {
  $stermios->getattr(SLAVE_TTY_FD);
  $stermios->setlflag($stermios->getlflag & ~(ECHO | ECHOE | ECHONL | ECHOK));
  defined $stermios->setattr(SLAVE_TTY_FD, TCSANOW) or die "setattr failed: $!";
  select undef, undef, undef, TTY_READKEY_TIMEOUT; ## no critic
  die "Can't disable echo on slave: $!" if echo_enabled;
}

sub enable_echo () {
  $stermios->getattr(SLAVE_TTY_FD);
  $stermios->setlflag($stermios->getlflag | (ECHO | ECHOE | ECHONL | ECHOK));
  defined $stermios->setattr(SLAVE_TTY_FD, TCSANOW) or die "setattr failed: $!";
  select undef, undef, undef, TTY_READKEY_TIMEOUT; ## no critic
  die "Can't enable echo on slave: $!" unless echo_enabled;
}

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
    alarm TTY_WRITE_TIMEOUT;
    do {
      my $w = syswrite $mterm, $_, $blen - $wrote, $wrote;
      die "syswrite failed: $!" unless defined $w and $w >= 0;
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
    alarm TTY_WRITE_TIMEOUT;
    do {
      my $w = syswrite $sterm, $_, $blen - $wrote, $wrote;
      die "syswrite failed: $!" unless $w >= 0;
      $wrote += $w;
    } while $wrote < $blen;
    alarm 0;
  };
  die $@ if $@;
  return $wrote;
}

=item read_input_nb ($)

ReadKey in a (portable non-blocking) loop on the passed filehandle, to $_.
Returns length of $_.

=cut

sub read_input_nb ($) {
  my $r = shift; # either a socket or a terminal - either way ReadKey will work
  my $flags = fcntl $r, F_GETFL, 0 or die "fcntl F_GETFL: $!";
  $_ = "";
  fcntl $r, F_SETFL, $flags | O_NONBLOCK or die "fcntl F_SETFL O_NONBLOCK: $!";
  my $loop;
  while (defined read($r, $_, BUFSIZE, length)) {
    last if ++$loop == READ_LOOP_MAX;
    select undef, undef, undef, TTY_READKEY_TIMEOUT; ## no critic
  }
  fcntl $r, F_SETFL, $flags or die "fcntl reset: $!";
  return length;
}

=item prompt ($;$)

Prompt master terminal for a password of a given argument $type and return it.

=cut

sub prompt ($;$) {
  my $type = shift;
  my $format = shift // 'Password for "%s"';
  # block these to avoid leaving $mterm in a non-echo state
  local $SIG{INT} = local $SIG{QUIT} = local $SIG{TSTP} = "IGNORE";
  if (echo_enabled) {
    ReadMode restore => $mterm;
  }
  else {
    ReadMode noecho => $mterm;
  }
  no warnings;
  write_master sprintf "\n$format (^D aborts %s): ", $type, $script_name; # aborting will terminate pty
  chomp(my $passwd = ReadLine 0, $mterm);
  defined $passwd or die "Operation aborted";
  ReadMode "ultra-raw" => $mterm;
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
              # a failure that requires a new prompt() by getpw()
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

sub getpw ($;$$) {
  my ($type, $prompt, $label) = @_;
  index($type, ' ') >= 0
    and die "getpw(): invalid type '$type' contains a space char!";

  if (-S PTY_AGENT_SOCKET) {

    my $socket = IO::Socket::UNIX->new(
      Domain => AF_UNIX,
      Type => SOCK_STREAM,
      Peer => PTY_AGENT_SOCKET,
      ) or warn "Can't connect to pty-agent socket: $!\n"
        and goto NO_SOCKET;

    if ($prompt or $saw_pw{$type}++) {
      my $newvalue = prompt $type, $label;
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
    return "$reply\n";
  }
  else {
  NO_SOCKET:
    $secret{$type} = prompt $type, $label if $prompt or $saw_pw{$type}++
      or not $secret{$type};
    return "$secret{$type}\n";
  }
}

# main:: globals for internal/external drive {} code blocks.

=back

=head2 GLOBAL VARIABLES

=cut

()=<<'=pod'; # copy following code into podlator

=pod

our $PREFIX_RE = qr/.*/; # everything on the current line

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
  my $custom_handler = shift;

  my $s = IO::Select->new(\*STDIN, $mterm); # can't use $sterm because pty consumes its input

  local $_;

  my $clear = `clear`;

  while (my @readable = $s->can_read) {

    for my $r (@readable) {
      # a normal exit here can happen when the driven process shuts down.
      read_input_nb $r or exit;

      if ($r != $mterm) {
        # adjust driver activation status vie $ptyon_dir/$stty_name semaphore.
        s{^($PREFIX_RE)$script_name( on| off)(\s)}{
          if ($2 eq " on") {
            no warnings "once";
            open my $dummy, ">", "$ptyon_dir/$stty_name";
          }
          else {
            unlink "$ptyon_dir/$stty_name";
          }
          "$1$script_name turned$2.$3"
        }gem if IN_BAND_TOGGLER;

        # write SLAVE output in $_ to MASTER so we can see it.
        write_master;

        if (index($_, $clear) >= 0) {
          # don't process window clears (during a redraw).
        }
        elsif (! -f "$ptyon_dir/$stty_name") {
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

# check if called from do FILE or require FILE or use MODULE in another script.
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

  if (m!\Q(yes/${NSM}no\)?! or /'yes' or ${NSM}'no'/) {
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
