#!/usr/bin/env perl
#
# pty driver customized for SYSTEM password and SSH passphrase prompt
# SPDX License Identifier: Apache License 2.0

use strict;
use warnings FATAL => 'all';
use IPC::Open2;
use URI;
use constant OTP_MINIMUM => 10;
use Cwd;
use lib "$ENV{HOME}/bin";
use pty_driver;

# keep 'use strict' happy (these are the two global vars we need
# from pty_driver.pm):

our $PREFIX_RE;
our $NSM;

my $gpg_prompt = 0;

drive {
  # this is the actual running program where user-customizable
  # code changes (to test $_) go.  Returns true if we handled
  # the contents of $_, false otherwise.

  if (m!\(yes/${NSM}no\)\?! or /'yes' or ${NSM}'no'/) {
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
  elsif (/^$PREFIX_RE\: ([PpQq]) (.+)/m and echo_enabled) {
    no warnings;
    no strict;
    local ($@, $_, @_);
    local $SIG{__DIE__} = sub { die shift };
    $_ = $2;
    my $wa    = $1 eq "P" || $1 eq "Q";
    my $quiet = $1 eq "q" || $1 eq "Q";
    s/[^[:print:]].*$//mg;
    s/\[\w+\]\s*$//mg;
    s/([a-z]+)\.(\S+)/$1 . (index($2, "'") == -1 && index($2, q(")) == -1 ? "->$2" : ".$2")/ge;
    if ($wa) {
      @_ = eval;
    }
    else {
      $_[0] = eval;
      $#_ == 1;
    }
    $quiet or write_master("\r\n$_\r\n$@\r\n") for join "\r\n", @_;
  }
  elsif (/^$PREFIX_RE\botp-sha1 (\d+) (\w+)/m and not echo_enabled) {
    my ($idx, $salt) = ($1, $2);
    my $pid = open2 my $out, my $in, "otp-sha1 $idx $salt 2>&-";
    my $pw = getpw("OTP");
    print $in $pw;
    close $in;
    write_slave <$out>;
    close $out;
    waitpid $pid, 0;
    if (--$idx < OTP_MINIMUM) {
      my $cpid = open2 my $cout, my $cin, "otp-sha1 $idx $salt 2>&-" or die "Can't popen otp-sha1: $!";
      print $cin $pw;
      close $cin;
      my $result = join "", <$cout>;
      close $cout;
      waitpid $cpid, 0;
      $pid = open2 $out, $in, "pty -ie ortpasswd 2>&1" or die "Can't popen ortpasswd: $!";
      scalar <$out>; # Password:
      $|=1, select $_ for select $in;
      print $in $result;
      scalar <$out>; # Password:
      s/^.*(otp-sha1 \d+ \w+).*$/$1/ or die "Bad challenge: $_" for my $challenge = <$out>;
      $cpid = open2 $cout, $cin, "$challenge 2>&-" or die "Can't popen $challenge: $!";
      print $cin $pw;
      close $cin;
      print $in join "", <$cout>;
      close $in;
      waitpid $cpid, 0;
      waitpid $pid, 0;
    }
    undef $pw;
  }
  elsif (/^$PREFIX_RE\bUsername for '([^']+)':/m) {
    write_slave getpw($1, 0, 'Username for "%s"');
  }
  elsif (/^$PREFIX_RE\b[Pp]assword for '([^']+)':/m and not echo_enabled) {
    write_slave getpw($1);
  }
  elsif (/^$PREFIX_RE\b(Authentication failed|Acess denied)/) {
    # skip to retry (git)
  }
  elsif (/^$PREFIX_RE\bEnter the [Pp]assword for/m and not echo_enabled) {
    write_slave getpw("1Password");
  }
  elsif(/^$PREFIX_RE\[ERROR\].*\(?401\)? Unauthorized/m) {
    # skip to reprompt (on above 1Password login failure)
  }
  elsif (/^$PREFIX_RE\b[Vv]ault [Pp]assword[^:\n]*:/m and not echo_enabled) {
    write_slave getpw("Vault");
  }
  elsif (/^$PREFIX_RE[Pp]assword(?: for $ENV{USER})?$NSM:/m and not echo_enabled) {
    write_slave getpw($ENV{USER});
  }
  elsif (/^$PREFIX_RE[Pp]assphrase:/m and not echo_enabled) {
    write_slave getpw("GPG", $gpg_prompt);
    $gpg_prompt=0;
  }
  elsif (/^${PREFIX_RE}gpg:.*[Bb]ad passphrase/m and echo_enabled) {
    $gpg_prompt=1;
  }
  elsif (/^$PREFIX_RE(?:Enter passphrase for|Bad passphrase, try again for)$NSM /m and not echo_enabled) {
    write_slave getpw("SSH");
  }
  elsif (/^$PREFIX_RE(?:Verification code|T?OTP\(([a-z0-9\@.-]+)\))$NSM:/m and not echo_enabled) {
    my $tag = defined($1) ? "\u$1" : $ENV{OP_TOTP};
    write_slave qx(eval "\$(pty -nie -- pty -d pty-driver.pl -- op signin -f 2>&1 | grep '^export ' | tr -d '\r')"; op item get --otp $tag);
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
  elsif (exists $ENV{MOZILLA} and length $ENV{MOZILLA}) {
    # use a port to evade this url pattern on the command-line (history!)
    my (%url_cache, $match);
    while (m!\b(https://[\w.-]+/[$URI::uric#|]+)!g) {
      $match++;
      next if $url_cache{$1}++;
      my $url = $1;
      my $pw = getpw($url, 1, "Visit %s in browser [y/n]?");
      system "('$ENV{MOZILLA}' '$url' >/dev/null 2>&1 &)" if $pw =~ /y/i;
      write_master "\r\n";
    }
    return $match;
  }
  else {
    return 0; # not handled by us
  }
  return 1; # handled successfully
}
