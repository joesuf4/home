#!/usr/bin/perl -nal

# pipe the output of `eks batch $foo strace -t -p 1 -f` to this script

# $ENV{IP} is optional (target host IP)
# $ENV{PORT} is required, defaults to https(443)
# $ENV{SPATH} is optional (unix socket path)

BEGIN {
  $ENV{PORT} //= 443;
  $SIG{TERM} = $SIG{HUP} = $SIG{INT} = sub { exit };

  sub parse {
    for my $fd (grep $_ > 0, @_) {
      my @pid;
      for (@{$tinfo{$name}[$fd]}) {
        push @{$pid[$_->[0]]}, $_;
        shift @$_;
      }
      print "BEGIN TXN:$name\t$fd";
      for (grep defined $pid[$_], 1..$#pid) {
        print "  BEGIN TID:\t$_";
        for my $line (@{$pid[$_]}) {
          print "    $_\t@$line";
        }
        print "  END TID:  \t$_";
      }
      print "END TXN:$name\t$fd";
    }
  }
}

tr /0-9//dc for $fd = $F[4], $F[2];
tr /a-z//dc for $call = $F[4];
$rv = ($F[-2] eq "=") ? $F[-1] : undef;
$name = shift @F;
shift @F;

if ($call eq "connect") {
  parse $fd;
  $tinfo{$name}[$fd] = ((/\Qhtons($ENV{PORT})/ && /\Q$ENV{IP}/) || (/AF_UNIX/ && exists $ENV{SPATH} and /\Q$ENV{SPATH}/)) ? [[@F]] : undef;
}
elsif ($call eq "open" or $call eq "socket") {
  parse $rv;
  $tinfo{$name}[$rv] = undef;
}
elsif (defined $tinfo{$name}[$fd]) {
  push @{$tinfo{$name}[$fd]}, [@F];
}

END {
  print "FINAL";
  for $name (keys %tinfo) {
    parse grep defined $tinfo{$name}[$_], 0..$#{$tinfo{$name}};
  }
}
