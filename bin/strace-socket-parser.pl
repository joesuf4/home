#!/usr/bin/perl -nal

# pipe the output of `eks batch $foo strace -t -p 1 -f` to this script

# $ENV{IP} is optional (target host IP)
# $ENV{PORT} is required, defaults to https(443)

$ENV{PORT} //= 443;
$SIG{TERM} = $SIG{HUP} = $SIG{INT} = sub { exit };

BEGIN {
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

if ($call eq "socket" or $call eq "open") {
  parse $rv;
  $tinfo{$name}[$rv] = [[@F]];
}
elsif ($call eq "connect") {
  parse $fd;
  $tinfo{$name}[$fd] = (/\Qhtons($ENV{PORT})/ && /\Q$ENV{IP}/) ? [[@F]] : undef;
}
elsif (defined $tinfo{$name}[$fd]) {
  push @{$tinfo{$name}[$fd]}, [@F];
}

END {
  for my $n (keys %tinfo) {
    $name = $n;
    parse grep defined $tinfo{$n}[$_], 0..$#{$tinfo{$n}};
  }
}
