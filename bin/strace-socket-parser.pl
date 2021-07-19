#!/usr/bin/perl -nal

# pipe the output of `strace -t` to this script

# $ENV{IP} is optional (target host IP)
# $ENV{PORT} is required, defaults to https(443)

$ENV{PORT} //= 443;

BEGIN {
    sub parse {
        for my $fd (grep $_ > 0, @_) {
            my @pid;
            for (@{$tinfo{$name}[$fd]}) {
                push @{$pid[$_->[0]]}, $_;
                shift @$_;
            }
            print "BEGIN TXN:\t$fd";
            for (grep defined $pid[$_], 1..$#pid) {
                print "  BEGIN TID:\t$_";
                for my $line (@{$pid[$_]}) {
                    print "    $_\t@$line";
                }
                print "  END TID:  \t$_";
            }
            print "END TXN:  \t$fd";
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
if (defined $tinfo{$name}[$fd]) {
    push @{$tinfo{$name}[$fd]}, [@F];
}

END {
    for my $n (keys %tinfo) {
      parse grep defined $tinfo{$n}[$_], 0..$#{$tinfo{$n}};
    }
}
