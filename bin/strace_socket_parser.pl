#!/usr/bin/perl -nal

# pipe the output of `strace -t` to this script

# $ENV{IP} is optional (target host IP)
# $ENV{PORT} is required, defaults to https(443)

$ENV{PORT} //= 443;

BEGIN {
    sub parse {
        for my $fd (grep $_ > 0, @_) {
            my @pid;
            for (@{$tinfo[$fd]}) {
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
tr /0-9//dc for $fd = $F[3], $F[1];
tr /a-z//dc for $call = $F[3];
$rv = ($F[-2] eq "=") ? $F[-1] : undef;
shift @F;

if ($call eq "socket" or $call eq "open") {
    parse $rv;
    $tinfo[$rv] = [[@F]];
}
elsif ($call eq "connect") {
    parse $fd;
    $tinfo[$fd] = (/\Qhtons($ENV{PORT})/ && /\Q$ENV{IP}/) ? [[@F]] : undef;
}
if (defined $tinfo[$fd]) {
    push @{$tinfo[$fd]}, [@F];
}

END {
    parse grep defined $tinfo[$_], 0..$#tinfo;
}
