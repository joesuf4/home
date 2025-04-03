#!/usr/bin/perl -n
#
# stackcollapse-bpftrace.pl	collapse bpftrace samples into single lines.
#
# USAGE ./stackcollapse-bpftrace.pl infile > outfile
#
# Example input:
#
# @[
# _raw_spin_lock_bh+0
# tcp_recvmsg+808
# inet_recvmsg+81
# sock_recvmsg+67
# sock_read_iter+144
# new_sync_read+228
# __vfs_read+41
# vfs_read+142
# sys_read+85
# do_syscall_64+115
# entry_SYSCALL_64_after_hwframe+61
# ]: 3
#
# Example output:
#
# entry_SYSCALL_64_after_hwframe+61;do_syscall_64+115;sys_read+85;vfs_read+142;__vfs_read+41;new_sync_read+228;sock_read_iter+144;sock_recvmsg+67;inet_recvmsg+81;tcp_recvmsg+808;_raw_spin_lock_bh+0 3
#
# Copyright 2018 Peter Sanford.  All rights reserved.
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
#  (http://www.gnu.org/copyleft/gpl.html)
#
use strict;

BEGIN {
  our $timing_data = @ARGV && ($ARGV[0] eq "-t");
  our $increment = @ARGV && ($ARGV[0] eq "++");
  shift if $timing_data;
  shift if $increment;
  our %symbols;
  while (@ARGV) {
    my $fname = shift;
    my @s = qx(readelf -s -W $fname 2>/dev/null);
    next if $?;
    for (@s) {
      /\d+: 0+(\w+)\s+\d+\s+\w+\s+\w+\s+\w+\s+\w+\s+(\S+)/ or next;
      $symbols{"0x$1"} = $2;
    }
    unshift @ARGV, map / => (\S+)/, qx(ldd $fname);
    push our @pname, $fname;
  }

  our %nano = (
    K => 1024,
    M => 1024**2,
    G => 1024**3,
    T => 1024**4,
    P => 1024**5,
  );
  our $nk = join "", keys %nano;
}

chomp;
s/\r$//;
our (@stack, $increment, $timing_data, $in_stack, %symbols, %h, %nano, $nk, @pname);

if (!$in_stack) {
  $in_stack = /^@\w*\[[^\]]*$/;
}
else {
  if (/^,?\s?(.*)\]:\s*(\d+)?$/) {
    my $count = $2;
    unless ($count) {
      while (<>) {
        chomp;
        s/\r$//;
        last unless /(\s+)(\d+)\s+[|]\@*/;
        my $c = $2;
        if ($timing_data and /\[\d+[$nk], (\d+)([$nk])\)/) {
          my $ns_upper = $1 * $nano{$2};
          $c *= $ns_upper;
        }
        $count += $c;
      }
    }
    $h{join(';', reverse(@stack))} += $increment || ($timing_data ? log($count) : $count);
    $in_stack = 0;
    @stack = ();
  }
  else {
    /^\s+[\dxa-f]+ (\w.*?[+]\d+|[\dxa-f]+)/ and push @stack, $symbols{$1} //= do {
      my @s;
      my $addr = $1;
      for my $p (@pname) {
        @s = qx(addr2line -f -e $p $addr);
        chomp $s[0];
        last if length($s[0]) > 0 and index($s[0], "?") == -1;
      }
      index($s[0], "?") == -1 ? $s[0] : $addr
    };
  }
}

END {
  $, = " ";
  $\ = "\n";
  print $_, $h{$_} for sort {$h{$b} <=> $h{$a}} keys %h;
#  warn join ":", our %symbols;
}
