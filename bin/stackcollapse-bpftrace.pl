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
  our $increment = @ARGV && ($ARGV[0] eq "++");
  shift if $increment;
}

chomp;
s/\r$//;
our (@stack, $increment, $in_stack, %h);

if (!$in_stack) {
  $in_stack = /^@\w*\[[^\]]*$/;
} else {
  if (/^,?\s?(.*)\]:\s*(\d+)?$/) {
    my $count = $2;
    unless ($count) {
      while (<>) {
        chomp;
        s/\r$//;
        last unless /(\s+)(\d+)\s+[|]\@*/;
        $count += $2;
      }
    }
    $h{join(';',reverse( @stack))} += our $increment || $count;
    $in_stack = 0;
    @stack = ();
  } else {
    /^\s+[\dxa-f]+ (\w.*?[+]\d+|[\dxa-f]+)/ and push @stack, $1;
  }
}

END {
  $, = " ";
  $\ = "\n";
  print $_, $h{$_} for sort {$h{$b} <=> $h{$a}} keys %h;
}
