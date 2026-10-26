#!/usr/bin/perl
# usage: analyze-a.pl <t0> <db-ids> <writer-log> <consumer-log>
use strict; use warnings;
my ($t0, $dbf, $wf, $cf) = @ARGV;
my (%db, %ack, %recv, %first);
open my $f, '<', $dbf or die; while (<$f>) { chomp; $db{$_} = 1 if /^\d+$/ } close $f;
open $f, '<', $wf or die; while (<$f>) { $ack{$1} = $2 if /^(\d+) ([\d.]+)/ } close $f;
open $f, '<', $cf or die;
while (<$f>) {
  next unless /^([\d.]+) table public\.events: INSERT: id\[bigint\]:(\d+)/;
  my ($t, $id) = ($1, $2);
  $recv{$id}++;
  $first{$id} = $t if !exists $first{$id};
}
close $f;
my $extra = 0; $extra += $_ - 1 for values %recv;
my @dups = sort { $a <=> $b } grep { $recv{$_} > 1 } keys %recv;
my @missing = sort { $a <=> $b } grep { !$recv{$_} } keys %db;
my @ahead = sort { $a <=> $b } grep { !$db{$_} } keys %recv;
my @lost_acked = sort { $a <=> $b } grep { !$db{$_} } keys %ack;
sub rng { my @a = @_; return @a ? sprintf("%d..%d", $a[0], $a[-1]) : "-" }
sub longest_gap {
  my @t = sort { $a <=> $b } @_;
  my ($best, $from, $to) = (0, undef, undef);
  for my $i (1 .. $#t) { my $g = $t[$i] - $t[$i-1]; ($best, $from, $to) = ($g, $t[$i-1], $t[$i]) if $g > $best }
  return ($best, $from, $to);
}
printf "rows committed (in the database now)          : %d\n", scalar keys %db;
printf "rows acknowledged to the writer               : %d\n", scalar keys %ack;
printf "distinct rows the consumer received           : %d\n", scalar keys %recv;
printf "duplicate deliveries (extra receipts)         : %d  (ids %s)\n", $extra, rng(@dups);
printf "holes: in the database, never received        : %d  (ids %s)\n", scalar @missing, rng(@missing);
printf "ahead: received, absent from the database     : %d  (ids %s)\n", scalar @ahead, rng(@ahead);
printf "acknowledged to the writer but lost           : %d  (ids %s)\n", scalar @lost_acked, rng(@lost_acked);
my ($g, $f, $t) = longest_gap(values %first);
printf "consumer: longest silence %.1f s, from t0%+.1f s to t0%+.1f s\n", $g, $f - $t0, $t - $t0;
($g, $f, $t) = longest_gap(values %ack);
printf "writer:   longest silence %.1f s, from t0%+.1f s to t0%+.1f s\n", $g, $f - $t0, $t - $t0;
