#!/usr/bin/perl -w

use strict;

my @col1;
my %col2;

open( my $FH, '<', 'data.txt' ) or die("Failed to open file: $!\n");

while( my $line = <$FH> ){
  if( $line =~ m/(\d+)\s+(\d+)$/ ){
    push @col1, $1;
    if( exists( $col2{$2} ) ){
      $col2{$2} += 1;
    }
    else{
      $col2{$2} = 1;
    }
  }
}

close( $FH );

my $sum = 0;

foreach my $entry ( @col1 ){
  if( exists( $col2{$entry} ) ){
    $sum += $entry * $col2{$entry};
  }
}

print("sum: $sum\n");
