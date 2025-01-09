#!/usr/bin/perl -w

use strict;

my @col1;
my @col2;

open( my $FH, '<', 'data.txt' ) or die( "Failed to open file: $!\n" );

#while( my $line = <$FH> ){
#  my ( $c1, $c2 ) = split( /\s+/, $line );
#  push @col1, $c1;
#  push @col2, $c2;
#}

while( my $line = <$FH> ){
  if( $line =~ m/(\d+)\s+(\d+)$/ ){
    push @col1, $1;
    push @col2, $2;
  }
}

close( $FH );

@col1 = sort( @col1 );
@col2 = sort( @col2 );

my $sum = 0;

for( my $i = 0; $i < @col1; $i++ ){
  $sum += abs( $col1[$i] - $col2[$i] );
}

print("sum: $sum\n");
