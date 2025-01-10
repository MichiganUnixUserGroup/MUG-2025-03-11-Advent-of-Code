#!/usr/bin/perl -w

#
# This Perl script expects the data to be in a file passed on the command line
#
# Easiest way to run this is by doing:
#
#    ./1a.pl  data_sample.txt
#
# Once you run it with the sample data then try it
# with the real data file.
#
# Created by: James McQuillan <jam@McQuil.com>
#

use strict;

my $filename = $ARGV[0] or die("Must specify filename as the only argument!\n");

#
# @col1 is an array to hold all of the values in column 1 of the input data
#
my @col1;

#
# @col2 is an array to hold all of the values in column 2 of the input data
#
my @col2;

open( my $FH, '<', $filename ) or die( "Failed to open $filename: $!\n" );

#
# Loop through the lines of the data file and build the Arrays
#
while( my $line = <$FH> ){
  my ( $left, $right ) = split( /\s+/, $line );

  push @col1, $left;
  push @col2, $right;
}

close( $FH );

#
# Sort both arrays
#
@col1 = sort( @col1 );
@col2 = sort( @col2 );

my $sum = 0;

#
# Loop through the arrays and add up the differences
#
for( my $i = 0; $i < @col1; $i++ ){
  $sum += abs( $col1[$i] - $col2[$i] );
}

print( "\nTotal distance between lists: $sum\n\n" );
