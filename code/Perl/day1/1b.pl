#!/usr/bin/perl -w

#
# This Perl script expects the data to be in a file passed on the command line
#
# Easiest way to run this is by doing:
#
#    ./1b.pl  data_sample.txt
#
# Once you run it with the sample data then try it
# with the real data file.
#
# Created by: James McQuillan <jam@McQuil.com>
#

use strict;

#
# @col1 is an array to hold all of the values in column 1 of the input data
#
my @col1;

#
# %col2 is a hash where we keep track of the frequency of each value.
# The key is the value from column 2 of the input data and the value
# is the number of times that key appears.
#
my %col2;

my $filename = $ARGV[0] or die("Must specify filename as the only argument!\n");

open( my $FH, '<', $filename ) or die( "Failed to open $filename: $!\n" );

#
# Loop through the lines of the data file and build the Array and Hash
#
while( my $line = <$FH> ){
  my ( $left, $right ) = split( /\s+/, $line );

  push @col1, $left;

  #
  # See if the key exists in the hash
  #
  if( exists( $col2{$right} ) ){
    #
    # If it already exists then increment the counter
    #
    $col2{$right} += 1;
  }
  else{
    #
    # Otherwise, create the hash entry and initialize it to 1
    #
    $col2{$right} = 1;
  }
}

close( $FH );

my $sum = 0;

#
# Loop through the @col1 array and for each number that appears
# in the hash, do the multipication and add it to the sum
#
foreach my $entry ( @col1 ){
  if( exists( $col2{$entry} ) ){
    #
    # Multiply the value by the frequency and add it to the sum
    #
    $sum += $entry * $col2{$entry};
  }
}

print( "\nTotal simularity score: $sum\n\n" );
