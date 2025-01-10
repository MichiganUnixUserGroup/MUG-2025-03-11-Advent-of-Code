vim9script
#
# This vim9script expects the data to be loaded in the current
# vim buffer.
#
# Easiest way to run this is by doing:
#
#   vim data_sample.txt
#   :source 1a.vim
#
# Once you've run it with the sample data then try it
# by loading your actual data file.
#
# Created by:  James McQuillan <jam@McQuil.com>
#

#
# Create a list called 'lines' and load it with all of the lines
# current buffer
#
var lines = getline(1, '$')

#
# Define a couple lists to hold the 2 columns
#
var col1 = []
var col2 = []

#
# Loop through the lines of data and build the 2 new lists
#
for line in lines
  var flds = line->split()              # Split on whitespace
  col1->add( str2nr( flds->get(0) ) )   # fld 0 goes in col1
  col2->add( str2nr( flds->get(1) ) )   # fld 1 goes in col2
endfor

#
# Sort the two lists
#
col1 = col1->sort()
col2 = col2->sort()

#
# Loop through the two lists and get the sum of the distance
# between the value in col1 and the value in col2
#
# List items start at index 0 so our range needs to go from
# 0 to the size of the list - 1.
#

var sum = 0

for i in range( 0, col1->len() - 1 )
  sum += abs( col1->get( i ) - col2->get( i ) )
endfor

echo "Total distance between lists: " sum
