vim9script
#
# This vim9script expects the data to be loaded in the current
# vim buffer.
#
# Easiest way to run this is by doing:
#
#   vim data_sample.txt
#   :source 1b.vim
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
var lines = getline( 1, '$' )

#
# col1 is a list of all of the values in column 1 of the input data
#
var col1 = []

#
# col2 is a Dictionary (hash) where we keep track of the frequency
# of each value.
# The key is the value from column 2 of the input data and the value
# is the number of times that key appears.
#
var col2 = {}

#
# Loop through the lines of data and build the List and Dictionary
#
for line in lines
  var flds  = line->split()              # Split on whitespace

  #
  # Get the values of the 2 columns and turn them into numbers
  #
  var left  = str2nr( flds->get(0) )
  var right = str2nr( flds->get(1) )

  col1->add( left )

  #
  # See if the key exists in the dictionary
  #
  if col2->has_key( right )
    #
    # If it already exists then increment the counter
    #
    col2[right] += 1
  else
    #
    # Otherwise, create the dictionary entry and initialize it to 1
    #
    col2[right] = 1
  endif
endfor

var sum = 0

for entry in ( col1 )
  if col2->has_key( entry )
    #
    # Multiply the value by the frequency and add it to the sum
    #
    sum += entry * col2[entry]
  endif
endfor

echo "Total simularity score: " sum
