#
# This Awk script is run by running awk with this script passed on the command line
# followed by the name of the data file
#
# Example:
#
#   awk -f 1b.awk data_sample.txt
#
#
# Created by: James McQuillan <jam@McQuil.com>
#
#-------------------------------------------------------------------------------

{
  col1[NR] = $1
  col2[$2]++
}

END {
  sum = 0

  for( i = 1; i <= length( col1 ); i++ )
    sum = sum + col1[i] * col2[col1[i]]

  print( "\nTotal simularity score: " sum "\n" )
}
