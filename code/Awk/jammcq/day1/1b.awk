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
