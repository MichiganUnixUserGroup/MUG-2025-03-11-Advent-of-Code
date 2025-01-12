{
  col1[NR] = $1
  col2[NR] = $2
}

#-------------------------------------------------------------------------------

END {
  sort( col1 )
  sort( col2 )

  sum = 0

  for( i = 1; i <= length( col1 ); i++ )
    if( col1[i] > col2[i] )
      sum = sum + ( col1[i] - col2[i] )
    else
      sum = sum + ( col2[i] - col1[i] )

  print( "\nTotal distance between lists: " sum "\n" )
}

#-------------------------------------------------------------------------------

function sort( ARRAY ){
  len     = length( ARRAY )
  swapped = 1

  do{
    swapped = 0
    for( i = 2; i <= len; i++ ){
      if( ARRAY[i-1] > ARRAY[i] ){
        temp       = ARRAY[i-1]
        ARRAY[i-1] = ARRAY[i]
        ARRAY[i]   = temp
        swapped    = 1
      }
    }
  } while( swapped )
}
