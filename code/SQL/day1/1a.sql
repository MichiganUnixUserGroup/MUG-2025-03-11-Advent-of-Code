
CREATE TEMP TABLE data ( line TEXT );

--
-- Pick which input file you want to use
--

\copy data FROM 'data_sample.txt'

--\copy data FROM 'data.txt'


SELECT SUM( bar.diff )
  FROM ( SELECT abs( foo.col1 - foo.col2 ) AS diff
          FROM ( SELECT UNNEST( ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\1' )::INT
                                           FROM data
                                          ORDER BY 1
                                       ) )
                              ) AS col1,
                        UNNEST( ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\2' )::INT
                                           FROM data
                                          ORDER BY 1
                                       ) )
                              ) AS col2
               ) AS foo
       ) AS bar;
