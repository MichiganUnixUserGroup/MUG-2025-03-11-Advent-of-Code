\set QUIET ON

CREATE TEMP TABLE data ( line TEXT );

--
-- Pick which input file you want to use
--

\copy data FROM 'data_sample.txt'

--\copy data FROM 'data.txt'

CREATE TEMP TABLE columns (
  col1 INT,
  col2 INT
);


INSERT INTO columns (
  col1,
  col2
) SELECT UNNEST( ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\1' )::INT
                            FROM data
                      ) )
               ) AS col1,
         UNNEST( ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\2' )::INT
                            FROM data
                        ) )
               ) AS col2;

SELECT SUM(bar.similarity_score) AS "Total simularity score"
  FROM (
         SELECT columns.col1 * ( SELECT COUNT(*)
                                   FROM columns AS foo
                                  WHERE foo.col2 = columns.col1
                               ) AS similarity_score
           FROM columns
       ) AS bar;
