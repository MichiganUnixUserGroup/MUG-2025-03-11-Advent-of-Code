--
-- SQL implementation of day 1 part A
--
-- This *probably* only works in Postgresql, as other databases don't have the *UNNEST* function.
--
-- Run this by using the psql command and redirecting the data file as stdin
--
-- Example:
--
--   psql -f 1a.sql <data_sample.txt
--
--
-- Created by: James McQuillan <jam@McQuil.com>
--
-- -----------------------------------------------------------------------------
--
-- Create a table to hold the data so we can query it
--
CREATE TEMP TABLE data ( line TEXT );

--
-- Get the data into the database by getting it from STDIN of the psql command
--
\copy data FROM PSTDIN

SELECT SUM( bar.distance ) AS "Total distance between lists"
  FROM ( SELECT abs( foo.col1 - foo.col2 ) AS distance
           FROM UNNEST( ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\1' )::INT
                                   FROM data
                                  ORDER BY 1
                             ) ),
                        ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\2' )::INT
                                   FROM data
                                  ORDER BY 1
                             ) )
                      ) AS foo( col1, col2 )
        ) AS bar;

/*

-- Deconstructed:

1. Create the table

CREATE TEMP TABLE data ( line TEXT );


2. Load the data

\copy data FROM 'data_sample.txt'


3. Get the first column and sort it

SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\1' )::INT
  FROM data
 ORDER BY 1;


4. Get the second column and sort it

SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\2' )::INT
  FROM data
 ORDER BY 1;



5. select data from both columns and turn them into arrays

SELECT ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\1' )::INT
                  FROM data
                 ORDER BY 1
            ) ),
       ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\2' )::INT
                  FROM data
                 ORDER BY 1
            ) );
      

6. Combine the 2 arrays into rows where each row of the first array gets paired
   with a row from the second array

SELECT *
  FROM UNNEST( ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\1' )::INT
                          FROM data
                         ORDER BY 1
                    ) ),
               ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\2' )::INT
                          FROM data
                         ORDER BY 1
                    ) )
             ) AS foo( col1, col2 );


7. Get the absolute difference between the value in the first column and the
   value in the second column

SELECT abs( foo.col1 - foo.col2 ) AS distance
  FROM UNNEST( ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\1' )::INT
                          FROM data
                         ORDER BY 1
                    ) ),
               ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\2' )::INT
                          FROM data
                         ORDER BY 1
                    ) )
             ) AS foo( col1, col2 );


8. Sum the differences

SELECT SUM( bar.distance ) AS "Total distance between lists"
  FROM ( SELECT abs( foo.col1 - foo.col2 ) AS distance
           FROM UNNEST( ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\1' )::INT
                                   FROM data
                                  ORDER BY 1
                             ) ),
                        ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\2' )::INT
                                   FROM data
                                  ORDER BY 1
                             ) )
                      ) AS foo( col1, col2 )
        ) AS bar;
*/
