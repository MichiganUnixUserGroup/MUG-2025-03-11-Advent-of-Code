--
-- SQL implementation of day 1 part B
--
-- This *probably* only works in Postgresql, as other databases don't have the *UNNEST* function.
--
-- Run this by using the psql command and redirecting the data file as stdin
--
-- Example:
--
--   psql -f 1b.sql <data_sample.txt
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

CREATE TEMP TABLE columns (
  col1 INT,
  col2 INT
);

INSERT INTO columns (
  col1,
  col2
) SELECT * FROM UNNEST( ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\1' )::INT
                                   FROM data
                               ) ),
                        ARRAY( ( SELECT regexp_replace( data.line, '(\d+)\s+(\d+)', '\2' )::INT
                                   FROM data
                               ) )
                      ) AS foo;

SELECT SUM( bar.similarity_score ) AS "Total simularity score"
  FROM (
         SELECT columns.col1 * ( SELECT COUNT(*)
                                   FROM columns AS foo
                                  WHERE foo.col2 = columns.col1
                               ) AS similarity_score
           FROM columns
       ) AS bar;
