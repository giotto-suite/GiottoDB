# Convert giotto Object to GiottoDB

A convenience function that coerces a giotto object to a GiottoDB object
with a database backend. Expression matrices are converted to dbMatrix
objects and spatial data are converted to dbSpatial objects.

## Usage

``` r
as_giottodb(
  x,
  con = NULL,
  db_path = NULL,
  prefix = "gdb_",
  overwrite = FALSE,
  verbose = TRUE,
  temporary = TRUE
)
```

## Arguments

- x:

  A giotto object

- con:

  A `DBIConnection` object from a
  [duckdb::duckdb](https://r.duckdb.org/reference/duckdb.html)
  connection object

- db_path:

  Path to the database file if creating a new persistent connection

- prefix:

  A string prefix to add to database table names

- overwrite:

  Whether to overwrite existing tables

- verbose:

  Whether to print progress messages

- temporary:

  if TRUE (default), will create a temporary table that is local to this
  connection and will be automatically deleted when con expires

## Value

A [`GiottoDB`](https://rdrr.io/pkg/GiottoDB/man/GiottoDB-class.html)
object

## Examples

``` r
if (FALSE) { # \dontrun{
library(GiottoDB)
library(dbSpatial)

# Create connection
con <- dbConnect(duckdb(), dbdir = ":memory:")
loadSpatial(con)

# Convert Giotto object
my_giotto_db <- as_giottodb(my_giotto, con = con)

# Don't forget to close the connection when done
dbDisconnect(con, shutdown = TRUE)
} # }
```
