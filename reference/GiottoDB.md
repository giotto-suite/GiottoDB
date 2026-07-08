# Create a new GiottoDB object

Create a new GiottoDB object, which is a database-backed implementation
of the Giotto object.

## Usage

``` r
GiottoDB(con, ...)
```

## Arguments

- con:

  A `DBIConnection` object to a
  [duckdb::duckdb](https://r.duckdb.org/reference/duckdb.html) database

- ...:

  Additional arguments passed to the giotto constructor

## Value

A GiottoDB object

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a new GiottoDB object with a new database connection
library(GiottoDB)
library(duckdb)
library(DBI)

# Create a connection to a DuckDB database
con <- dbConnect(duckdb(), dbdir = ":memory:")
dbSpatial::loadSpatial(con) # Load spatial extension

# Create a new GiottoDB object
gobj_db <- GiottoDB(con = con)

# Don't forget to close the connection when done
dbDisconnect(con, shutdown = TRUE)
} # }
```
