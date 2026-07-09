# Convert GiottoDB Object to giotto

A convenience function that coerces a `GiottoDB` object to an in-memory
`giotto` object. Expression matrices are converted to in-memory matrices
using dbverse coercions:

- `dbSparseMatrix`/`dbMatrix` -\> `dgCMatrix`

- `dbDenseMatrix` -\> base `matrix`

Spatial data stored as `dbSpatial` are converted to
[`terra::SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html).

## Usage

``` r
as_giotto(x, verbose = TRUE)
```

## Arguments

- x:

  A
  [`GiottoDB`](https://giotto-suite.github.io/GiottoDB/reference/GiottoDB-class.html)
  object

- verbose:

  Whether to print progress messages

## Value

A
[`GiottoClass::giotto`](https://giotto-suite.github.io/GiottoClass/reference/giotto-class.html)
object

## Examples

``` r
if (FALSE) { # \dontrun{
library(GiottoDB)
library(GiottoData)
library(duckdb)
library(DBI)

g <- GiottoData::loadGiottoMini("visium")
con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
gdb <- as_giottodb(g, con = con)

g_inmem <- as_giotto(gdb)
DBI::dbDisconnect(con, shutdown = TRUE)
} # }
```
