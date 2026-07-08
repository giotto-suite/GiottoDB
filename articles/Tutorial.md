# Tutorial

``` r

library(GiottoDB)
#> Loading required package: GiottoClass
#> GiottoDB v0.0.0.9001
#> 
#> Attaching package: 'GiottoDB'
#> The following objects are masked from 'package:GiottoClass':
#> 
#>     loadGiotto, saveGiotto, tessellate
```

## Introduction

- Link the Overview vignette.
- Demonstration of analyzing a MERFISH dataset with GiottoDB.

## Creating a GiottoDB Object

This section will demonstrate loading a Giotto object and converting it
into a GiottoDB object.

``` r

options("giotto.use_conda" = FALSE)
gobject <- GiottoData::loadGiottoMini(dataset = 'vizgen')

tmpfile = file.path(tempdir(), "test.db")
con = DBI::dbConnect(duckdb::duckdb(), tmpfile)
gobject_db <- as_giottodb(gobject, con = con)
```

### Convenience function to convert existing Giotto objects into GiottoDB objects

Alternatively, we can use the
[`as_giottodb()`](https://giotto-suite.github.io/GiottoDB/reference/as_giottodb.md)
function to convert an existing Giotto object into a GiottoDB object.

``` r

# gobject_db <- as_giottodb(gobject)
```

## Loading MERFISH Data

## Filtering

## Normalization

## 
