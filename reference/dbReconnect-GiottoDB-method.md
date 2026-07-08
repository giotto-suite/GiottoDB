# Reconnect a GiottoDB object

Reconnects a stale `GiottoDB` object by reusing the canonical connection
from one persistent database-backed child object.

## Usage

``` r
# S4 method for class 'GiottoDB'
dbReconnect(x)
```

## Arguments

- x:

  A [`GiottoDB`](https://rdrr.io/pkg/GiottoDB/man/GiottoDB-class.html)
  object.

## Value

A [`GiottoDB`](https://rdrr.io/pkg/GiottoDB/man/GiottoDB-class.html)
object with a valid top-level database connection.
