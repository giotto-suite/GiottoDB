# GiottoDB Class

S4 class that extends the giotto class to provide a database-backed
implementation of Giotto objects using
[dbMatrix::dbMatrix](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.html)
and
[dbSpatial::dbSpatial](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html).

## Value

A GiottoDB object

## Details

The GiottoDB class extends the standard giotto class, replacing
in-memory objects with database-backed alternatives where appropriate:

- Expression matrices ([matrix](https://rdrr.io/r/base/matrix.html),
  [Matrix](https://rdrr.io/r/base/matrix.html)) are replaced with
  [dbMatrix::dbMatrix](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.html)
  objects

- Spatial objects (points, polygons) are replaced with
  [dbSpatial::dbSpatial](https://dbverse-org.github.io/dbspatial-r/reference/dbSpatial.html)
  objects

This allows Giotto to scale to larger-than-memory datasets while
maintaining API compatibility with existing Giotto workflows.

## Slots

- `conn`:

  A `DBIConnection` object to a
  [duckdb::duckdb](https://r.duckdb.org/reference/duckdb.html) database
