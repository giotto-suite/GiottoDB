# Create GiottoPoints object using dbSpatial

Create a `giottoPoints` object that wraps a dbSpatial points object to
support larger-than-memory spatial point data. This implementation
extends the standard GiottoClass implementation by providing specific
methods for dbSpatial objects.

## Usage

``` r
# S4 method for class 'dbSpatial'
createGiottoPoints(
  x,
  feat_type = "rna",
  verbose = TRUE,
  split_keyword = NULL,
  unique_IDs = NULL
)
```

## Arguments

- x:

  dbSpatial object, SpatVector, or data.frame-like object with points
  coordinate information (x, y, feat_ID)

- feat_type:

  character. feature type. Provide more than one value if using the
  `split_keyword` param. For each set of keywords to split by, an
  additional feat_type should be provided in the same order.

- verbose:

  be verbose

- split_keyword:

  list of character vectors of keywords to split the giottoPoints object
  based on their feat_ID. Keywords will be
  [`grepl()`](https://rdrr.io/r/base/grep.html) matched against the
  feature IDs information.

- unique_IDs:

  (optional) character vector of unique IDs present within the
  spatVector data. Provided for cacheing purposes

## Value

giottoPoints object wrapping a dbSpatial object
