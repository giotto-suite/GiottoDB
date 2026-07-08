# Add statistics for Giotto / GiottoDB

S3 generic that delegates to
[`Giotto::addStatistics()`](https://rdrr.io/pkg/Giotto/man/addStatistics.html)
for plain giotto objects, and adds DuckDB-native polygon area support
for GiottoDB objects with dbSpatial-backed polygons.

## Usage

``` r
addStatistics(gobject, ...)

# S3 method for class 'giotto'
addStatistics(
  gobject,
  feat_type = NULL,
  spat_unit = NULL,
  stats = c("feature", "cell", "area"),
  expression_values = c("normalized", "scaled", "custom"),
  detection_threshold = 0,
  return_gobject = TRUE,
  verbose = TRUE,
  ...
)

# S3 method for class 'GiottoDB'
addStatistics(
  gobject,
  feat_type = NULL,
  spat_unit = NULL,
  stats = c("feature", "cell", "area"),
  expression_values = c("normalized", "scaled", "custom"),
  detection_threshold = 0,
  return_gobject = TRUE,
  verbose = TRUE,
  ...
)

# Default S3 method
addStatistics(gobject, ...)
```

## Arguments

- gobject:

  A giotto or GiottoDB object.

- ...:

  Additional arguments (currently ignored).

- feat_type:

  Feature type.

- spat_unit:

  Spatial unit.

- stats:

  Which statistics to compute.

- expression_values:

  Expression values to use.

- detection_threshold:

  Detection threshold.

- return_gobject:

  Whether to return the updated object.

- verbose:

  Verbosity.
