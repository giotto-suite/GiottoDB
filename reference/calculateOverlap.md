# Calculate overlaps with dbSpatial-backed objects

Compute overlaps between dbSpatial-backed polygon and point features
while keeping the result database-backed.

## Usage

``` r
# S4 method for class 'dbSpatial,dbSpatial'
calculateOverlap(
  x,
  y,
  poly_subset_ids = NULL,
  feat_subset_column = NULL,
  feat_subset_values = NULL,
  feat_count_column = NULL,
  feat_subset_ids = NULL,
  count_info_column = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  Object with spatial annotations: `giottoPolygon`, or `SpatVector`
  polygons. Can also be a `giotto` object

- y:

  Object with features to overlap: `giottoPoints`, `giottoLargeImage`,
  `SpatVector` points or `SpatRaster`

- poly_subset_ids:

  character vector. (optional) Specific poly_IDs to use

- feat_subset_column:

  character. (optional) feature info attribute to subset feature points
  on when performing overlap calculation.

- feat_subset_values:

  (optional) values matched against in `feat_subset_column` in order to
  subset feature points when performing overlap calculation.

- feat_count_column:

  character. (optional) column with count information. Useful in cases
  when more than one detection is reported per point. If a column called
  "count" is present in the feature points data, it will be
  automatically selected.

- feat_subset_ids:

  deprecated. Use `feat_subset_values` instead.

- count_info_column:

  deprecated. Use `feat_count_column` instead.

- verbose:

  be verbose

- ...:

  additional params to pass to methods.

## Value

A dbSpatial object containing point features joined to overlapping
polygon metadata.
