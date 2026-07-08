# Convert dbSpatial overlaps to a matrix

Convert dbSpatial overlap results into a dbMatrix, sparse Matrix, or
data.table-like output for downstream Giotto workflows.

## Usage

``` r
# S4 method for class 'dbSpatial'
overlapToMatrix(
  x,
  col_names = NULL,
  row_names = NULL,
  feat_count_column = NULL,
  count_info_column = NULL,
  output = c("dbSparseMatrix", "Matrix", "data.table"),
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  object containing overlaps info. Can be giotto object or SpatVector
  points or data.table of overlaps generated from `calculateOverlap`

- col_names, row_names:

  character vector. (optional) Set of row and col names that are
  expected to exist. This fixes the dimensions of the matrix since the
  overlaps information does not directly report rows and cols where no
  values were detected.

- feat_count_column:

  column with count information. If a column called "count" is present
  in the feature points data, it will be automatically selected.

- count_info_column:

  deprecated. Use `feat_count_column` instead.

- output:

  data format/class to return the results as. Default is "Matrix"

- verbose:

  be verbose

- ...:

  additional params to pass to methods

## Value

A dbSparseMatrix, sparse Matrix, or data.table-like result, depending on
`output`.
