# Normalize Expression for GiottoDB with dbMatrix support

For GiottoDB objects with dbMatrix expression, scaling (centering) is
handled implicitly during PCA via `db_svd`. If `scale_feats` or
`scale_cells` are `TRUE`, a warning is emitted and both are silently
forced to `FALSE`. No "scaled" expression slot is created.

## Usage

``` r
normalizeGiotto(gobject, ...)

# S3 method for class 'GiottoDB'
normalizeGiotto(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = "raw",
  norm_methods = c("standard", "pearson_resid", "osmFISH", "quantile"),
  library_size_norm = TRUE,
  scalefactor = 6000,
  log_norm = TRUE,
  log_offset = 1,
  logbase = 2,
  scale_feats = TRUE,
  scale_genes = lifecycle::deprecated(),
  scale_cells = TRUE,
  scale_order = c("first_feats", "first_cells"),
  theta = 100,
  name = "scaled",
  update_slot = lifecycle::deprecated(),
  verbose = TRUE,
  ...
)

# S3 method for class 'giotto'
normalizeGiotto(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = "raw",
  norm_methods = c("standard", "pearson_resid", "osmFISH", "quantile"),
  library_size_norm = TRUE,
  scalefactor = 6000,
  log_norm = TRUE,
  log_offset = 1,
  logbase = 2,
  scale_feats = TRUE,
  scale_genes = lifecycle::deprecated(),
  scale_cells = TRUE,
  scale_order = c("first_feats", "first_cells"),
  theta = 100,
  name = "scaled",
  update_slot = lifecycle::deprecated(),
  verbose = TRUE,
  ...
)
```

## Arguments

- gobject:

  A giotto or GiottoDB object

- ...:

  Additional arguments passed to
  [`Giotto::normalizeGiotto`](https://rdrr.io/pkg/Giotto/man/normalizeGiotto.html)

- scale_feats:

  Not supported for GiottoDB. Forced to `FALSE` with a warning.
  Centering is performed inside `runPCA` via `db_svd`.

- scale_cells:

  Not supported for GiottoDB. Forced to `FALSE` with a warning.
  Centering is performed inside `runPCA` via `db_svd`.

## Details

S3 generic that dispatches to
[`Giotto::normalizeGiotto`](https://rdrr.io/pkg/Giotto/man/normalizeGiotto.html)
with optimizations for
[`dbMatrix::dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.html)-backed
expression data.
