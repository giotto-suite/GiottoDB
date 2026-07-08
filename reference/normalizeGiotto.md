# Normalize Expression for GiottoDB with dbMatrix support

For GiottoDB objects with dbMatrix expression, scaling (centering) is
handled implicitly during PCA via `db_svd`. Therefore, `scale_feats` and
`scale_cells` are always set to FALSE for GiottoDB objects.

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

  Ignored for GiottoDB. Always set to FALSE because centering is handled
  implicitly in `runPCA` via `db_svd`.

- scale_cells:

  Ignored for GiottoDB. Always set to FALSE because centering is handled
  implicitly in `runPCA` via `db_svd`.

## Details

S3 generic that dispatches to
[`Giotto::normalizeGiotto`](https://rdrr.io/pkg/Giotto/man/normalizeGiotto.html)
with optimizations for
[`dbMatrix::dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.html)-backed
expression data.
