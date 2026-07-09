# Run PCA on GiottoDB with [`dbMatrix::dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.html) support

S3 generic for PCA that dispatches to
[`dbMatrix::db_svd`](https://dbverse-org.github.io/dbmatrix-r/reference/db_svd.html)
when expression data is stored as
[`dbMatrix::dbMatrix`](https://dbverse-org.github.io/dbmatrix-r/reference/dbMatrix.html).

## Usage

``` r
runPCA(gobject, ...)

# S3 method for class 'GiottoDB'
runPCA(gobject, ...)

# S3 method for class 'giotto'
runPCA(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = "normalized",
  name = "pca",
  feats_to_use = "hvf",
  return_gobject = TRUE,
  ncp = 100,
  center = TRUE,
  scale_unit = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- gobject:

  A
  [`GiottoClass::giotto`](https://giotto-suite.github.io/GiottoClass/reference/giotto-class.html)
  or
  [`GiottoDB`](https://giotto-suite.github.io/GiottoDB/reference/GiottoDB-class.html)
  object

- ...:

  Additional arguments passed to underlying PCA methods

- spat_unit:

  spatial unit

- feat_type:

  feature type

- expression_values:

  expression values to use

- name:

  name of PCA dimension reduction

- feats_to_use:

  features to use for PCA

- return_gobject:

  whether to return giotto object

- ncp:

  number of principal components

- center:

  center data before PCA

- scale_unit:

  scale features before PCA

- verbose:

  verbosity
