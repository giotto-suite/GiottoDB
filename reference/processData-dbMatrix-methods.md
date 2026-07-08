# Count Matrix Processing for GiottoDB Objects

processData contains methods for GiottoDB objects that provide various
normalization and scaling operations on count matrices as implemented in
Giotto.

Working methods delegate to existing implementations in Giotto:

- Library normalization (`libraryNormParam`)

- Log normalization (`logNormParam`)

- osmFISH normalization (`osmFISHNormParam`)

- Arcsinh transformation (`arcsinhNormParam`)

- L2 normalization (`l2NormParam`)

- Z-score scaling (`zscoreScaleParam`)

- Default normalization (`defaultNormParam`) - combines library + log

- List processing - enables composable operations

**Unsupported methods**:

- TF-IDF normalization (`tfidfNormParam`)

- Quantile normalization (`quantileNormParam`)

- Pearson residuals normalization (`pearsonResidNormParam`) - requires
  dense intermediate matrices incompatible with the sparse backend

Additionally, `calculateHVF(expression_values = "scaled")` and
`normalizeGiotto(scale_feats = TRUE)` are not supported for `GiottoDB`
objects. Feature centering is handled implicitly during PCA via
`db_svd`.

## Usage

``` r
processData(x, param, ...)
```

## Arguments

- x:

  dbMatrix object

- param:

  S4 parameter class defining the transform operation. Can be:

  - `libraryNormParam` - library size normalization

  - `logNormParam` - log transformation

  - `osmFISHNormParam` - osmFISH normalization

  - `arcsinhNormParam` - arcsinh transformation

  - `l2NormParam` - L2/Euclidean normalization

  - `zscoreScaleParam` - z-score scaling

  - `defaultNormParam` - default normalization (library + log)

  - `list` - for chained operations

  - `tfidfNormParam` - **not supported**

  - `quantileNormParam` - **not supported**

  - `pearsonResidNormParam` - **not supported**

- ...:

  additional params to pass to the underlying methods

## Value

A dbMatrix object

## Details

All results are lazily evaluated. Please read
[dbplyr::collapse.tbl_sql](https://dbplyr.tidyverse.org/reference/collapse.tbl_sql.html)
to compute/save results to the db.

## See also

[`processData`](https://rdrr.io/pkg/Giotto/man/processData.html) for the
generic and other methods

[`processExpression`](https://rdrr.io/pkg/Giotto/man/processExpression.html)
for use with giotto objects

[`normParam`](https://rdrr.io/pkg/Giotto/man/process_param.html),
[`scaleParam`](https://rdrr.io/pkg/Giotto/man/process_param.html) for
parameter creation

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a dbMatrix object
library(dbMatrix)
con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
mat <- matrix(rpois(100, 5), nrow = 10)
dbmat <- dbMatrix(mat, con = con, name = "test")

# Library normalization
lib_norm <- processData(dbmat, normParam("library"))

# Log normalization
log_norm <- processData(dbmat, normParam("log"))

# Chained operations
scaled <- processData(dbmat, list(
  normParam("library"),
  normParam("log"),
  scaleParam("zscore")
))
} # }
```
