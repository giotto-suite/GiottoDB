# Find marker features one-vs-all with GiottoDB support

S3 wrapper for
[`Giotto::findMarkers_one_vs_all()`](https://rdrr.io/pkg/Giotto/man/findMarkers_one_vs_all.html).
For `GiottoDB` inputs, the specific expression matrix
(`spat_unit`/`feat_type`/`expression_values`) is materialized from
`dbMatrix` to `dgCMatrix` before delegating to the Giotto
implementation. Only the requested matrix is converted; all other slots
remain database-backed.

## Usage

``` r
findMarkers_one_vs_all(gobject, ...)

# S3 method for class 'GiottoDB'
findMarkers_one_vs_all(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  expression_values = c("normalized", "scaled", "custom"),
  ...
)

# S3 method for class 'giotto'
findMarkers_one_vs_all(gobject, ...)
```

## Arguments

- gobject:

  A `giotto` or `GiottoDB` object

- ...:

  Additional arguments passed to
  [`Giotto::findMarkers_one_vs_all`](https://rdrr.io/pkg/Giotto/man/findMarkers_one_vs_all.html)

- spat_unit:

  spatial unit (default: object default)

- feat_type:

  feature type (default: object default)

- expression_values:

  expression values to use

## Value

data.table of marker results

## Note

For methods that call
[`subsetGiotto()`](https://giotto-suite.github.io/GiottoClass/reference/subsetGiotto.html)
internally (e.g. `"gini"`, `"mast"`), this wrapper creates a minimal
in-memory `giotto` marker object from the requested expression matrix
and metadata, instead of converting the full object.
