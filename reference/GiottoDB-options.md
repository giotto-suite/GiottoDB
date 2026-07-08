# GiottoDB Global Options

GiottoDB sets specific global options to optimize performance for
database-backed operations.

## Options

- giotto.dbmatrix_compute:

  (Logical) Controls whether `normalizeGiotto` automatically
  materializes the result via `compute()` / `.compute_dbMatrix()`.
  Defaults to `TRUE` in GiottoDB to prevent lazy query chain overhead
  during downstream operations like `db_svd`. This is a global option
  implemented in the `Giotto` package but enabled by default in
  `GiottoDB`.
