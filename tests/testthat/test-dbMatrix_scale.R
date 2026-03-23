# Tests for normalizeGiotto scale suppression on GiottoDB objects.
#
# GiottoDB does not support scale_feats/scale_cells: centering is handled
# implicitly in runPCA via db_svd. normalizeGiotto.GiottoDB warns and forces
# both to FALSE.

library(GiottoDB)
library(Giotto)

test_that("normalizeGiotto.GiottoDB warns when scale_feats = TRUE", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  gdb <- as_giottodb(g, con = con, verbose = FALSE)

  expect_warning(
    normalizeGiotto(gdb,
      scale_feats = TRUE, scale_cells = FALSE,
      library_size_norm = FALSE, log_norm = FALSE, verbose = FALSE
    ),
    regexp = "scale_feats|scale_cells|not supported",
    ignore.case = TRUE
  )
})

test_that("normalizeGiotto.GiottoDB scale_feats=TRUE produces same output as scale_feats=FALSE", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  gdb <- as_giottodb(g, con = con, verbose = FALSE)

  # scale_feats=TRUE is forced to FALSE by GiottoDB dispatch (with warning)
  gdb_a <- suppressWarnings(normalizeGiotto(gdb,
    scale_feats = TRUE, scale_cells = FALSE,
    library_size_norm = TRUE, log_norm = TRUE, verbose = FALSE
  ))
  mat_a <- as.matrix(GiottoClass::getExpression(gdb_a, values = "normalized", output = "matrix"))

  # scale_feats=FALSE explicitly — identical behaviour expected
  gdb_b <- suppressWarnings(normalizeGiotto(gdb,
    scale_feats = FALSE, scale_cells = FALSE,
    library_size_norm = TRUE, log_norm = TRUE, verbose = FALSE
  ))
  mat_b <- as.matrix(GiottoClass::getExpression(gdb_b, values = "normalized", output = "matrix"))

  expect_equal(mat_a, mat_b, tolerance = 1e-12)
})
