# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

# ---------------------------------------------------------------------------- #
# Setup data
test_that("calculateHVF with dbMatrix integration works correctly", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")

  # Load test data
  visium <- GiottoData::loadGiottoMini(dataset = "visium", init_gobject = FALSE)

  # Create dbMatrix version
  dgc <- getExpression(visium, values = "normalized", output = "matrix")

  # Create dbSparseMatrix
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  dbsm <- dbMatrix::dbMatrix(
    value = dgc,
    con = con,
    name = "dgc",
    class = "dbSparseMatrix",
    overwrite = TRUE
  )

  # Create exprObj with dbsm
  expObj_db <- createExprObj(
    expression_data = dbsm,
    expression_matrix_class = "dbMatrix",
    name = "normalized"
  )

  # Create giotto object with dbMatrix
  gobject_db <- suppressWarnings(createGiottoObject(expression = expObj_db))

  # ---------------------------------------------------------------------------- #
  # Test for method = "var_p_resid"

  # Apply calculateHVF to regular matrix
  visium_hvf <- calculateHVF(
    visium,
    method = "var_p_resid",
    expression_values = "normalized",
    verbose = FALSE
  )

  # Apply calculateHVF to dbMatrix (should use GiottoDB implementation)
  gobject_db_hvf <- calculateHVF(
    gobject_db,
    method = "var_p_resid",
    expression_values = "normalized",
    verbose = FALSE
  )

  # Get feature metadata with HVF results
  hvf_standard <- fDataDT(visium_hvf)
  hvf_dbMatrix <- fDataDT(gobject_db_hvf)

  # Test that results are equivalent
  expect_equal(hvf_standard$hvf, hvf_dbMatrix$hvf)

  # ---------------------------------------------------------------------------- #
  # Test for method = "cov_groups"

  # Apply calculateHVF to regular matrix
  visium_hvf_cov <- calculateHVF(
    visium,
    method = "cov_groups",
    expression_values = "normalized",
    verbose = FALSE
  )

  # Apply calculateHVF to dbMatrix (should use GiottoDB implementation)
  gobject_db_hvf_cov <- calculateHVF(
    gobject_db,
    method = "cov_groups",
    expression_values = "normalized",
    verbose = FALSE
  )

  # Get feature metadata with HVF results
  hvf_standard_cov <- fDataDT(visium_hvf_cov)
  hvf_dbMatrix_cov <- fDataDT(gobject_db_hvf_cov)

  # Test that results are equivalent
  expect_equal(hvf_standard_cov$hvf, hvf_dbMatrix_cov$hvf)

  # ---------------------------------------------------------------------------- #
  # Test for method = "cov_loess"

  # Apply calculateHVF to regular matrix
  visium_hvf_loess <- calculateHVF(
    visium,
    method = "cov_loess",
    expression_values = "normalized",
    verbose = FALSE
  )

  # Apply calculateHVF to dbMatrix (should use GiottoDB implementation)
  gobject_db_hvf_loess <- calculateHVF(
    gobject_db,
    method = "cov_loess",
    expression_values = "normalized",
    verbose = FALSE
  )

  # Get feature metadata with HVF results
  hvf_standard_loess <- fDataDT(visium_hvf_loess)
  hvf_dbMatrix_loess <- fDataDT(gobject_db_hvf_loess)

  # Test that results are equivalent (allow for small numerical differences)
  expect_equal(hvf_standard_loess$hvf, hvf_dbMatrix_loess$hvf, tolerance = 1e-6)

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})
