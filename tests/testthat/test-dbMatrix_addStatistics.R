# Test addStatistics with dbMatrix support

# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")

skip_if_not_installed("dbMatrix")
skip_if_not_installed("Matrix")
skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")
skip_if_not_installed("GiottoClass")
skip_if_not_installed("GiottoData")

library(testthat)
library(Giotto)
library(GiottoDB) # Load GiottoDB to ensure methods are registered (if using S3 approach later)
library(dbMatrix)
library(Matrix)
library(duckdb)
library(DBI)
library(data.table)

# ---------------------------------------------------------------------------- #
# Setup data & Objects

# Create temp file and connection, ensure cleanup
con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
# Ensure disconnection on exit/error
on.exit(
  {
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
  },
  add = TRUE
)

# Load mini visium dataset (acts as the standard Matrix version)
# Ensure it's initialized and has normalized data
options("giotto.use_conda" = FALSE)
gobject <- GiottoData::loadGiottoMini(dataset = "visium", verbose = FALSE)
gobject_mat <- getExpression(gobject, output = 'matrix', values = 'normalized')

# Create dbMatrix version
con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
dbsm <- dbMatrix::dbMatrix(
  value = gobject_mat,
  con = con,
  name = "gobject_mat",
  class = "dbSparseMatrix",
  overwrite = TRUE
)

gobject_mat_db <- Giotto::createExprObj(
  expression_data = dbsm,
  expression_matrix_class = "dbMatrix",
  name = "normalized"
)

gobject_db <- gobject
gobject_db <- setExpression(gobject_db, x = gobject_mat_db)

# ---------------------------------------------------------------------------- #
# Run addStatistics

test_that("addStatistics (feature & cell) works equivalently for Matrix and dbMatrix", {
  # Run on Matrix object
  gobject_matrix_stats <- Giotto::addStatistics(
    gobject = gobject,
    expression_values = "normalized",
    detection_threshold = 0,
    verbose = FALSE
  )

  # Run on dbMatrix object
  # Assuming GiottoDB is loaded OR the vectorized changes are in Giotto directly
  gobject_db_stats <- Giotto::addStatistics(
    gobject = gobject,
    expression_values = "normalized",
    detection_threshold = 0,
    verbose = FALSE
  )

  # Compare Feature Statistics
  f_meta_matrix <- GiottoClass::fDataDT(gobject_matrix_stats)
  f_meta_db <- GiottoClass::fDataDT(gobject_db_stats)

  # Order by feature ID to ensure comparison is valid
  data.table::setorder(f_meta_matrix, feat_ID)
  data.table::setorder(f_meta_db, feat_ID)

  # Select only the stats columns added by addFeatStatistics
  stats_cols_f <- c(
    "feat_ID",
    "nr_cells",
    "perc_cells",
    "total_expr",
    "mean_expr",
    "mean_expr_det"
  )
  # Use tolerance due to potential floating point differences
  expect_equal(
    f_meta_matrix[, ..stats_cols_f],
    f_meta_db[, ..stats_cols_f],
    tolerance = 1e-6
  )

  # Compare Cell Statistics
  c_meta_matrix <- GiottoClass::pDataDT(gobject_matrix_stats)
  c_meta_db <- GiottoClass::pDataDT(gobject_db_stats)

  # Order by cell ID
  data.table::setorder(c_meta_matrix, cell_ID)
  data.table::setorder(c_meta_db, cell_ID)

  # Select only the stats columns added by addCellStatistics
  stats_cols_c <- c("cell_ID", "nr_feats", "perc_feats", "total_expr")
  # Use tolerance due to potential floating point differences
  expect_equal(
    c_meta_matrix[, ..stats_cols_c],
    c_meta_db[, ..stats_cols_c],
    tolerance = 1e-6
  )
})

test_that("addStatistics (feature & cell) works with detection_threshold = 1", {
  # Run on Matrix object
  gobject_matrix_stats <- Giotto::addStatistics(
    gobject = gobject,
    expression_values = "normalized",
    detection_threshold = 1,
    verbose = FALSE
  )

  # Run on dbMatrix object
  # Assuming GiottoDB is loaded OR the vectorized changes are in Giotto directly
  gobject_db_stats <- Giotto::addStatistics(
    gobject = gobject,
    expression_values = "normalized",
    detection_threshold = 1,
    verbose = FALSE
  )

  # Compare Feature Statistics
  f_meta_matrix <- GiottoClass::fDataDT(gobject_matrix_stats)
  f_meta_db <- GiottoClass::fDataDT(gobject_db_stats)

  # Order by feature ID to ensure comparison is valid
  data.table::setorder(f_meta_matrix, feat_ID)
  data.table::setorder(f_meta_db, feat_ID)

  # Select only the stats columns added by addFeatStatistics
  stats_cols_f <- c(
    "feat_ID",
    "nr_cells",
    "perc_cells",
    "total_expr",
    "mean_expr",
    "mean_expr_det"
  )
  # Use tolerance due to potential floating point differences
  expect_equal(
    f_meta_matrix[, ..stats_cols_f],
    f_meta_db[, ..stats_cols_f],
    tolerance = 1e-6
  )

  # Compare Cell Statistics
  c_meta_matrix <- GiottoClass::pDataDT(gobject_matrix_stats)
  c_meta_db <- GiottoClass::pDataDT(gobject_db_stats)

  # Order by cell ID
  data.table::setorder(c_meta_matrix, cell_ID)
  data.table::setorder(c_meta_db, cell_ID)

  # Select only the stats columns added by addCellStatistics
  stats_cols_c <- c("cell_ID", "nr_feats", "perc_feats", "total_expr")
  # Use tolerance due to potential floating point differences
  expect_equal(
    c_meta_matrix[, ..stats_cols_c],
    c_meta_db[, ..stats_cols_c],
    tolerance = 1e-6
  )
})
