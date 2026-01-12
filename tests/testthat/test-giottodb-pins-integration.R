# Tests for GiottoDB save/load with pins integration

test_that("saveGiotto/loadGiotto roundtrip preserves GiottoDB", {
  skip_if_not_installed("GiottoClass")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  temp_dir <- tempfile("giottodb_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(temp_dir, "test.duckdb")
  save_dir <- file.path(temp_dir, "saved_giottodb")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dbSpatial::loadSpatial(con)

  # Create expression matrix
  set.seed(42)
  expr_mat <- Matrix::rsparsematrix(100, 50, density = 0.3)
  rownames(expr_mat) <- paste0("gene_", seq_len(100))
  colnames(expr_mat) <- paste0("cell_", seq_len(50))

  # Create Giotto object and convert to GiottoDB
  gobject <- GiottoClass::createGiottoObject(
    expression = expr_mat,
    expression_feat = "rna"
  )
  gdb <- as_giottodb(gobject, con = con)

  # Save
  saveGiotto(
    gdb,
    foldername = basename(save_dir),
    dir = dirname(save_dir),
    overwrite = TRUE,
    verbose = FALSE
  )

  # Verify database directory and file exist
  db_dir <- file.path(save_dir, "Database")
  expect_true(dir.exists(db_dir))
  expect_true(length(list.files(db_dir, pattern = "\\.duckdb$")) >= 1)

  # Load and verify
  gdb_restored <- loadGiotto(save_dir, verbose = FALSE)
  expect_s4_class(gdb_restored, "GiottoDB")
  expect_true(DBI::dbIsValid(gdb_restored@conn))
})

test_that("dbMatrix is preserved through save/load cycle", {
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  temp_dir <- tempfile("dbmatrix_test_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(temp_dir, "test.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create and compute dbMatrix
  expr_mat <- Matrix::rsparsematrix(20, 10, density = 0.3)
  rownames(expr_mat) <- paste0("gene_", 1:20)
  colnames(expr_mat) <- paste0("cell_", 1:10)

  db_mat <- dbMatrix::as.dbMatrix(expr_mat, con = con, name = "test_expr")
  db_mat_scaled <- db_mat * 2
  db_mat_computed <- compute(
    db_mat_scaled,
    name = "computed_expr",
    temporary = FALSE
  )

  # Reload from database
  db_mat_loaded <- dbMatrix::dbLoad(
    con,
    name = "computed_expr",
    class = "dbSparseMatrix"
  )

  expect_s4_class(db_mat_loaded, "dbSparseMatrix")
  expect_equal(dim(db_mat_loaded), dim(expr_mat))
})
