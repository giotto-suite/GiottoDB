library(GiottoDB)
library(testthat)

test_that("GiottoDB save/load reconnects multiple expression entries", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  # Setup: mini visium fixture already provides both `raw` and `normalized`
  # expression entries, so this directly exercises a multi-entry save path.
  gobject <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)

  gdb <- as_giottodb(gobject, con = con, verbose = FALSE)

  # Confirm both entries exist pre-save
  expect_true("raw" %in% names(gdb@expression$cell$rna))
  expect_true("normalized" %in% names(gdb@expression$cell$rna))

  pre_dim_raw  <- dim(gdb@expression$cell$rna$raw@exprMat)
  pre_dim_norm <- dim(gdb@expression$cell$rna$normalized@exprMat)

  # Save
  save_dir <- tempfile()
  dir.create(save_dir)
  saveGiotto(gdb, foldername = "test_save", dir = save_dir, verbose = FALSE)

  # Load
  saved_path <- file.path(save_dir, "test_save")
  gdb_loaded <- loadGiotto(saved_path, verbose = FALSE)

  # Both expression entries must reconnect to real dbMatrices
  raw_mat  <- gdb_loaded@expression$cell$rna$raw@exprMat
  norm_mat <- gdb_loaded@expression$cell$rna$normalized@exprMat

  expect_s4_class(raw_mat, "dbSparseMatrix")
  expect_s4_class(norm_mat, "dbSparseMatrix")
  expect_equal(dim(raw_mat), pre_dim_raw)
  expect_equal(dim(norm_mat), pre_dim_norm)

  # Cleanup
  DBI::dbDisconnect(con, shutdown = TRUE)
  DBI::dbDisconnect(gdb_loaded@conn, shutdown = TRUE)
  unlink(save_dir, recursive = TRUE)
  unlink(temp_db)
})
