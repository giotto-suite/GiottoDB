# Test reconnection of dbMatrix objects inside GiottoDB
library(testthat)
library(dbMatrix)
library(GiottoData)
library(dbSpatial)
library(dbProject)

test_that("dbMatrix reconnection works inside GiottoDB", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")

  options("giotto.use_conda" = FALSE)
  gobject <- GiottoData::loadGiottoMini("visium")
  expect_s4_class(gobject, "giotto")

  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)

  gdb <- as_giottodb(gobject, con = con, verbose = TRUE)

  # Confirm connection is valid
  expect_true(DBI::dbIsValid(con))

  # Disconnect
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_false(DBI::dbIsValid(con))

  # Try extracting the dbMatrix (should trigger reconnection logic)
  expr_dbm <- gdb@expression$cell$rna$raw
  expect_s4_class(expr_dbm, "exprObj")
  expect_s4_class(expr_dbm@exprMat, "dbSparseMatrix")

  # This should trigger reconnection and not error
  testthat::expect_no_error({
    expr_dbm@exprMat[]
  })

  # Clean up
  if (file.exists(temp_db)) file.remove(temp_db)
})
