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

  gobject <- GiottoData::loadGiottoMini("visium")
  expect_s4_class(gobject, "giotto")

  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  attr(con, "dbdir") <- temp_db
  dbSpatial::loadSpatial(con)

  gdb <- as_giottodb(gobject, con = con, verbose = TRUE)

  expect_true(DBI::dbIsValid(con))

  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_false(DBI::dbIsValid(con))

  expr_dbm <- gdb@expression$cell$rna$raw
  expect_s4_class(expr_dbm, "exprObj")
  expect_s4_class(expr_dbm@exprMat, "dbSparseMatrix")

  # This should trigger reconnection and not error
  # [] triggers auto-reconnect via dbReconnect().
  testthat::expect_no_error({
    result <- expr_dbm@exprMat[]
  })

  collected <- dplyr::collect(head(expr_dbm@exprMat[], 5))
  expect_true(nrow(collected) >= 0)

  # Clean up
  if (file.exists(temp_db)) file.remove(temp_db)
})

test_that("dbReconnect bypasses conn() auto-reconnect bug", {
  # Regression: dbReconnect() must not use conn(x) (auto-reconnect would mask staleness).
  skip_if_not_installed("dbMatrix")

  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  attr(con, "dbdir") <- temp_db

  # Create a small dbSparseMatrix
  mat <- Matrix::sparseMatrix(
    i = c(1, 2, 3), j = c(1, 2, 3), x = c(10, 20, 30),
    dims = c(3, 3),
    dimnames = list(c("a", "b", "c"), c("x", "y", "z"))
  )
  dbm <- dbMatrix::dbMatrix(
    value = mat, class = "dbSparseMatrix",
    con = con, name = "test_recon", overwrite = TRUE
  )

  # Kill the connection
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_false(DBI::dbIsValid(con))

  # dbReconnect should detect staleness and update @value
  dbm_recon <- dbProject::dbReconnect(dbm)

  # The reconnected object should have a valid tbl
  recon_con <- dbProject::conn(dbm_recon)
  expect_true(DBI::dbIsValid(recon_con))

  # Should be able to collect data
  collected <- dplyr::collect(dbm_recon[])
  expect_equal(nrow(collected), 3)

  # Clean up
  DBI::dbDisconnect(recon_con, shutdown = TRUE)
  if (file.exists(temp_db)) file.remove(temp_db)
})
