library(GiottoDB)

test_that("exprObj reconnection works", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  # Setup: Load a mini Giotto object
  gobject <- GiottoData::loadGiottoMini("visium")
  expect_s4_class(gobject, "giotto")

  # Create temporary file for the database
  temp_db <- tempfile(fileext = ".duckdb")

  # Create DuckDB connection
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)

  # Create GiottoDB object
  gdb <- as_giottodb(gobject, con = con, verbose = TRUE, overwrite = TRUE)

  # Check that the result is a GiottoDB class
  expect_s4_class(gdb, "GiottoDB")

  # Check connection is valid
  expect_true(DBI::dbIsValid(con))

  # Disconnect the connection
  DBI::dbDisconnect(con, shutdown = TRUE)

  # Expect connection is not valid
  expect_false(DBI::dbIsValid(con))

  # Properly check that accessing exprObj throws an error
  expect_error(
    print(gdb@expression$cell$rna$raw@exprMat),
    regexp = "rapi_prepare"
  )
})

test_that("exprMat compute and reconnect with empty extract works", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  gobject <- GiottoData::loadGiottoMini("visium")
  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)
  gdb <- as_giottodb(gobject, con = con, verbose = TRUE, overwrite = TRUE)

  # Compute exprMat to a permanent table
  exprMat <- gdb@expression$cell$rna$raw@exprMat
  gdb@expression$cell$rna$raw@exprMat[] <- exprMat[] |>
    dplyr::compute(temporary = FALSE, name = 'test', overwrite = TRUE)

  # Disconnect
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_false(DBI::dbIsValid(con))

  # Should reconnect and return a tbl with empty extract
  expect_s3_class(gdb@expression$cell$rna$raw@exprMat[], "tbl")
})
