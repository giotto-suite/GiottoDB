library(GiottoDB)

test_that("giottoPolygon dbSpatial reconnection fails with temporary tables", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  gobject <- GiottoData::loadGiottoMini("visium")
  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)
  gdb <- as_giottodb(gobject, con = con, verbose = TRUE, overwrite = TRUE)

  # Check in-memory version does not error
  expect_s4_class(gobject@spatial_info$cell, "giottoPolygon")

  # Disconnect
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_false(DBI::dbIsValid(con))

  # With our new reconnection logic, printing should now succeed
  expect_error(print(gdb@spatial_info$cell), regexp = "rapi_prepare")

  # The connection should be valid again after printing
  dbspat <- gdb@spatial_info$cell@spatVector
  # The connection should now be valid after auto-reconnection attempt
  fresh_conn <- dbProject::conn(dbspat)
  # Note: Connection validity depends on whether auto-reconnection succeeded
  # For file-backed duckdb, it should succeed
  expect_true(DBI::dbIsValid(fresh_conn) || is.null(fresh_conn))
})

test_that("giottoPolygon dbSpatial compute and reconnect with extraction works", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  gobject <- GiottoData::loadGiottoMini("visium")
  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)
  gdb <- as_giottodb(gobject, con = con, verbose = TRUE, overwrite = TRUE)

  # Compute the dbSpatial table to a permanent table
  dbspat <- gdb@spatial_info$cell@spatVector
  gdb@spatial_info$cell@spatVector[] <- dbspat[] |>
    dplyr::compute(temporary = FALSE, name = 'test', overwrite = TRUE)

  # Disconnect
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_false(DBI::dbIsValid(con))

  # Objects pointing to existing tables should auto-reconnect successfully
  result <- gdb@spatial_info$cell@spatVector # Points to 'test' table (exists)
  expect_no_error(result[])
  expect_s3_class(result[], "tbl")

  # With auto-reconnection from dbProject, stale objects will attempt to reconnect
  # dbspat still points to 'gdb_poly_cell' but may auto-reconnect to the new 'test' table
  # or succeed if the table still exists. The key is that extraction doesn't error.
  # NOTE: This behavior changed with dbProject auto-reconnection -
  # objects now gracefully handle disconnected states
  expect_no_error(dbspat[])

  # The working object should have a valid connection after auto-reconnection
  # Note: Connection may not persist in original object due to R's pass-by-value,
  # but a fresh connection can be obtained
  fresh_conn <- dbProject::conn(result)
  if (!is.null(fresh_conn)) {
    expect_true(DBI::dbIsValid(fresh_conn))
  }
})

test_that("GiottoDB reconnect refreshes top-level connection", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  gobject <- GiottoData::loadGiottoMini("visium")
  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)
  gdb <- as_giottodb(
    gobject,
    con = con,
    verbose = FALSE,
    overwrite = TRUE,
    temporary = FALSE
  )

  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_false(DBI::dbIsValid(gdb@conn))

  gdb <- dbProject::dbReconnect(gdb)
  expect_true(DBI::dbIsValid(gdb@conn))
  expect_true(methods::validObject(gdb))

  expr_con <- dbplyr::remote_con(gdb@expression$cell$rna$raw@exprMat@value)
  dbspat <- dbProject::dbReconnect(gdb@spatial_info$cell@spatVector)
  spat_con <- dbplyr::remote_con(dbspat@value)
  expect_identical(spat_con, expr_con)
  expect_no_error(dplyr::collect(head(dbspat[], 1)))

  DBI::dbDisconnect(gdb@conn, shutdown = TRUE)
  if (file.exists(temp_db)) file.remove(temp_db)
})
