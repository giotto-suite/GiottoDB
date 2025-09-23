library(GiottoDB)

test_that("giottoPolygon dbSpatial reconnection fails with temporary tables", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  options("giotto.use_conda" = FALSE)
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
  expect_false(DBI::dbIsValid(dbspat@conn))
})

test_that("giottoPolygon dbSpatial compute and reconnect with extraction works", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  options("giotto.use_conda" = FALSE)
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

  # Should now successfully reconnect thanks to our implementation
  result <- gdb@spatial_info$cell@spatVector
  result[] <- gdb@spatial_info$cell@spatVector[]
  expect_s3_class(result[], "tbl")

  # And the connection should be valid again
  expect_true(DBI::dbIsValid(result@conn))
})
