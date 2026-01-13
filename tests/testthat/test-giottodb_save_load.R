library(GiottoDB)
library(testthat)

test_that("GiottoDB save/load works", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  # Setup
  gobject <- GiottoData::loadGiottoMini("visium")
  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)

  # Create GiottoDB object
  gdb <- as_giottodb(gobject, con = con, verbose = FALSE)

  # Save
  save_dir <- tempfile()
  dir.create(save_dir)

  # Use generic saveGiotto - should dispatch to saveGiotto.GiottoDB
  saveGiotto(gdb, foldername = "test_save", dir = save_dir, verbose = TRUE)

  # Verify save structure
  saved_path <- file.path(save_dir, "test_save")
  expect_true(dir.exists(saved_path))
  expect_true(dir.exists(file.path(saved_path, "Database")))
  expect_true(file.exists(file.path(saved_path, "Database", basename(temp_db))))
  expect_true(file.exists(file.path(
    saved_path,
    "Database",
    "dbspatial_metadata.rds"
  )))
  expect_true(file.exists(file.path(
    saved_path,
    "Database",
    "dbmatrix_metadata.rds"
  )))

  # Load
  gdb_loaded <- loadGiotto(saved_path, verbose = FALSE)

  # Verify loaded object
  expect_s4_class(gdb_loaded, "GiottoDB")
  expect_true(DBI::dbIsValid(gdb_loaded@conn))

  # Verify spatial info reconnected
  expect_s4_class(gdb_loaded@spatial_info$cell@spatVector, "dbSpatial")
  expect_true(DBI::dbIsValid(dbProject::conn(gdb_loaded@spatial_info$cell@spatVector)))

  # Cleanup
  DBI::dbDisconnect(con, shutdown = TRUE)
  DBI::dbDisconnect(gdb_loaded@conn, shutdown = TRUE)
  unlink(save_dir, recursive = TRUE)
  unlink(temp_db)
})

test_that("loadGiottoDB works", {
  skip_if_not_installed("GiottoData")

  gobject <- GiottoData::loadGiottoMini("visium")
  save_dir <- tempfile()
  dir.create(save_dir)

  # Save standard Giotto
  GiottoClass::saveGiotto(
    gobject,
    foldername = "test_save_std",
    dir = save_dir,
    verbose = FALSE
  )
  saved_path <- file.path(save_dir, "test_save_std")

  # Load as GiottoDB with new connection
  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)

  gdb_loaded <- loadGiottoDB(saved_path, con = con, verbose = FALSE)

  expect_s4_class(gdb_loaded, "GiottoDB")
  expect_true(DBI::dbIsValid(gdb_loaded@conn))

  DBI::dbDisconnect(con, shutdown = TRUE)
  unlink(save_dir, recursive = TRUE)
  unlink(temp_db)
})
