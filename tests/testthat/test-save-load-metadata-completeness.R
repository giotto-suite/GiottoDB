library(GiottoDB)
library(testthat)

test_that("dbmatrix_metadata.rds matches expression slots and persisted DuckDB tables", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  # Setup: mini visium fixture already provides both `raw` and `normalized`
  # expression entries, exercising a multi-entry metadata write path.
  gobject <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)

  gdb <- as_giottodb(gobject, con = con, verbose = FALSE)

  # Enumerate every spat_unit/feat_type/expr_name path whose exprMat is a
  # dbSparseMatrix (the only entries that the save loop is responsible for
  # persisting via dbmatrix_metadata).
  slot_names <- character()
  for (su in names(gdb@expression)) {
    for (ft in names(gdb@expression[[su]])) {
      for (en in names(gdb@expression[[su]][[ft]])) {
        m <- gdb@expression[[su]][[ft]][[en]]@exprMat
        if (methods::is(m, "dbSparseMatrix") || methods::is(m, "dbDenseMatrix")) {
          slot_names <- c(slot_names, paste(su, ft, en, sep = "/"))
        }
      }
    }
  }
  slot_names <- sort(slot_names)

  # Save
  save_dir <- tempfile()
  dir.create(save_dir)
  saveGiotto(gdb, foldername = "test_save", dir = save_dir, verbose = FALSE)

  saved_path <- file.path(save_dir, "test_save")
  db_dir <- file.path(saved_path, "Database")

  # Read metadata
  md_file <- file.path(db_dir, "dbmatrix_metadata.rds")
  expect_true(file.exists(md_file))
  md <- readRDS(md_file)

  # Enumerate metadata-recorded paths
  md_names <- character()
  ref_tables <- character()
  for (su in names(md)) {
    for (ft in names(md[[su]])) {
      for (en in names(md[[su]][[ft]])) {
        md_names <- c(md_names, paste(su, ft, en, sep = "/"))
        ref_tables <- c(ref_tables, md[[su]][[ft]][[en]]$table_name)
      }
    }
  }
  md_names <- sort(md_names)

  # Every expression slot is represented in metadata
  expect_equal(md_names, slot_names)

  # Every referenced table_name exists in the saved DuckDB
  db_files <- list.files(db_dir, pattern = "\\.(db|duckdb)$", full.names = TRUE)
  expect_length(db_files, 1)

  DBI::dbDisconnect(con, shutdown = TRUE)

  new_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_files[1])
  db_tables <- DBI::dbListTables(new_con)
  expect_true(all(ref_tables %in% db_tables))

  # Cleanup
  DBI::dbDisconnect(new_con, shutdown = TRUE)
  unlink(save_dir, recursive = TRUE)
  unlink(temp_db)
})
