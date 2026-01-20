# Ensure saveGiotto.GiottoDB repoints dbProject cachedConnection after DB move

test_that("saveGiotto updates dbProject cachedConnection after moving DB", {
  skip_if_not_installed("GiottoClass")
  skip_if_not_installed("dbProject")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("pins")
  skip_if_not_installed("connections")

  temp_dir <- tempfile("giottodb_dbproject_update_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(temp_dir, "test.duckdb")
  project_dir <- file.path(temp_dir, "project_pins")
  save_dir <- file.path(temp_dir, "saved_giottodb")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dbSpatial::loadSpatial(con)

  expr_mat <- Matrix::rsparsematrix(10, 6, density = 0.3)
  rownames(expr_mat) <- paste0("gene_", seq_len(nrow(expr_mat)))
  colnames(expr_mat) <- paste0("cell_", seq_len(ncol(expr_mat)))

  gobject <- GiottoClass::createGiottoObject(
    expression = expr_mat,
    expression_feat = "rna"
  )
  gdb <- as_giottodb(gobject, con = con, verbose = FALSE, temporary = TRUE)

  # Create a dbProject board near the original DB path
  proj <- dbProject::dbProject$new(path = project_dir, dbdir = db_path)
  proj$disconnect()

  saveGiotto(
    gdb,
    foldername = basename(save_dir),
    dir = dirname(save_dir),
    overwrite = TRUE,
    verbose = FALSE
  )

  new_db_path <- file.path(save_dir, "Database", basename(db_path))
  expect_true(file.exists(new_db_path))
  expect_false(file.exists(db_path))

  board <- pins::board_folder(project_dir, versioned = TRUE)
  conn_wrap <- connections::connection_pin_read(board = board, name = "cachedConnection")
  on.exit(DBI::dbDisconnect(conn_wrap@con, shutdown = TRUE), add = TRUE)

  actual_dbname <- DBI::dbGetInfo(conn_wrap@con)$dbname
  expect_equal(
    normalizePath(actual_dbname, mustWork = FALSE),
    normalizePath(new_db_path, mustWork = FALSE)
  )

  expect_no_error(DBI::dbGetQuery(conn_wrap@con, "SELECT 1 AS one"))
})
