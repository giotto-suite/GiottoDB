# Test createGiottoPoints with dbSpatial

test_that("createGiottoPoints works with dbSpatial objects", {
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  # Setup in-memory DuckDB database
  tmpfile <- tempfile(fileext = ".duckdb")
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = tmpfile)
  on.exit({
    DBI::dbDisconnect(conn, shutdown = TRUE)
    unlink(tmpfile)
  }, add = TRUE)

  # Create a minimal points table with required columns
  points_df <- data.frame(
    x = c(1, 2, 3),
    y = c(10, 20, 30),
    feat_ID = c("gene_1", "gene_2", "gene_3")
  )

  db_points <- dbSpatial::dbSpatial(
    value = points_df,
    name = "points",
    conn = conn,
    x_colName = "x",
    y_colName = "y",
    overwrite = TRUE
  )

  # Test basic creation - suppress ORDER BY warnings
  suppressWarnings({
    gpoints <- createGiottoPoints(db_points)
  })

  # Basic checks
  expect_s4_class(gpoints, "giottoPoints")
  expect_equal(gpoints@feat_type, "rna")
  expect_true("dbSpatial" %in% class(gpoints@spatVector))

  # Test with custom feat_type - suppress ORDER BY warnings
  suppressWarnings({
    gpoints2 <- createGiottoPoints(db_points, feat_type = "protein")
  })
  expect_equal(gpoints2@feat_type, "protein")
})

test_that("createGiottoPoints produces equivalent results to original giottoPoints", {
  # Skip if required packages are not available
  skip_if_not_installed("dbSpatial")

  # Load a giottoPoints object from GiottoData
  original_gpoints <- GiottoData::loadSubObjectMini("giottoPoints")
  expect_s4_class(original_gpoints, "giottoPoints")

  # Extract the terra SpatVector from the giottoPoints object
  original_spatvector <- original_gpoints@spatVector
  expect_s4_class(original_spatvector, "SpatVector")

  # Setup in-memory DuckDB database
  tmpfile <- tempfile(fileext = ".duckdb")
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = tmpfile)

  # Directly convert the SpatVector to a dbSpatial object using as_dbSpatial
  db_points <- dbSpatial::as_dbSpatial(
    rSpatial = original_spatvector,
    conn = conn,
    name = "points_spatial",
    overwrite = TRUE
  )

  # Create a new giottoPoints object using createGiottoPoints with the dbSpatial object
  # Suppress ORDER BY warnings
  suppressWarnings({
    db_gpoints <- createGiottoPoints(
      db_points,
      feat_type = original_gpoints@feat_type
    )
  })

  # Compare properties of both giottoPoints objects
  expect_s4_class(db_gpoints, "giottoPoints")
  expect_equal(db_gpoints@feat_type, original_gpoints@feat_type)
  expect_true("dbSpatial" %in% class(db_gpoints@spatVector))

  # Ensure unique_ID_cache contents equal
  expect_equal(
    head(original_gpoints@unique_ID_cache, 5),
    head(db_gpoints@unique_ID_cache, 5)
  )

  # The number of unique IDs should be similar (allow for some differences)
  expect_equal(
    length(original_gpoints@unique_ID_cache),
    length(db_gpoints@unique_ID_cache)
  )

  # Clean up
  DBI::dbDisconnect(conn, shutdown = TRUE)
  unlink(tmpfile)
})
