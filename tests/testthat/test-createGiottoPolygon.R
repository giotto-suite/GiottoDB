# Test createGiottoPolygon with dbSpatial

test_that("createGiottoPolygon works with dbSpatial objects", {
  # Skip if required packages are not available
  skip_if_not_installed("dbSpatial")

  # Create test objects
  db_poly <- dbSpatial:::.sim_dbSpatial(geom = "polygon")

  # Suppress ORDER BY warnings
  suppressWarnings({
    gpolygon <- createGiottoPolygon(db_poly, name = "test_polygons")
  })

  # Basic checks
  expect_s4_class(gpolygon, "giottoPolygon")
  expect_equal(objName(gpolygon), "test_polygons")
  expect_true("dbSpatial" %in% class(gpolygon@spatVector))

  # Test with custom name
  suppressWarnings({
    gpolygon2 <- createGiottoPolygon(db_poly, name = "cell_boundaries")
  })
  expect_equal(objName(gpolygon2), "cell_boundaries")
})


test_that("createGiottoPolygon supports splitting by keyword", {
  skip_if_not_installed("dbSpatial")

  # Create a simulated dbSpatial object
  db_poly <- dbSpatial:::.sim_dbSpatial(geom = "polygon")

  # Modify polygon IDs to include pattern prefixes for testing
  db_pattern_polygons <- db_poly
  db_pattern_polygons[] <- db_poly[] |>
    dplyr::mutate(
      poly_ID = dplyr::case_when(
        dplyr::row_number() %% 3 == 0 ~ paste0("cellA_", poly_ID),
        dplyr::row_number() %% 3 == 1 ~ paste0("cellB_", poly_ID),
        TRUE ~ paste0("other_", poly_ID)
      )
    )

  # Test split by keyword - suppress ORDER BY warnings
  suppressWarnings({
    split_result <- createGiottoPolygon(
      db_pattern_polygons,
      name = "cells",
      split_keyword = list(c("cellA"), c("cellB"))
    )
  })

  # Should return a list of three giottoPolygon objects
  expect_type(split_result, "list")
  expect_length(split_result, 3) # Default group (other_1), cellA group, cellB group

  # Check the names
  expect_equal(objName(split_result[[1]]), "cells_1")
  expect_equal(objName(split_result[[2]]), "cells_2")
  expect_equal(objName(split_result[[3]]), "cells_3")

  # Check that IDs match expected patterns
  # Extract unique_ID_cache for each polygon object
  ids_1 <- split_result[[1]]@unique_ID_cache
  ids_2 <- split_result[[2]]@unique_ID_cache
  ids_3 <- split_result[[3]]@unique_ID_cache

  # Verify the pattern matches
  expect_true(all(grepl("other_", ids_1)))
  expect_true(all(grepl("cellA_", ids_2)))
  expect_true(all(grepl("cellB_", ids_3)))
})

test_that("createGiottoPolygon produces equivalent results to original giottoPolygon", {
  # Skip if required packages are not available
  skip_if_not_installed("dbSpatial")

  # Load a giottoPolygon object from GiottoData
  original_gpolygon <- GiottoData::loadSubObjectMini("giottoPolygon")
  expect_s4_class(original_gpolygon, "giottoPolygon")

  # Extract the terra SpatVector from the giottoPolygon object
  original_spatvector <- original_gpolygon@spatVector
  expect_s4_class(original_spatvector, "SpatVector")

  # Setup in-memory DuckDB database
  tmpfile <- tempfile(fileext = ".duckdb")
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = tmpfile)

  # Directly convert the SpatVector to a dbSpatial object using as_dbSpatial
  db_polygons <- dbSpatial::as_dbSpatial(
    rSpatial = original_spatvector,
    conn = conn,
    name = "polygons_spatial",
    overwrite = TRUE
  )

  # Create a new giottoPolygon object using createGiottoPolygon with the dbSpatial object
  # Suppress ORDER BY warnings
  suppressWarnings({
    db_gpolygon <- createGiottoPolygon(
      db_polygons,
      name = objName(original_gpolygon)
    )
  })

  # Compare properties of both giottoPolygon objects
  expect_s4_class(db_gpolygon, "giottoPolygon")
  expect_equal(objName(db_gpolygon), objName(original_gpolygon))
  expect_true("dbSpatial" %in% class(db_gpolygon@spatVector))

  # Inspect and debug the unique_ID_cache contents
  expect_equal(original_gpolygon@unique_ID_cache, db_gpolygon@unique_ID_cache)

  # Clean up
  DBI::dbDisconnect(conn, shutdown = TRUE)
  unlink(tmpfile)
})
