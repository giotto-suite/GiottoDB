# Test createGiottoPolygon with dbSpatial

test_that("createGiottoPolygon works with dbSpatial objects", {
  # Skip if required packages are not available
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("sf")

  # Setup in-memory DuckDB database
  tmpfile <- tempfile(fileext = ".duckdb")
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = tmpfile)
  on.exit({
    DBI::dbDisconnect(conn, shutdown = TRUE)
    unlink(tmpfile)
  }, add = TRUE)

  # Create test objects
  poly_sf <- sf::st_sf(
    poly_ID = c("poly_1", "poly_2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
      ))),
      sf::st_polygon(list(rbind(
        c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2)
      )))
    )
  )

  db_poly <- dbSpatial::as_dbSpatial(
    rSpatial = poly_sf,
    conn = conn,
    name = "polygons",
    overwrite = TRUE
  )

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
  skip_if_not_installed("duckdb")
  skip_if_not_installed("sf")

  # Setup in-memory DuckDB database
  tmpfile <- tempfile(fileext = ".duckdb")
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = tmpfile)
  on.exit({
    DBI::dbDisconnect(conn, shutdown = TRUE)
    unlink(tmpfile)
  }, add = TRUE)

  # Create a simulated dbSpatial object
  poly_sf <- sf::st_sf(
    poly_ID = paste0("poly_", seq_len(10)),
    geometry = sf::st_sfc(lapply(seq_len(10), function(i) {
      x0 <- i * 10
      y0 <- i * 10
      sf::st_polygon(list(rbind(
        c(x0, y0), c(x0 + 1, y0), c(x0 + 1, y0 + 1), c(x0, y0 + 1), c(x0, y0)
      )))
    }))
  )

  db_poly <- dbSpatial::as_dbSpatial(
    rSpatial = poly_sf,
    conn = conn,
    name = "polygons_for_split",
    overwrite = TRUE
  )

  # Instead of complex case_when with string concatenation,
  # use simple prefixes that DuckDB can handle
  db_pattern_polygons <- db_poly

  # Test split by keyword using subsets of existing poly_IDs
  # Get a few sample poly_IDs to use as keywords
  sample_ids <- db_pattern_polygons[] |>
    dplyr::select(poly_ID) |>
    head(6) |>
    dplyr::collect() |>
    dplyr::pull(poly_ID)

  keyword1 <- sample_ids[1:2] # First 2 IDs
  keyword2 <- sample_ids[3:4] # Next 2 IDs

  # Test split by keyword - suppress ORDER BY warnings
  suppressWarnings({
    split_result <- createGiottoPolygon(
      db_pattern_polygons,
      name = "cells",
      split_keyword = list(keyword1, keyword2)
    )
  })

  # Should return a list of three giottoPolygon objects
  expect_type(split_result, "list")
  expect_length(split_result, 3) # Default group, keyword1 group, keyword2 group

  # Check the names
  expect_equal(objName(split_result[[1]]), "cells_1")
  expect_equal(objName(split_result[[2]]), "cells_2")
  expect_equal(objName(split_result[[3]]), "cells_3")

  # Check that splitting worked by verifying the polygon counts sum correctly
  total_original <- db_pattern_polygons[] |> dplyr::count() |> dplyr::pull(n)
  total_split <- length(split_result[[1]]@unique_ID_cache) +
    length(split_result[[2]]@unique_ID_cache) +
    length(split_result[[3]]@unique_ID_cache)
  expect_equal(total_original, total_split)
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
