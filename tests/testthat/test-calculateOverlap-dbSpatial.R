test_that("calculateOverlap(dbSpatial, dbSpatial) works on as_giottodb(vizgen)", {
  gobject <- suppressWarnings(GiottoData::loadGiottoMini("vizgen"))

  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  dbSpatial::loadSpatial(conn)

  gdb <- GiottoDB::as_giottodb(gobject, con = conn, verbose = FALSE, overwrite = TRUE)

  poly_names <- GiottoClass::list_spatial_info_names(gdb)
  feat_names <- GiottoClass::list_feature_info_names(gdb)

  gpoly <- GiottoClass::getPolygonInfo(
    gdb,
    polygon_name = poly_names[[1]],
    return_giottoPolygon = TRUE,
    verbose = FALSE,
    simplify = TRUE
  )
  gpoints <- GiottoClass::getFeatureInfo(
    gdb,
    feat_type = feat_names[[1]],
    return_giottoPoints = TRUE,
    simplify = TRUE
  )

  poly_db <- gpoly@spatVector
  pts_obj <- gpoints@spatVector

  expect_true(inherits(poly_db, "dbSpatial"))

  # GiottoDB::as_giottodb() converts giottoPolygon (spatial_info) and giottoPoints
  # (feat_info) spatVector backends to dbSpatial.
  expect_true(inherits(pts_obj, "dbSpatial"))
  pts_db <- pts_obj

  # Get one known overlapping (poly_ID, feat_ID) pair so the subset tests are stable.
  res0 <- GiottoDB::calculateOverlap(poly_db, pts_db, verbose = FALSE)
  pair <- dplyr::collect(
    res0[] |>
      dplyr::filter(!is.na(poly_ID)) |>
      dplyr::select(poly_ID, feat_ID) |>
      utils::head(1)
  )

  poly_id <- pair$poly_ID[[1]]
  feat_id <- pair$feat_ID[[1]]

  res_new <- GiottoDB::calculateOverlap(
    poly_db,
    pts_db,
    poly_subset_ids = poly_id,
    feat_subset_column = "feat_ID",
    feat_subset_values = feat_id,
    verbose = FALSE
  )
  n_new <- nrow(dplyr::collect(res_new[]))
  expect_gt(n_new, 0)
  poly_new <- dplyr::collect(res_new[] |> dplyr::distinct(poly_ID))
  expect_equal(nrow(poly_new), 1)
  expect_equal(as.character(poly_new$poly_ID[[1]]), as.character(poly_id))

  res_old <- GiottoDB::calculateOverlap(
    poly_db,
    pts_db,
    poly_subset_ids = poly_id,
    feat_subset_column = "feat_ID",
    feat_subset_ids = feat_id,
    verbose = FALSE
  )
  n_old <- nrow(dplyr::collect(res_old[]))
  expect_equal(n_old, n_new)
  poly_old <- dplyr::collect(res_old[] |> dplyr::distinct(poly_ID))
  expect_equal(nrow(poly_old), 1)
  expect_equal(as.character(poly_old$poly_ID[[1]]), as.character(poly_id))
})
