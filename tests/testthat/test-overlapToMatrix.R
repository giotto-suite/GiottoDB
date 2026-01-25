test_that("overlapToMatrix works", {
  gpolys <- suppressWarnings(GiottoData::loadSubObjectMini("giottoPolygon"))
  gpoints <- suppressWarnings(GiottoData::loadSubObjectMini("giottoPoints"))

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  dbSpatial::loadSpatial(con)
  dbs_polys <- dbSpatial::as_dbSpatial(
    rSpatial = gpolys[],
    conn = con,
    name = "gpolys"
  )
  dbs_points <- dbSpatial::as_dbSpatial(
    rSpatial = gpoints[],
    conn = con,
    name = "gpoints"
  )

  # test Giotto
  res_giotto <- GiottoClass::calculateOverlap(
    x = gpolys,
    y = gpoints
  )

  res_mat <- GiottoClass::overlapToMatrix(x = res_giotto)

  # test dbSpatial
  res_giotto <- GiottoClass::calculateOverlap(
    x = dbs_polys,
    y = dbs_points
  )

  res_dbs_mat <- GiottoDB::overlapToMatrix(
    x = res_giotto,
    output = "matrix",
    row_names = rownames(res_mat),
    col_names = colnames(res_mat),
    verbose = FALSE
  )

  rn <- intersect(rownames(res_mat), rownames(res_dbs_mat))
  cn <- intersect(colnames(res_mat), colnames(res_dbs_mat))
  expect_true(setequal(rownames(res_mat), rownames(res_dbs_mat)))
  expect_true(setequal(colnames(res_mat), colnames(res_dbs_mat)))
  expect_equal(res_mat[rn, cn, drop = FALSE], res_dbs_mat[rn, cn, drop = FALSE])
})

test_that("Giotto vs GiottoDB (vizgen) overlapToMatrix are consistent", {
  g <- suppressWarnings(GiottoData::loadGiottoMini("vizgen"))

  poly_names <- GiottoClass::list_spatial_info_names(g)
  feat_names <- GiottoClass::list_feature_info_names(g)
  expect_gt(length(poly_names), 0)
  expect_gt(length(feat_names), 0)

  gpoly <- GiottoClass::getPolygonInfo(
    g,
    polygon_name = poly_names[[1]],
    return_giottoPolygon = TRUE,
    verbose = FALSE,
    simplify = TRUE
  )
  gpoints <- GiottoClass::getFeatureInfo(
    g,
    feat_type = feat_names[[1]],
    return_giottoPoints = TRUE,
    simplify = TRUE
  )

  ov_t <- GiottoClass::calculateOverlap(
    x = gpoly,
    y = gpoints,
    return_gpolygon = FALSE,
    verbose = FALSE
  )
  m_t <- GiottoClass::overlapToMatrix(x = ov_t)

  con2 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)
  dbSpatial::loadSpatial(con2)

  gdb <- GiottoDB::as_giottodb(g, con = con2, verbose = FALSE, overwrite = TRUE)
  gpoly_db <- GiottoClass::getPolygonInfo(
    gdb,
    polygon_name = poly_names[[1]],
    return_giottoPolygon = TRUE,
    verbose = FALSE,
    simplify = TRUE
  )
  gpoints_db <- GiottoClass::getFeatureInfo(
    gdb,
    feat_type = feat_names[[1]],
    return_giottoPoints = TRUE,
    simplify = TRUE
  )

  ov_db <- GiottoClass::calculateOverlap(
    x = gpoly_db,
    y = gpoints_db,
    return_gpolygon = FALSE,
    verbose = FALSE
  )
  m_db <- GiottoDB::overlapToMatrix(
    x = ov_db,
    output = "matrix",
    row_names = rownames(m_t),
    col_names = colnames(m_t),
    verbose = FALSE
  )

  expect_true(setequal(rownames(m_t), rownames(m_db)))
  expect_true(setequal(colnames(m_t), colnames(m_db)))

  rn <- intersect(rownames(m_t), rownames(m_db))
  cn <- intersect(colnames(m_t), colnames(m_db))
  expect_equal(m_t[rn, cn, drop = FALSE], m_db[rn, cn, drop = FALSE])
})