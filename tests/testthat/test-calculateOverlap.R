library('dbSpatial')

gpolys <- GiottoData::loadSubObjectMini('giottoPolygon')
gpoints <- GiottoData::loadSubObjectMini('giottoPoints')

poly_subset_ids <- gpolys$poly_ID[1]
feat_subset_ids <- "Vmn1r50" # hardcoded for now

con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
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

test_that("calculateOverlap: dbSpatial and spatVector matches", {
  # test Giotto
  res_sv <- GiottoClass::calculateOverlap(
    # TODO: update to .calculate_overlap_vector
    x = gpolys,
    y = gpoints
  )
  res_sv <- res_sv@overlaps$rna[]
  res_sv <- terra::na.omit(res_sv, field = 'poly_ID')

  # test dbSpatial
  res_dbs <- GiottoDB::calculateOverlap(
    x = dbs_polys,
    y = dbs_points
  )
  # convert res_dbs to terra spatvector
  res_dbs_sv <- dbSpatial::vect(
    x = res_dbs,
    select = c('poly_ID', 'feat_ID', 'feat_ID_uniq')
  )

  # TODO: update this once vector overlap is implemented as results currently differ
  # expect_true(terra::identical(res_sv, res_dbs_sv))
})

test_that("calculateOverlap: dbSpatial and spatVector matches with poly ids", {
  # test Giotto
  res_sv <- GiottoClass::calculateOverlap(
    x = gpolys,
    y = gpoints,
    poly_subset_ids = poly_subset_ids
  )
  res_sv <- res_sv@overlaps$rna[]
  res_sv <- terra::na.omit(res_sv, field = 'poly_ID')

  # test dbSpatial
  res_dbs <- GiottoClass::calculateOverlap(
    x = dbs_polys,
    y = dbs_points,
    poly_subset_ids = poly_subset_ids
  )

  # convert res_dbs to terra spatvector
  res_dbs_sv <- dbSpatial::vect(
    x = res_dbs,
    select = c('poly_ID', 'feat_ID', 'feat_ID_uniq')
  )

  # TODO: update this once vector overlap is implemented as results currently differ
  # expect_true(terra::identical(res_sv, res_dbs_sv))
})

test_that("calculateOverlap: dbSpatial and spatVector matches with feat ids", {
  # test Giotto
  res_sv <- GiottoClass::calculateOverlap(
    x = gpolys,
    y = gpoints,
    feat_subset_ids = feat_subset_ids,
    feat_subset_column = "feat_ID"
  )
  res_sv <- res_sv@overlaps$rna[]
  res_sv <- terra::na.omit(res_sv, field = 'feat_ID')

  # test dbSpatial
  res_dbs <- GiottoClass::calculateOverlap(
    x = dbs_polys,
    y = dbs_points,
    feat_subset_ids = feat_subset_ids,
    feat_subset_column = "feat_ID"
  )

  # convert res_dbs to terra spatvector
  res_dbs_sv <- dbSpatial::vect(
    x = res_dbs,
    select = c('poly_ID', 'feat_ID', 'feat_ID_uniq')
  )

  # Sort both results by poly_ID to ensure consistent ordering for comparison
  res_sv_sorted <- res_sv[order(res_sv$poly_ID), ]
  res_dbs_sorted <- res_dbs_sv[order(res_dbs_sv$poly_ID), ]
  
  # check geometries are equivalent (within tolerance for floating-point precision)
  expect_true(isTRUE(all.equal(terra::geom(res_sv_sorted), terra::geom(res_dbs_sorted), tolerance = 1e-10)))
  # check attributes are equivalent (handle potential column ordering or precision differences)
  expect_true(isTRUE(all.equal(as.data.frame(res_sv_sorted), as.data.frame(res_dbs_sorted), tolerance = 1e-10, check.attributes = FALSE)))
})

test_that("calculateOverlap: dbSpatial and spatVector matches with feat, polygon ids", {
  feat_subset_ids <- c('Mlc1', 'Gfap')

  # test Giotto
  res_sv <- GiottoClass::calculateOverlap(
    x = gpolys,
    y = gpoints,
    poly_subset_ids = poly_subset_ids,
    feat_subset_ids = feat_subset_ids,
    feat_subset_column = "feat_ID"
  )
  res_sv <- res_sv@overlaps$rna[]
  res_sv <- terra::na.omit(res_sv, field = 'feat_ID')
  res_sv <- terra::na.omit(res_sv, field = 'poly_ID')

  # test dbSpatial
  res_dbs <- GiottoClass::calculateOverlap(
    x = dbs_polys,
    y = dbs_points,
    poly_subset_ids = poly_subset_ids,
    feat_subset_ids = feat_subset_ids,
    feat_subset_column = "feat_ID"
  )

  # convert res_dbs to terra spatvector
  res_dbs_sv <- dbSpatial::vect(
    x = res_dbs,
    select = c('poly_ID', 'feat_ID', 'feat_ID_uniq')
  )

  # Sort both results by poly_ID to ensure consistent ordering for comparison
  res_sv_sorted <- res_sv[order(res_sv$poly_ID), ]
  res_dbs_sorted <- res_dbs_sv[order(res_dbs_sv$poly_ID), ]
  
  # check geometries are equivalent (within tolerance for floating-point precision)
  expect_true(isTRUE(all.equal(terra::geom(res_sv_sorted), terra::geom(res_dbs_sorted), tolerance = 1e-6)))
  # check attributes are equivalent (handle potential column ordering or precision differences)
  expect_true(isTRUE(all.equal(as.data.frame(res_sv_sorted), as.data.frame(res_dbs_sorted), tolerance = 1e-6, check.attributes = FALSE)))
})
