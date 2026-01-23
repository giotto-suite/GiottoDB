library('dbSpatial')

.ovlpPointDT_to_spatvector <- function(ovlp, points_sv) {
  # Convert GiottoClass overlapPointDT (poly/feat relationships) back into a
  # terra SpatVector of points, duplicating points when they map to multiple
  # polygons.
  ovlp_df <- as.data.frame(ovlp)
  idx <- match(ovlp_df$feat_ID_uniq, points_sv$feat_ID_uniq)
  out <- points_sv[idx]
  out$poly_ID <- ovlp_df$poly_ID
  out$feat_ID <- ovlp_df$feat_ID
  out
}

.canonical_overlap_df <- function(sv) {
  coords <- terra::crds(sv, df = TRUE)
  attrs <- as.data.frame(sv)
  out <- data.frame(
    poly_ID = attrs$poly_ID,
    feat_ID = attrs$feat_ID,
    feat_ID_uniq = attrs$feat_ID_uniq,
    x = coords[[1]],
    y = coords[[2]],
    stringsAsFactors = FALSE
  )
  out <- out[order(out$poly_ID, out$feat_ID_uniq, out$x, out$y), , drop = FALSE]
  rownames(out) <- NULL
  out
}

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

gpolys_db <- GiottoDB::createGiottoPolygon(dbs_polys, name = "cell", verbose = FALSE)
gpoints_db <- GiottoDB::createGiottoPoints(dbs_points, feat_type = "rna", verbose = FALSE)

test_that("calculateOverlap: dbSpatial and spatVector matches", {
  # test Giotto
  res_sv <- GiottoClass::calculateOverlap(
    # TODO: update to .calculate_overlap_vector
    x = gpolys,
    y = gpoints
  )
  res_sv_pts <- .ovlpPointDT_to_spatvector(res_sv@overlaps$rna, gpoints[])
  res_sv_pts <- terra::na.omit(res_sv_pts, field = "poly_ID")

  # test dbSpatial
  res_dbs <- GiottoDB::calculateOverlap(
    x = dbs_polys,
    y = dbs_points
  )
  res_dbs_sv <- dbSpatial::vect(
    x = res_dbs,
    select = c("poly_ID", "feat_ID", "feat_ID_uniq")
  )

  expect_equal(
    .canonical_overlap_df(res_sv_pts),
    .canonical_overlap_df(res_dbs_sv),
    tolerance = 1e-10
  )
})

test_that("calculateOverlap: giottoPolygon/giottoPoints dbSpatial-backed returns points", {
  res_gpoly <- GiottoDB::calculateOverlap(
    x = gpolys_db,
    y = gpoints_db,
    return_gpolygon = FALSE
  )
  expect_true(inherits(res_gpoly, "dbSpatial"))

  res_gpoly_sv <- dbSpatial::vect(
    x = res_gpoly,
    select = c("poly_ID", "feat_ID", "feat_ID_uniq")
  )
  expect_true(inherits(res_gpoly_sv, "SpatVector"))
  expect_true(all(grepl("point", tolower(as.character(terra::geomtype(res_gpoly_sv))))))
})

test_that("calculateOverlap: dbSpatial/dbSpatial rejects non poly->point", {
  expect_error(
    GiottoDB::calculateOverlap(x = dbs_polys, y = dbs_polys),
    "expects `y` to contain point geometries"
  )
  expect_error(
    GiottoDB::calculateOverlap(x = dbs_points, y = dbs_points),
    "expects `x` to contain polygon geometries"
  )
  expect_error(
    GiottoDB::calculateOverlap(x = dbs_points, y = dbs_polys),
    "expects `x` to contain polygon geometries"
  )
})

test_that("calculateOverlap: rejects mixing dbSpatial-backed and terra-backed", {
  expect_error(
    GiottoDB::calculateOverlap(x = gpolys_db, y = gpoints),
    "mixing dbSpatial-backed and terra-backed"
  )
  expect_error(
    GiottoDB::calculateOverlap(x = gpolys, y = gpoints_db),
    "mixing dbSpatial-backed and terra-backed"
  )
})

test_that("calculateOverlap: dbSpatial and spatVector matches with poly ids", {
  # test Giotto
  res_sv <- GiottoClass::calculateOverlap(
    x = gpolys,
    y = gpoints,
    poly_subset_ids = poly_subset_ids
  )
  res_sv_pts <- .ovlpPointDT_to_spatvector(res_sv@overlaps$rna, gpoints[])
  res_sv_pts <- terra::na.omit(res_sv_pts, field = "poly_ID")

  # test dbSpatial
  res_dbs <- GiottoDB::calculateOverlap(
    x = dbs_polys,
    y = dbs_points,
    poly_subset_ids = poly_subset_ids
  )
  res_dbs_sv <- dbSpatial::vect(
    x = res_dbs,
    select = c("poly_ID", "feat_ID", "feat_ID_uniq")
  )

  expect_equal(
    .canonical_overlap_df(res_sv_pts),
    .canonical_overlap_df(res_dbs_sv),
    tolerance = 1e-10
  )
})

test_that("calculateOverlap: dbSpatial and spatVector matches with feat ids", {
  # test Giotto
  res_sv <- GiottoClass::calculateOverlap(
    x = gpolys,
    y = gpoints,
    feat_subset_ids = feat_subset_ids,
    feat_subset_column = "feat_ID"
  )
  res_sv_pts <- .ovlpPointDT_to_spatvector(res_sv@overlaps$rna, gpoints[])
  res_sv_pts <- terra::na.omit(res_sv_pts, field = "feat_ID")

  # test dbSpatial
  res_dbs <- GiottoDB::calculateOverlap(
    x = dbs_polys,
    y = dbs_points,
    feat_subset_ids = feat_subset_ids,
    feat_subset_column = "feat_ID"
  )
  res_dbs_sv <- dbSpatial::vect(
    x = res_dbs,
    select = c("poly_ID", "feat_ID", "feat_ID_uniq")
  )

  expect_equal(
    .canonical_overlap_df(res_sv_pts),
    .canonical_overlap_df(res_dbs_sv),
    tolerance = 1e-10
  )
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
  res_sv_pts <- .ovlpPointDT_to_spatvector(res_sv@overlaps$rna, gpoints[])
  res_sv_pts <- terra::na.omit(res_sv_pts, field = "feat_ID")
  res_sv_pts <- terra::na.omit(res_sv_pts, field = "poly_ID")

  # test dbSpatial
  res_dbs <- GiottoDB::calculateOverlap(
    x = dbs_polys,
    y = dbs_points,
    poly_subset_ids = poly_subset_ids,
    feat_subset_ids = feat_subset_ids,
    feat_subset_column = "feat_ID"
  )
  res_dbs_sv <- dbSpatial::vect(
    x = res_dbs,
    select = c("poly_ID", "feat_ID", "feat_ID_uniq")
  )

  expect_equal(
    .canonical_overlap_df(res_sv_pts),
    .canonical_overlap_df(res_dbs_sv),
    tolerance = 1e-10
  )
})
