library('dbSpatial')
library('dbMatrix')

gpolys <- GiottoData::loadSubObjectMini('giottoPolygon')
gpoints <- GiottoData::loadSubObjectMini('giottoPoints')

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

test_that("overlapToMatrix works", {
  # test Giotto
  res_giotto <- GiottoClass::calculateOverlap(
    #TODO: update to .calculate_overlap_vector
    x = gpolys,
    y = gpoints
  )

  res_mat <- GiottoClass::overlapToMatrix(x = res_giotto)

  # test dbSpatial
  res_giotto <- GiottoClass::calculateOverlap(
    #TODO: update to .calculate_overlap_vector
    x = dbs_polys,
    y = dbs_points
  )

  res_dbs_mat <- GiottoDB::overlapToMatrix(x = res_giotto, output = "matrix")

  # TODO: update this once vector overlap is implemented as results currently differ, tests currently failing due to raster overlap
  # expect_true(identical(res_mat, res_dbs_mat))
})

#TODO: test for count_info_column
