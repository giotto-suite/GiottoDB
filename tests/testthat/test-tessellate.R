test_that("tessellate works for dbSpatial objects", {
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  points <- data.frame(
    x = c(100, 200, 300),
    y = c(500, 600, 700),
    id = 1:3
  )

  db_points <- dbSpatial::dbSpatial(
    conn = con,
    value = points,
    x_colName = "x",
    y_colName = "y",
    name = "points",
    overwrite = TRUE
  )

  res <- tessellate(
    db_points,
    name = "test_tessellation",
    shape = "hexagon",
    shape_size = 60,
    overwrite = TRUE
  )

  expect_s4_class(res, "dbSpatial")
  expect_true("test_tessellation" %in% DBI::dbListTables(con))
  expect_gt(nrow(DBI::dbGetQuery(con, "SELECT * FROM test_tessellation")), 0)

  expect_error(
    tessellate(
      db_points,
      extent = GiottoClass::ext(0, 100, 0, 100),
      shape = "square",
      shape_size = 10
    ),
    "Provide either `x` or `extent`, not both."
  )
})

test_that("tessellate preserves GiottoClass extent behavior", {
  extent <- GiottoClass::ext(0, 100, 0, 100)

  res_named <- tessellate(
    extent = extent,
    shape = "square",
    shape_size = 10,
    gap = 1
  )

  res_positional <- tessellate(extent, shape = "square", shape_size = 10, gap = 1)

  expect_s4_class(res_named, "giottoPolygon")
  expect_s4_class(res_positional, "giottoPolygon")
})
