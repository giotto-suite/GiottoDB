library(GiottoDB)

test_that("findMarkers_one_vs_all (scran) works for GiottoDB objects", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("scran")

  gobject <- GiottoData::loadGiottoMini("visium")

  local_tmp <- file.path(getwd(), "tmp")
  dir.create(local_tmp, showWarnings = FALSE, recursive = TRUE)
  temp_db <- tempfile(tmpdir = local_tmp, fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)
  on.exit({
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
    if (file.exists(temp_db)) file.remove(temp_db)
  }, add = TRUE)

  gdb <- as_giottodb(gobject, con = con, verbose = FALSE, overwrite = TRUE)

  # Confirm expression is dbMatrix before the call
  expr_obj <- GiottoClass::getExpression(
    gdb,
    spat_unit = "cell", feat_type = "rna",
    values = "normalized", output = "exprObj"
  )
  expect_true(inherits(methods::slot(expr_obj, "exprMat"), "dbMatrix"))

  result <- findMarkers_one_vs_all(
    gobject = gdb,
    spat_unit = "cell",
    feat_type = "rna",
    expression_values = "normalized",
    cluster_column = "leiden_clus",
    method = "scran",
    pval = 0.01,
    logFC = 0.5,
    min_feats = 1,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)

  # Original gobject expression should still be dbMatrix (not mutated)
  expr_obj2 <- GiottoClass::getExpression(
    gdb,
    spat_unit = "cell", feat_type = "rna",
    values = "normalized", output = "exprObj"
  )
  expect_true(inherits(methods::slot(expr_obj2, "exprMat"), "dbMatrix"))

  # dbMatrix.max_mem_convert should be restored
  expect_false(identical(getOption("dbMatrix.max_mem_convert"), Inf))
})

test_that("findMarkers_one_vs_all (gini) works for GiottoDB objects", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  gobject <- GiottoData::loadGiottoMini("visium")

  local_tmp <- file.path(getwd(), "tmp")
  dir.create(local_tmp, showWarnings = FALSE, recursive = TRUE)
  temp_db <- tempfile(tmpdir = local_tmp, fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)
  on.exit({
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
    if (file.exists(temp_db)) file.remove(temp_db)
  }, add = TRUE)

  gdb <- as_giottodb(gobject, con = con, verbose = FALSE, overwrite = TRUE)

  result <- findMarkers_one_vs_all(
    gobject = gdb,
    spat_unit = "cell",
    feat_type = "rna",
    expression_values = "normalized",
    cluster_column = "leiden_clus",
    method = "gini",
    pval = 0.01,
    logFC = 0.5,
    min_feats = 1,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("findMarkers_one_vs_all (mast) works for GiottoDB objects", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("MAST")

  gobject <- GiottoData::loadGiottoMini("visium")

  local_tmp <- file.path(getwd(), "tmp")
  dir.create(local_tmp, showWarnings = FALSE, recursive = TRUE)
  temp_db <- tempfile(tmpdir = local_tmp, fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)
  on.exit({
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
    if (file.exists(temp_db)) file.remove(temp_db)
  }, add = TRUE)

  gdb <- as_giottodb(gobject, con = con, verbose = FALSE, overwrite = TRUE)

  result <- findMarkers_one_vs_all(
    gobject = gdb,
    spat_unit = "cell",
    feat_type = "rna",
    expression_values = "normalized",
    cluster_column = "leiden_clus",
    method = "mast",
    pval = 0.01,
    logFC = 0.5,
    min_feats = 1,
    verbose = FALSE
  )

  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})
