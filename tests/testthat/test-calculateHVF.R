# Tests for calculateHVF S3 generic — GiottoDB equivalence

library(GiottoDB)
library(Giotto)

test_that("calculateHVF produces identical HVF column for GiottoDB vs Giotto (cov_groups)", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  gdb <- as_giottodb(g, con = con, verbose = FALSE)

  # Both run on the same normalized expression
  g_h <- suppressWarnings(calculateHVF(g,
    expression_values = "normalized",
    method = "cov_groups",
    show_plot = FALSE, return_plot = FALSE, save_plot = FALSE,
    verbose = FALSE
  ))
  gdb_h <- suppressWarnings(calculateHVF(gdb,
    expression_values = "normalized",
    method = "cov_groups",
    show_plot = FALSE, return_plot = FALSE, save_plot = FALSE,
    verbose = FALSE
  ))

  fmeta_g <- data.table::setorder(
    GiottoClass::fDataDT(g_h, spat_unit = "cell", feat_type = "rna"), feat_ID
  )
  fmeta_gdb <- data.table::setorder(
    GiottoClass::fDataDT(gdb_h, spat_unit = "cell", feat_type = "rna"), feat_ID
  )

  expect_equal(fmeta_g$hvf, fmeta_gdb$hvf)
})

test_that("calculateHVF produces identical HVF column for GiottoDB vs Giotto (cov_loess)", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  gdb <- as_giottodb(g, con = con, verbose = FALSE)

  g_h <- suppressWarnings(calculateHVF(g,
    expression_values = "normalized",
    method = "cov_loess",
    show_plot = FALSE, return_plot = FALSE, save_plot = FALSE,
    verbose = FALSE
  ))
  gdb_h <- suppressWarnings(calculateHVF(gdb,
    expression_values = "normalized",
    method = "cov_loess",
    show_plot = FALSE, return_plot = FALSE, save_plot = FALSE,
    verbose = FALSE
  ))

  fmeta_g <- data.table::setorder(
    GiottoClass::fDataDT(g_h, spat_unit = "cell", feat_type = "rna"), feat_ID
  )
  fmeta_gdb <- data.table::setorder(
    GiottoClass::fDataDT(gdb_h, spat_unit = "cell", feat_type = "rna"), feat_ID
  )

  expect_equal(fmeta_g$hvf, fmeta_gdb$hvf)
})

test_that("calculateHVF.GiottoDB errors on expression_values = 'scaled'", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  gdb <- as_giottodb(g, con = con, verbose = FALSE)

  expect_error(
    calculateHVF(gdb, expression_values = "scaled"),
    regexp = "scaled.*not supported|not supported.*scaled",
    ignore.case = TRUE
  )
})
