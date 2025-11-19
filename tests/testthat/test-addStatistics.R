test_that("addStatistics GiottoDB vs Giotto equivalence", {
  skip_if_not_installed("GiottoData")
  
  # Load test data and create objects
  g <- GiottoData::loadGiottoMini("visium")
  gdb <- as_giottodb(g)
  
  # Run addStatistics on both with all stats including area
  g_stats <- Giotto::addStatistics(g, return_gobject = FALSE)
  gdb_stats <- addStatistics(gdb, return_gobject = FALSE)
  
  # Test feature statistics equivalence
  expect_equal(g_stats$feat_stats$feats, gdb_stats$feat_stats$feats)
  expect_equal(g_stats$feat_stats$nr_cells, gdb_stats$feat_stats$nr_cells)
  expect_equal(g_stats$feat_stats$total_expr, gdb_stats$feat_stats$total_expr, tolerance = 1e-10)
  expect_equal(g_stats$feat_stats$mean_expr_det, gdb_stats$feat_stats$mean_expr_det, tolerance = 1e-10)
  
  # Test cell statistics equivalence  
  expect_equal(g_stats$cell_stats$cells, gdb_stats$cell_stats$cells)
  expect_equal(g_stats$cell_stats$nr_feats, gdb_stats$cell_stats$nr_feats)
  expect_equal(g_stats$cell_stats$total_expr, gdb_stats$cell_stats$total_expr, tolerance = 1e-10)
  
  # Test area statistics equivalence
  expect_equal(g_stats$poly_stats$cell_ID, gdb_stats$poly_stats$cell_ID)
  expect_equal(g_stats$poly_stats$area, gdb_stats$poly_stats$area, tolerance = 1e-10)
})

test_that("addStatistics works with regular giotto objects when GiottoDB is loaded", {
  skip_if_not_installed("GiottoData")
  
  # This test specifically checks the issue user reported
  g <- GiottoData::loadGiottoMini("visium")
  
  # Test with different expression_values parameter like user's case
  expect_no_error({
    result <- addStatistics(gobject = g, expression_values = "raw")
  })
  
  # Test that it returns a giotto object
  result <- addStatistics(gobject = g, expression_values = "raw", verbose = FALSE)
  expect_s4_class(result, "giotto")
  
  # Test with return_gobject = FALSE
  result_list <- addStatistics(gobject = g, expression_values = "raw", return_gobject = FALSE, verbose = FALSE)
  expect_type(result_list, "list")
  expect_true("feat_stats" %in% names(result_list))
  expect_true("cell_stats" %in% names(result_list))
})