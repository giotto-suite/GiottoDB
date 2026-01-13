library(GiottoVisuals)

test_that("Visualization generics exist", {
    expect_true(is.function(spatPlot2D))
    expect_true(is.function(spatInSituPlotPoints))
    expect_true(is.function(plotUMAP))
    expect_true(is.function(plotPCA))
})

test_that("Visualization dispatch works correctly for GiottoDB objects", {
    skip_if_not_installed("GiottoData")
    skip_if_not_installed("duckdb")
    
    # Create a real GiottoDB object for proper testing
    g <- GiottoData::loadGiottoMini("visium")
    con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    dbSpatial::loadSpatial(con)
    gdb <- as_giottodb(g, con = con, verbose = FALSE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    
    # Test invalid backend
    expect_error(
        spatPlot2D(gdb, plot_method = "invalid_backend"),
        "'arg' should be one of"
    )

    # Test 'giotto' backend on GiottoDB object (should error as it is excluded)
    expect_error(
        spatPlot2D(gdb, plot_method = "giotto"),
        "'arg' should be one of"
    )
})

