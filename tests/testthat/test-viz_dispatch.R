test_that("Visualization generics exist", {
    expect_true(is.function(spatPlot2D))
    expect_true(is.function(spatInSituPlotPoints))
    expect_true(is.function(plotUMAP))
    expect_true(is.function(plotPCA))
})

test_that("Visualization dispatch works correctly for GiottoDB objects", {
    # Create a dummy class to test dispatch
    dummy_db <- structure(list(), class = c("GiottoDB", "giotto"))

    # Test invalid backend
    expect_error(
        spatPlot2D(dummy_db, plot_method = "invalid_backend"),
        "'arg' should be one of"
    )

    # Test 'giotto' backend on GiottoDB object (should error as it is excluded)
    expect_error(
        spatPlot2D(dummy_db, plot_method = "giotto"),
        "'arg' should be one of"
    )

    # Test 'deckgl' backend
    # This will fail inside .spatPlot2D_deckgl because dummy object is not a valid S4 object,
    # causing GiottoClass getters to fail when accessing slots (e.g. @expression).
    # This confirms it dispatched to the correct internal function.
    expect_error(
        spatPlot2D(dummy_db, plot_method = "deckgl"),
        "no applicable method|Package 'rDeckgl' is required|spatPlot2D for GiottoDB only accepts|Failed to extract"
    )
})
