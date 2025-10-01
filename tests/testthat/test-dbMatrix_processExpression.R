# Test processExpression with dbMatrix
# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")
library(Giotto)

# ---------------------------------------------------------------------------- #
# Setup data & Objects (runs once for all tests in this file)
skip_if_not_installed("dbMatrix")
# Create temp file and connection, ensure cleanup
tmpfile <- tempfile(fileext = ".db")
con <- DBI::dbConnect(duckdb::duckdb(), tmpfile, read_only = FALSE)
# Ensure disconnection and file deletion on exit/error
on.exit(
  {
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
    if (file.exists(tmpfile)) unlink(tmpfile, force = TRUE)
  },
  add = TRUE
)


# Load test data
visium <- GiottoData::loadGiottoMini(dataset = "visium")
dgc <- getExpression(visium, output = "matrix")

# Create dbMatrix
dbsm <- dbMatrix::dbMatrix(
  value = dgc,
  con = con,
  name = "dgc",
  class = "dbSparseMatrix",
  overwrite = TRUE
)

# Create exprObj with dbsm
expObj_db <- createExprObj(
  expression_data = dbsm,
  expression_matrix_class = "dbMatrix",
  name = "raw"
)

# Create giotto object
gobject_db <- suppressWarnings(
  createGiottoObject(expression = expObj_db)
)

# Filter both objects
visium_filtered <- filterGiotto(
  visium,
  spat_unit = "cell",
  feat_type = "rna",
  expression_values = "raw"
)

gobject_db_filtered <- filterGiotto(
  gobject_db,
  spat_unit = "cell",
  feat_type = "rna",
  expression_values = "raw"
)

# --- Sequential Processing Steps ---

# 1. Library normalization
visium_lib <- processExpression(
  visium_filtered,
  param = normParam("library"),
  name = "lib_norm"
)
gobject_db_lib <- processExpression(
  gobject_db_filtered,
  param = normParam("library"),
  name = "lib_norm"
)

# 2. Log normalization
visium_log <- processExpression(
  visium_lib,
  param = normParam("log"),
  name = "log_norm"
)
gobject_db_log <- processExpression(
  gobject_db_lib,
  param = normParam("log"),
  name = "log_norm"
)

# 3. Z-score scaling across features
visium_scale_feats <- processExpression(
  visium_log,
  param = scaleParam("zscore", MARGIN = 1),
  name = "scaled_feats"
)
gobject_db_scale_feats <- processExpression(
  gobject_db_log,
  param = scaleParam("zscore", MARGIN = 1),
  name = "scaled_feats"
)

# 4. Z-score scaling across cells
visium_scale_cells <- processExpression(
  visium_scale_feats,
  param = scaleParam("zscore", MARGIN = 2),
  name = "scaled_cells"
)
gobject_db_scale_cells <- processExpression(
  gobject_db_scale_feats,
  param = scaleParam("zscore", MARGIN = 2),
  name = "scaled_cells"
)

# # 5. Full pipeline with default processing params
visium_default <- processExpression(
  visium_filtered,
  list(
    normParam("library"),
    normParam("log"),
    scaleParam("zscore", MARGIN = 1),
    scaleParam("zscore", MARGIN = 2)
  ),
  name = "scaled_default"
)

gobject_db_default <- processExpression(
  gobject_db_filtered,
  list(
    normParam("library"),
    normParam("log"),
    scaleParam("zscore", MARGIN = 1),
    scaleParam("zscore", MARGIN = 2)
  ),
  name = "scaled_default"
)

# --- Tests ---

test_that("Library normalization matches for dbMatrix", {
  dgc_visium <- getExpression(
    visium_lib,
    output = "matrix",
    values = "lib_norm"
  )
  mat_db <- getExpression(
    gobject_db_lib,
    output = "matrix",
    values = "lib_norm"
  )
  dgc_db <- as.matrix(mat_db, sparse = TRUE, names = TRUE)
  expect_equal(dgc_visium, dgc_db)
})

test_that("Log normalization matches for dbMatrix", {
  dgc_visium <- getExpression(
    visium_log,
    output = "matrix",
    values = "log_norm"
  )
  mat_db <- getExpression(
    gobject_db_log,
    output = "matrix",
    values = "log_norm"
  )
  dgc_db <- as.matrix(mat_db, sparse = TRUE, names = TRUE)
  expect_equal(dgc_visium, dgc_db)
})

test_that("Z-score scaling (features) matches for dbMatrix", {
  dgc_visium <- getExpression(
    visium_scale_feats,
    output = "matrix",
    values = "scaled_feats"
  ) |>
    as.matrix()
  mat_db <- getExpression(
    gobject_db_scale_feats,
    output = "matrix",
    values = "scaled_feats"
  )
  dgc_db <- as.matrix(mat_db, sparse = FALSE, names = TRUE)
  # Allow small numerical differences due to different implementations
  expect_equal(dgc_visium, dgc_db, tolerance = 1e-6)
})

test_that("Z-score scaling (cells) matches for dbMatrix", {
  mat_giotto <- getExpression(
    visium_scale_cells,
    output = "matrix",
    values = "scaled_cells"
  ) |>
    as.matrix()
  mat_db <- getExpression(
    gobject_db_scale_cells,
    output = "matrix",
    values = "scaled_cells"
  ) |>
    as.matrix(names = TRUE)
  # Allow small numerical differences due to different implementations
  expect_equal(mat_giotto, mat_db, tolerance = 1e-6)
})

test_that("Full default pipeline matches for dbMatrix", {
  mat_giotto <- getExpression(
    visium_default,
    output = "matrix",
    values = "scaled_default"
  ) |>
    as.matrix()

  mat_db <- getExpression(
    gobject_db_default,
    output = "matrix",
    values = "scaled_default"
  ) |>
    as.matrix(names = TRUE)
  # Allow small numerical differences due to different implementations
  expect_equal(mat_giotto, mat_db, tolerance = 1e-6)
})

test_that("dbMatrix class is retained throughout processing", {
  result_db <- getExpression(
    gobject_db_default,
    values = "scaled_default",
    output = "exprObj"
  )
  expect_true(inherits(result_db[], "dbMatrix"))
})

# ---------------------------------------------------------------------------- #
# Tests for osmFISH normalization

test_that("osmFISH normalization works with dbMatrix", {
  # Process with osmFISH normalization
  visium_osm <- processExpression(
    visium_filtered,
    param = normParam("osmfish"),
    name = "osm_norm"
  )

  gobject_db_osm <- processExpression(
    gobject_db_filtered,
    param = normParam("osmfish"),
    name = "osm_norm"
  )

  # Compare results
  mat_giotto <- getExpression(
    visium_osm,
    output = "matrix",
    values = "osm_norm"
  ) |>
    as.matrix()

  mat_db <- getExpression(
    gobject_db_osm,
    output = "matrix",
    values = "osm_norm"
  ) |>
    as.matrix(names = TRUE)

  # Allow small numerical differences due to different implementations
  expect_equal(mat_giotto, mat_db, tolerance = 1e-6)

  # Check that dbMatrix class is retained
  result_db <- getExpression(
    gobject_db_osm,
    values = "osm_norm",
    output = "exprObj"
  )
  expect_true(inherits(result_db[], "dbMatrix"))
})

# ---------------------------------------------------------------------------- #
# Tests for Pearson residual normalization

test_that("Pearson residual normalization throws error for dbMatrix", {
  # Pearson residual normalization should throw an informative error for dbMatrix
  expect_error(
    processExpression(
      gobject_db_filtered,
      param = normParam("pearson"),
      name = "pearson_norm"
    ),
    "Pearson residual normalization is not currently supported for dbMatrix objects",
    fixed = TRUE
  )

  # Test with custom theta parameter also throws error
  expect_error(
    processExpression(
      gobject_db_filtered,
      param = normParam("pearson", theta = 50),
      name = "pearson_custom"
    ),
    "Pearson residual normalization is not currently supported for dbMatrix objects",
    fixed = TRUE
  )
})

# ---------------------------------------------------------------------------- #
# Tests for quantile normalization

# test_that("quantile normalization works with dbMatrix", {
#   # Process with quantile normalization
#   visium_quant <- processExpression(
#     visium_filtered,
#     param = normParam("quantile"),
#     name = "quantile_norm"
#   )
#
#   gobject_db_quant <- processExpression(
#     gobject_db_filtered,
#     param = normParam("quantile"),
#     name = "quantile_norm"
#   )
#
#   # Compare results
#   mat_giotto <- getExpression(
#     visium_quant,
#     output = "matrix",
#     values = "quantile_norm"
#   ) |> as.matrix()
#
#   mat_db <- getExpression(
#     gobject_db_quant,
#     output = "matrix",
#     values = "quantile_norm"
#   ) |> as.matrix(names = TRUE)
#
#   # Allow small numerical differences due to different implementations
#   expect_equal(mat_giotto, mat_db, tolerance = 1e-6)
#
#   # Check that dbMatrix class is retained
#   result_db <- getExpression(
#     gobject_db_quant,
#     values = "quantile_norm",
#     output = "exprObj"
#   )
#   expect_true(inherits(result_db[], "dbMatrix"))
# })

# ---------------------------------------------------------------------------- #
# Tests for TF-IDF normalization

# test_that("TF-IDF normalization works with dbMatrix", {
#   # Process with TF-IDF normalization (default)
#   visium_tfidf <- processExpression(
#     visium_filtered,
#     param = normParam("tf-idf"),
#     name = "tfidf_norm"
#   )
#
#   gobject_db_tfidf <- processExpression(
#     gobject_db_filtered,
#     param = normParam("tf-idf"),
#     name = "tfidf_norm"
#   )
#
#   # Compare results
#   mat_giotto <- getExpression(
#     visium_tfidf,
#     output = "matrix",
#     values = "tfidf_norm"
#   ) |> as.matrix()
#
#   mat_db <- getExpression(
#     gobject_db_tfidf,
#     output = "matrix",
#     values = "tfidf_norm"
#   ) |> as.matrix(names = TRUE)
#
#   # Allow small numerical differences due to different implementations
#   expect_equal(mat_giotto, mat_db, tolerance = 1e-6)
#
#   # Test with different sub_methods
#   for (method_num in 1:3) {
#     visium_tfidf_method <- processExpression(
#       visium_filtered,
#       param = normParam("tf-idf", sub_method = method_num),
#       name = paste0("tfidf_method", method_num)
#     )
#
#     gobject_db_tfidf_method <- processExpression(
#       gobject_db_filtered,
#       param = normParam("tf-idf", sub_method = method_num),
#       name = paste0("tfidf_method", method_num)
#     )
#
#     # Compare results
#     mat_giotto_method <- getExpression(
#       visium_tfidf_method,
#       output = "matrix",
#       values = paste0("tfidf_method", method_num)
#     ) |> as.matrix()
#
#     mat_db_method <- getExpression(
#       gobject_db_tfidf_method,
#       output = "matrix",
#       values = paste0("tfidf_method", method_num)
#     ) |> as.matrix(names = TRUE)
#
#     # Allow small numerical differences due to different implementations
#     expect_equal(mat_giotto_method, mat_db_method, tolerance = 1e-6)
#   }
#
#   # Test with custom parameters
#   tfidf_custom <- normParam("tf-idf")
#   tfidf_custom$log_tf <- TRUE
#   tfidf_custom$scalefactor <- 5000
#
#   visium_tfidf_custom <- processExpression(
#     visium_filtered,
#     param = tfidf_custom,
#     name = "tfidf_custom"
#   )
#
#   gobject_db_tfidf_custom <- processExpression(
#     gobject_db_filtered,
#     param = tfidf_custom,
#     name = "tfidf_custom"
#   )
#
#   mat_giotto_custom <- getExpression(
#     visium_tfidf_custom,
#     output = "matrix",
#     values = "tfidf_custom"
#   ) |> as.matrix()
#
#   mat_db_custom <- getExpression(
#     gobject_db_tfidf_custom,
#     output = "matrix",
#     values = "tfidf_custom"
#   ) |> as.matrix(names = TRUE)
#
#   expect_equal(mat_giotto_custom, mat_db_custom, tolerance = 1e-6)
#
#   # Test with L2 normalization after TF-IDF
#   visium_tfidf_l2 <- processExpression(
#     visium_tfidf,
#     param = normParam("l2"),
#     name = "tfidf_l2"
#   )
#
#   gobject_db_tfidf_l2 <- processExpression(
#     gobject_db_tfidf,
#     param = normParam("l2"),
#     name = "tfidf_l2"
#   )
#
#   mat_giotto_l2 <- getExpression(
#     visium_tfidf_l2,
#     output = "matrix",
#     values = "tfidf_l2"
#   ) |> as.matrix()
#
#   mat_db_l2 <- getExpression(
#     gobject_db_tfidf_l2,
#     output = "matrix",
#     values = "tfidf_l2"
#   ) |> as.matrix(names = TRUE)
#
#   expect_equal(mat_giotto_l2, mat_db_l2, tolerance = 1e-6)
# })

# ---------------------------------------------------------------------------- #
# Tests for arcsinh transformation

# test_that("arcsinh normalization works with dbMatrix", {
#   # Process with arcsinh normalization (default cofactor c=5)
#   visium_arcsinh <- processExpression(
#     visium_filtered,
#     param = normParam("arcsinh"),
#     name = "arcsinh_norm"
#   )
#
#   gobject_db_arcsinh <- processExpression(
#     gobject_db_filtered,
#     param = normParam("arcsinh"),
#     name = "arcsinh_norm"
#   )
#
#   # Compare results
#   mat_giotto <- getExpression(
#     visium_arcsinh,
#     output = "matrix",
#     values = "arcsinh_norm"
#   ) |> as.matrix()
#
#   mat_db <- getExpression(
#     gobject_db_arcsinh,
#     output = "matrix",
#     values = "arcsinh_norm"
#   ) |> as.matrix(names = TRUE)
#
#   # Allow small numerical differences due to different implementations
#   expect_equal(mat_giotto, mat_db, tolerance = 1e-6)
#
#   # Test with different cofactor (c=1, common for IMC data)
#   visium_arcsinh_c1 <- processExpression(
#     visium_filtered,
#     param = normParam("arcsinh", c = 1),
#     name = "arcsinh_c1"
#   )
#
#   gobject_db_arcsinh_c1 <- processExpression(
#     gobject_db_filtered,
#     param = normParam("arcsinh", c = 1),
#     name = "arcsinh_c1"
#   )
#
#   mat_giotto_c1 <- getExpression(
#     visium_arcsinh_c1,
#     output = "matrix",
#     values = "arcsinh_c1"
#   ) |> as.matrix()
#
#   mat_db_c1 <- getExpression(
#     gobject_db_arcsinh_c1,
#     output = "matrix",
#     values = "arcsinh_c1"
#   ) |> as.matrix(names = TRUE)
#
#   expect_equal(mat_giotto_c1, mat_db_c1, tolerance = 1e-6)
#
#   # Check that dbMatrix class is retained
#   result_db <- getExpression(
#     gobject_db_arcsinh,
#     values = "arcsinh_norm",
#     output = "exprObj"
#   )
#   expect_true(inherits(result_db[], "dbMatrix"))
# })

# ---------------------------------------------------------------------------- #
# EXPANDED EQUIVALENCE TESTS FOR ALL WORKING METHODS

test_that("arcsinh normalization equivalence confirmed with dbMatrix", {
  # Test arcsinh normalization with default parameters
  visium_arcsinh <- processExpression(
    visium_filtered,
    param = normParam("arcsinh"),
    name = "arcsinh_norm"
  )

  gobject_db_arcsinh <- processExpression(
    gobject_db_filtered,
    param = normParam("arcsinh"),
    name = "arcsinh_norm"
  )

  # Compare results
  mat_giotto <- getExpression(
    visium_arcsinh,
    output = "matrix",
    values = "arcsinh_norm"
  ) |> as.matrix()

  mat_db <- getExpression(
    gobject_db_arcsinh,
    output = "matrix",
    values = "arcsinh_norm"
  ) |> as.matrix(names = TRUE)

  # Test numerical equivalence
  expect_equal(mat_giotto, mat_db, tolerance = 1e-6)
  
  # Test with custom cofactor (c=1)
  arcsinh_c1_param <- normParam("arcsinh")
  arcsinh_c1_param$c <- 1
  
  visium_arcsinh_c1 <- processExpression(
    visium_filtered,
    param = arcsinh_c1_param,
    name = "arcsinh_c1"
  )

  gobject_db_arcsinh_c1 <- processExpression(
    gobject_db_filtered,
    param = arcsinh_c1_param,
    name = "arcsinh_c1"
  )

  mat_giotto_c1 <- getExpression(
    visium_arcsinh_c1,
    output = "matrix",
    values = "arcsinh_c1"
  ) |> as.matrix()

  mat_db_c1 <- getExpression(
    gobject_db_arcsinh_c1,
    output = "matrix",
    values = "arcsinh_c1"
  ) |> as.matrix(names = TRUE)

  expect_equal(mat_giotto_c1, mat_db_c1, tolerance = 1e-6)

  # Check that dbMatrix class is retained
  result_db <- getExpression(
    gobject_db_arcsinh,
    values = "arcsinh_norm",
    output = "exprObj"
  )
  expect_true(inherits(result_db[], "dbMatrix"))
})

test_that("l2 normalization equivalence confirmed with dbMatrix", {
  # Test l2 normalization
  visium_l2 <- processExpression(
    visium_filtered,
    param = normParam("l2"),
    name = "l2_norm"
  )

  gobject_db_l2 <- processExpression(
    gobject_db_filtered,
    param = normParam("l2"),
    name = "l2_norm"
  )

  # Compare results
  mat_giotto <- getExpression(
    visium_l2,
    output = "matrix",
    values = "l2_norm"
  ) |> as.matrix()

  mat_db <- getExpression(
    gobject_db_l2,
    output = "matrix",
    values = "l2_norm"
  ) |> as.matrix(names = TRUE)

  # Test numerical equivalence
  expect_equal(mat_giotto, mat_db, tolerance = 1e-6)

  # Check that dbMatrix class is retained
  result_db <- getExpression(
    gobject_db_l2,
    values = "l2_norm",
    output = "exprObj"
  )
  expect_true(inherits(result_db[], "dbMatrix"))
})

# ---------------------------------------------------------------------------- #
# ERROR HANDLING TESTS FOR UNSUPPORTED METHODS

test_that("quantile normalization throws informative error for dbMatrix", {
  # Quantile normalization should throw an informative error for dbMatrix
  expect_error(
    processExpression(
      gobject_db_filtered,
      param = normParam("quantile"),
      name = "quantile_norm"
    ),
    "Quantile normalization is not currently supported for dbMatrix objects",
    fixed = FALSE
  )
})

test_that("TF-IDF normalization throws informative error for dbMatrix", {
  # TF-IDF normalization should throw an informative error for dbMatrix
  expect_error(
    processExpression(
      gobject_db_filtered,
      param = normParam("tf-idf"),
      name = "tfidf_norm"
    ),
    "TF-IDF normalization is not currently supported for dbMatrix objects",
    fixed = FALSE
  )
})

# ---------------------------------------------------------------------------- #
# COMPREHENSIVE VALIDATION TESTS

test_that("all working normalization methods preserve matrix dimensions", {
  working_methods <- list(
    "library" = "lib_test",
    "log" = "log_test", 
    "osmfish" = "osm_test",
    "arcsinh" = "arcsinh_test",
    "l2" = "l2_test",
    "default" = "default_test"
  )
  
  # Get original dimensions
  original_dims <- dim(getExpression(gobject_db_filtered, values = "raw"))
  
  for(method in names(working_methods)) {
    result <- processExpression(
      gobject_db_filtered,
      param = normParam(method),
      name = working_methods[[method]]
    )
    
    result_dims <- dim(getExpression(result, values = working_methods[[method]]))
    expect_equal(result_dims, original_dims, 
                info = paste("Dimensions preserved for", method))
  }
})

test_that("all working scaling methods preserve matrix dimensions", {
  scaling_methods <- list(
    "zscore_features" = scaleParam("zscore", MARGIN = 1),
    "zscore_cells" = scaleParam("zscore", MARGIN = 2)
  )
  
  # Get original dimensions
  original_dims <- dim(getExpression(gobject_db_filtered, values = "raw"))
  
  for(method_name in names(scaling_methods)) {
    result <- processExpression(
      gobject_db_filtered,
      param = scaling_methods[[method_name]],
      name = method_name
    )
    
    result_dims <- dim(getExpression(result, values = method_name))
    expect_equal(result_dims, original_dims,
                info = paste("Dimensions preserved for", method_name))
  }
})

test_that("all working methods retain dbMatrix class", {
  working_methods <- c("library", "log", "osmfish", "arcsinh", "l2", "default")
  
  for(method in working_methods) {
    result <- processExpression(
      gobject_db_filtered,
      param = normParam(method),
      name = paste0(method, "_class_test")
    )
    
    result_obj <- getExpression(result, values = paste0(method, "_class_test"), output = "exprObj")
    expect_true(inherits(result_obj[], "dbMatrix"),
               info = paste("dbMatrix class retained for", method))
  }
})
