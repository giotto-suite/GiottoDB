library(testthat)

as.list.giotto <- function(gobject) {
  raw_list <- GiottoClass::as.list(gobject)

  result <- list()

  expr_objects <- Filter(function(obj) inherits(obj, "exprObj"), raw_list)
  for (obj in expr_objects) {
    key <- paste("exprObj", obj@spat_unit, obj@feat_type, obj@name, sep = ".")
    result[[key]] <- obj
  }

  spatial_objects <- Filter(
    function(obj) {
      inherits(obj, "giottoPoints") || inherits(obj, "giottoPolygon")
    },
    raw_list
  )

  for (obj in spatial_objects) {
    class_name <- class(obj)[1]
    spat_unit <- GiottoClass::spatUnit(obj)
    slot_names <- methods::slotNames(obj)
    obj_name <- if ("name" %in% slot_names) obj@name else "spatInfo"

    key <- paste(class_name, spat_unit, obj_name, sep = ".")
    result[[key]] <- obj
  }

  result
}

test_that("Conversion from GiottoDB to giotto works and coerces matrices/spatial", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  gobject <- GiottoData::loadGiottoMini("visium")
  expect_s4_class(gobject, "giotto")

  # Force at least one dense expression matrix so GiottoDB will contain dbDenseMatrix
  obj_list <- as.list.giotto(gobject)
  expr_keys <- grep("^exprObj\\.", names(obj_list), value = TRUE)
  expect_true(length(expr_keys) > 0)

  name_parts <- strsplit(expr_keys[1], "\\.")[[1]]
  spat_unit <- name_parts[2]
  feat_type <- name_parts[3]
  expr_name <- name_parts[4]

  # Add a second expression object so we verify multi-matrix preservation.
  # Use a unique name to avoid collisions with whatever GiottoData provides.
  extra_expr_name <- "giottodb_test_extra"
  if (
    extra_expr_name %in% names(gobject@expression[[spat_unit]][[feat_type]])
  ) {
    extra_expr_name <- paste0(extra_expr_name, "_2")
  }

  base_expr_obj <- gobject@expression[[spat_unit]][[feat_type]][[expr_name]]
  base_mat <- base_expr_obj@exprMat
  extra_expr_obj <- GiottoClass::createExprObj(
    expression_data = base_mat,
    name = extra_expr_name,
    spat_unit = spat_unit,
    feat_type = feat_type
  )
  gobject <- GiottoClass::setExpression(
    gobject = gobject,
    x = extra_expr_obj,
    spat_unit = spat_unit,
    feat_type = feat_type,
    name = extra_expr_name,
    verbose = FALSE,
    initialize = FALSE
  )

  expr_obj <- gobject@expression[[spat_unit]][[feat_type]][[expr_name]]
  if (methods::.hasSlot(expr_obj, "exprMat")) {
    expr_obj@exprMat <- as.matrix(expr_obj@exprMat)
    gobject@expression[[spat_unit]][[feat_type]][[expr_name]] <- expr_obj
  }

  tmp_dir <- file.path(getwd(), "tmp")
  if (!dir.exists(tmp_dir)) {
    dir.create(tmp_dir, recursive = TRUE)
  }
  temp_db <- file.path(
    tmp_dir,
    paste0("giottodb_as_giotto_", as.integer(Sys.time()), ".duckdb")
  )
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)

  tryCatch(
    {
      gdb <- as_giottodb(gobject, con = con, verbose = FALSE, overwrite = TRUE)
      expect_s4_class(gdb, "GiottoDB")

      g_back <- as_giotto(gdb, verbose = FALSE)
      expect_s4_class(g_back, "giotto")

      # The forced-dense expr matrix should come back as base matrix
      expr_back <- g_back@expression[[spat_unit]][[feat_type]][[expr_name]]
      expect_true(methods::.hasSlot(expr_back, "exprMat"))
      expect_true(is.matrix(expr_back@exprMat))

      # The extra expression should still exist and should be sparse in-memory.
      expect_true(
        extra_expr_name %in% names(g_back@expression[[spat_unit]][[feat_type]])
      )
      extra_back <- g_back@expression[[spat_unit]][[feat_type]][[
        extra_expr_name
      ]]
      expect_true(methods::.hasSlot(extra_back, "exprMat"))
      expect_true(inherits(extra_back@exprMat, "dgCMatrix"))

      # Spatial objects should be in-memory SpatVector when present
      if (length(g_back@spatial_info) > 0) {
        any_spat <- g_back@spatial_info[[names(g_back@spatial_info)[1]]]
        if (
          methods::.hasSlot(any_spat, "spatVector") &&
            !is.null(any_spat@spatVector)
        ) {
          expect_true(inherits(any_spat@spatVector, "SpatVector"))
        }
      }
    },
    finally = {
      DBI::dbDisconnect(con, shutdown = TRUE)
      if (file.exists(temp_db)) file.remove(temp_db)
    }
  )
})

test_that("as_giotto fails with informative message for non-GiottoDB input", {
  expect_error(as_giotto(list()), "Input must be a GiottoDB object")
})

test_that("round-trip: as_giottodb -> filter+normalize -> as_giotto materializes analysis results correctly", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")
  # GiottoUtils::get_args walks the call stack by name, so Giotto must be attached
  library(Giotto)

  g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)

  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  on.exit({
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
    if (file.exists(temp_db)) file.remove(temp_db)
  }, add = TRUE)

  gdb <- as_giottodb(g, con = con, verbose = FALSE)

  # Filter and normalize on GiottoDB directly
  gdb_f <- Giotto::filterGiotto(gdb,
    spat_unit = "cell", feat_type = "rna",
    expression_values = "raw", verbose = FALSE
  )
  gdb_n <- normalizeGiotto(gdb_f,
    spat_unit = "cell", feat_type = "rna",
    norm_methods = "standard", verbose = FALSE
  )

  # Convert back to in-memory giotto
  g_back <- as_giotto(gdb_n, verbose = FALSE)
  expect_s4_class(g_back, "giotto")

  mat_back <- getExpression(g_back, values = "normalized", output = "matrix")
  expect_true(inherits(mat_back, "Matrix") || is.matrix(mat_back))

  # Giotto reference: filter + normalize using in-memory Giotto
  g_f <- Giotto::filterGiotto(g,
    spat_unit = "cell", feat_type = "rna",
    expression_values = "raw", verbose = FALSE
  )
  g_n <- normalizeGiotto(g_f,
    spat_unit = "cell", feat_type = "rna",
    norm_methods = "standard", scale_feats = FALSE, scale_cells = FALSE,
    verbose = FALSE
  )
  mat_ref <- getExpression(g_n, values = "normalized", output = "matrix")

  # Align row/col order and compare
  mat_back <- as.matrix(mat_back)
  mat_ref <- as.matrix(mat_ref)
  mat_back <- mat_back[rownames(mat_ref), colnames(mat_ref)]

  expect_equal(mat_back, mat_ref, tolerance = 1e-10)
})

test_that("GiottoDB::runPCA delegates to Giotto::runPCA for in-memory objects after as_giotto()", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")
  library(Giotto)
  options(dbMatrix.allow_densify = TRUE, dbMatrix.max_mem_convert = 64 * 1024^3)

  g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  on.exit({
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
    if (file.exists(temp_db)) file.remove(temp_db)
  }, add = TRUE)

  gdb <- as_giottodb(g, con = con, verbose = FALSE)
  gdb_n <- GiottoDB::normalizeGiotto(gdb,
    spat_unit = "cell", feat_type = "rna",
    norm_methods = "standard", verbose = FALSE
  )
  gdb_h <- suppressWarnings(GiottoDB::calculateHVF(gdb_n,
    spat_unit = "cell", feat_type = "rna",
    expression_values = "normalized", method = "cov_groups",
    show_plot = FALSE, return_plot = FALSE, save_plot = FALSE, verbose = FALSE
  ))

  g_mem <- as_giotto(gdb_h, verbose = FALSE)
  expect_s4_class(g_mem, "giotto")

  # This must go through GiottoDB::runPCA -> runPCA.giotto -> Giotto::runPCA
  # without crashing at update_giotto_params/match.call
  g_pca <- GiottoDB::runPCA(g_mem,
    spat_unit = "cell", feat_type = "rna",
    expression_values = "normalized", feats_to_use = "hvf",
    ncp = 20, center = TRUE, scale_unit = FALSE, verbose = FALSE
  )

  pca_res <- Giotto::getDimReduction(g_pca,
    reduction = "cells", reduction_method = "pca", name = "pca"
  )
  expect_false(is.null(pca_res))
  expect_equal(ncol(pca_res@coordinates), 20)
})
