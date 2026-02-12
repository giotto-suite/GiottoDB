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
  if (extra_expr_name %in% names(gobject@expression[[spat_unit]][[feat_type]])) {
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
  if (!dir.exists(tmp_dir)) dir.create(tmp_dir, recursive = TRUE)
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
      expect_true(extra_expr_name %in% names(g_back@expression[[spat_unit]][[feat_type]]))
      extra_back <- g_back@expression[[spat_unit]][[feat_type]][[extra_expr_name]]
      expect_true(methods::.hasSlot(extra_back, "exprMat"))
      expect_true(inherits(extra_back@exprMat, "dgCMatrix"))

      # Spatial objects should be in-memory SpatVector when present
      if (length(g_back@spatial_info) > 0) {
        any_spat <- g_back@spatial_info[[names(g_back@spatial_info)[1]]]
        if (methods::.hasSlot(any_spat, "spatVector") && !is.null(any_spat@spatVector)) {
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
