#' @title Add feature and cell statistics for GiottoDB objects
#' @name addStatistics
#' @description S3 generic and method for adding statistics to GiottoDB objects
#' with database-optimized operations
#' @param gobject giotto object
#' @param ... additional arguments passed to methods
#' @returns giotto object
#' @export
addStatistics <- function(gobject, ...) {
  UseMethod("addStatistics")
}

#' @title Add statistics to regular giotto object
#' @name addStatistics.giotto
#' @description Default method that delegates to the original Giotto implementation
#' @param gobject giotto object
#' @param feat_type feature type
#' @param spat_unit spatial unit
#' @param stats character. What statistics to add.
#' default = c("cell", "feature") See details
#' @param expression_values expression values to use
#' @param detection_threshold detection threshold to consider a feature detected
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @param verbose be verbose
#' @returns giotto object if return_gobject = TRUE, else a list with results
#' @export
addStatistics.giotto <- function(gobject,
                                 feat_type = NULL,
                                 spat_unit = NULL,
                                 stats = c("feature", "cell", "area"),
                                 expression_values = c("normalized", "scaled", "custom"),
                                 detection_threshold = 0,
                                 return_gobject = TRUE,
                                 verbose = TRUE) {
  # Delegate to the original Giotto implementation
  # Note: Original Giotto::addStatistics doesn't have ... parameter
  Giotto::addStatistics(
    gobject = gobject,
    feat_type = feat_type,
    spat_unit = spat_unit,
    stats = stats,
    expression_values = expression_values,
    detection_threshold = detection_threshold,
    return_gobject = return_gobject,
    verbose = verbose
  )
}

#' @title Add statistics to GiottoDB object
#' @name addStatistics.GiottoDB
#' @description Database-optimized method for adding feature and cell statistics
#' @param gobject GiottoDB object
#' @param feat_type feature type
#' @param spat_unit spatial unit
#' @param stats character. What statistics to add.
#' default = c("cell", "feature") See details
#' @param expression_values expression values to use
#' @param detection_threshold detection threshold to consider a feature detected
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @param verbose be verbose
#' @param ... additional arguments
#' @returns GiottoDB object if return_gobject = TRUE, else a list with results
#' @details
#' This method provides database-optimized statistics calculation for GiottoDB objects.
#' It handles dbMatrix objects without forcing early materialization to memory.
#'
#' # `stats` options
#' "feature" - includes feature statistics results
#' "cell" - includes cell statistics results
#' "area" - includes polygon areas
#' @export
addStatistics.GiottoDB <- function(
  gobject,
  feat_type = NULL,
  spat_unit = NULL,
  stats = c("feature", "cell", "area"),
  expression_values = c("normalized", "scaled", "custom"),
  detection_threshold = 0,
  return_gobject = TRUE,
  verbose = TRUE,
  ...
) {
  # Set feat_type and spat_unit
  spat_unit <- GiottoClass::set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- GiottoClass::set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )

  stat_choices <- c("cell", "feature", "area")
  stats <- match.arg(
    tolower(stats),
    choices = stat_choices,
    several.ok = TRUE
  )
  # expression values to be used
  expression_values <- match.arg(
    expression_values,
    unique(c("normalized", "scaled", "custom", expression_values))
  )

  if (any(c("feature", "cell") %in% stats)) {
    if (verbose) {
      message(sprintf(
        "calculating statistics for \"%s\" expression",
        expression_values
      ))
    }
  }

  feat_stats <- NULL
  if ("feature" %in% stats) {
    # get feats statistics
    feat_stats <- .addFeatStatistics_GiottoDB(
      gobject,
      feat_type = feat_type,
      spat_unit = spat_unit,
      expression_values = expression_values,
      detection_threshold = detection_threshold,
      return_gobject = return_gobject,
      verbose = verbose
    )
    if (isTRUE(return_gobject)) {
      gobject <- feat_stats
    }
  }

  cell_stats <- NULL
  if ("cell" %in% stats) {
    # get cell statistics
    cell_stats <- .addCellStatistics_GiottoDB(
      gobject,
      feat_type = feat_type,
      spat_unit = spat_unit,
      expression_values = expression_values,
      detection_threshold = detection_threshold,
      return_gobject = return_gobject,
      verbose = verbose
    )
    if (isTRUE(return_gobject)) {
      gobject <- cell_stats
    }
  }

  poly_stats <- NULL
  if (any("area" %in% stats)) {
    # Use GiottoDB-specific implementation for area stats with dbSpatial support
    poly_stats <- .add_poly_statistics_GiottoDB(
      gobject,
      spat_unit = spat_unit,
      stats = stats,
      return_gobject = return_gobject
    )
    if (isTRUE(return_gobject)) {
      gobject <- poly_stats
    }
  }

  if (isTRUE(return_gobject)) {
    return(gobject)
  } else {
    out <- list()
    out$feat_stats <- feat_stats
    out$cell_stats <- cell_stats
    out$poly_stats <- poly_stats
    return(out)
  }
}

#' @title Add feature statistics for GiottoDB objects
#' @name .addFeatStatistics_GiottoDB
#' @description Internal function for adding feature statistics with dbMatrix optimization
#' @param gobject GiottoDB object
#' @param feat_type feature type
#' @param spat_unit spatial unit
#' @param expression_values expression values to use
#' @param detection_threshold detection threshold to consider a gene detected
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @param verbose be verbose
#' @returns GiottoDB object if return_gobject = TRUE
#' @keywords internal
#' @noRd 
.addFeatStatistics_GiottoDB <- function(
  gobject,
  feat_type = NULL,
  spat_unit = NULL,
  expression_values = c("normalized", "scaled", "custom"),
  detection_threshold = 0,
  return_gobject = TRUE,
  verbose = TRUE
) {
  # Set feat_type and spat_unit
  spat_unit <- GiottoClass::set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- GiottoClass::set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )

  # expression values to be used
  expression_values <- match.arg(
    expression_values,
    unique(c("normalized", "scaled", "custom", expression_values))
  )
  expr_data <- GiottoClass::getExpression(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    values = expression_values,
    output = "exprObj",
    set_defaults = FALSE
  )

  # Check if we have a dbMatrix - GiottoDB objects should always have dbMatrix
  if (!inherits(expr_data@exprMat, "dbMatrix")) {
    # If not dbMatrix, delegate to original Giotto implementation
    return(Giotto::addFeatStatistics(
      gobject = gobject,
      feat_type = feat_type,
      spat_unit = spat_unit,
      expression_values = expression_values,
      detection_threshold = detection_threshold,
      return_gobject = return_gobject,
      verbose = verbose
    ))
  }

  # For dbMatrix objects, ensure we get vectors for data.table construction
  feat_stats <- data.table::data.table(
    feats = rownames(expr_data@exprMat),
    nr_cells = as.vector(GiottoClass::rowSums_flex(
      expr_data[] > detection_threshold
    )),
    perc_cells = (as.vector(GiottoClass::rowSums_flex(
      expr_data[] > detection_threshold
    )) /
      ncol(expr_data@exprMat)) *
      100,
    total_expr = as.vector(GiottoClass::rowSums_flex(expr_data[])),
    mean_expr = as.vector(GiottoClass::rowMeans_flex(expr_data[]))
  )

  # Calculate mean_expr_det avoiding the BOOLEAN * DECIMAL issue in dbMatrix
  expr_mat <- expr_data@exprMat
  detected_mask <- expr_mat > detection_threshold

  detected_numeric <- detected_mask * 1.0 # This should cast BOOLEAN to NUMERIC
  sum_detected <- as.vector(GiottoClass::rowSums_flex(
    expr_mat * detected_numeric
  ))
  count_detected <- as.vector(GiottoClass::rowSums_flex(detected_numeric))

  mean_expr_detected <- sum_detected / count_detected
  mean_expr_detected[count_detected == 0] <- NaN

  feat_stats[, mean_expr_det := mean_expr_detected]

  if (return_gobject == TRUE) {
    # remove previous statistics
    feat_metadata <- GiottoClass::getFeatureMetadata(
      gobject,
      spat_unit = spat_unit,
      feat_type = feat_type,
      output = "featMetaObj",
      copy_obj = TRUE,
      set_defaults = FALSE
    )

    if (methods::isS4(expr_data)) {
      if (!identical(expr_data@provenance, feat_metadata@provenance)) {
        warning("expression and feature metadata provenance mismatch")
      }
    }

    metadata_names <- colnames(feat_metadata[])

    if ("nr_cells" %in% metadata_names) {
      if (verbose) {
        message("feat statistics has already been applied once; overwriting")
      }
      feat_metadata[][,
        c(
          "nr_cells",
          "perc_cells",
          "total_expr",
          "mean_expr",
          "mean_expr_det"
        ) := NULL
      ]
      gobject <- GiottoClass::setGiotto(gobject, feat_metadata, verbose = FALSE)
    }

    gobject <- GiottoClass::addFeatMetadata(
      gobject = gobject,
      feat_type = feat_type,
      spat_unit = spat_unit,
      new_metadata = feat_stats,
      by_column = TRUE,
      column_feat_ID = "feats"
    )

    # Update parameters used
    gobject <- GiottoClass::update_giotto_params(
      gobject,
      description = "_feat_stats",
      toplevel = 3
    )

    return(gobject)
  } else {
    return(feat_stats)
  }
}

#' @title Add cell statistics for GiottoDB objects
#' @name .addCellStatistics_GiottoDB
#' @description Internal function for adding cell statistics with dbMatrix optimization
#' @param gobject GiottoDB object
#' @param feat_type feature type
#' @param spat_unit spatial unit
#' @param expression_values expression values to use
#' @param detection_threshold detection threshold to consider a gene detected
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @param verbose be verbose
#' @returns GiottoDB object if return_gobject = TRUE
#' @keywords internal
#' @noRd
.addCellStatistics_GiottoDB <- function(
  gobject,
  feat_type = NULL,
  spat_unit = NULL,
  expression_values = c("normalized", "scaled", "custom"),
  detection_threshold = 0,
  return_gobject = TRUE,
  verbose = TRUE
) {
  # Set feat_type and spat_unit
  spat_unit <- GiottoClass::set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- GiottoClass::set_default_feat_type(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type
  )

  # expression values to be used
  expression_values <- match.arg(
    expression_values,
    unique(c("normalized", "scaled", "custom", expression_values))
  )
  expr_data <- GiottoClass::getExpression(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    values = expression_values,
    output = "exprObj",
    set_defaults = FALSE
  )

  # Check if we have a dbMatrix - GiottoDB objects should always have dbMatrix
  if (!inherits(expr_data@exprMat, "dbMatrix")) {
    # If not dbMatrix, delegate to original Giotto implementation
    return(Giotto::addCellStatistics(
      gobject = gobject,
      feat_type = feat_type,
      spat_unit = spat_unit,
      expression_values = expression_values,
      detection_threshold = detection_threshold,
      return_gobject = return_gobject,
      verbose = verbose
    ))
  }

  # For dbMatrix objects, ensure we get vectors for data.table construction
  cell_stats <- data.table::data.table(
    cells = colnames(expr_data@exprMat),
    nr_feats = as.vector(GiottoClass::colSums_flex(
      expr_data[] > detection_threshold
    )),
    perc_feats = (as.vector(GiottoClass::colSums_flex(
      expr_data[] > detection_threshold
    )) /
      nrow(expr_data@exprMat)) *
      100,
    total_expr = as.vector(GiottoClass::colSums_flex(expr_data[]))
  )

  if (return_gobject == TRUE) {
    # remove previous statistics
    cell_metadata <- GiottoClass::getCellMetadata(
      gobject,
      spat_unit = spat_unit,
      feat_type = feat_type,
      output = "cellMetaObj",
      copy_obj = TRUE,
      set_defaults = FALSE
    )

    if (methods::isS4(expr_data)) {
      if (!identical(expr_data@provenance, cell_metadata@provenance)) {
        warning("expression and feature metadata provenance mismatch")
      }
    }

    metadata_names <- colnames(cell_metadata[])
    if ("nr_feats" %in% metadata_names) {
      if (verbose) {
        message("cells statistics has already been applied once; overwriting")
      }
      cell_metadata[][, c("nr_feats", "perc_feats", "total_expr") := NULL]
      gobject <- GiottoClass::setGiotto(gobject, cell_metadata, verbose = FALSE)
    }

    gobject <- GiottoClass::addCellMetadata(
      gobject = gobject,
      feat_type = feat_type,
      spat_unit = spat_unit,
      new_metadata = cell_stats,
      by_column = TRUE,
      column_cell_ID = "cells"
    )

    # Update parameters used
    gobject <- GiottoClass::update_giotto_params(
      gobject,
      description = "_cell_stats",
      toplevel = 3
    )

    return(gobject)
  } else {
    return(cell_stats)
  }
}

#' @title Add polygon statistics for GiottoDB objects
#' @name .add_poly_statistics_GiottoDB
#' @description Internal function for adding polygon statistics with dbSpatial support
#' @param gobject GiottoDB object
#' @param spat_unit spatial unit
#' @param stats character. What statistics to add
#' @param return_gobject boolean: return giotto object (default = TRUE)
#' @returns GiottoDB object if return_gobject = TRUE
#' @keywords internal
#' @noRd
.add_poly_statistics_GiottoDB <- function(
  gobject,
  spat_unit = "cell",
  stats = c("area"),
  return_gobject = TRUE
) {
  stat_choices <- c("area")
  stats <- match.arg(
    tolower(stats),
    choices = stat_choices,
    several.ok = TRUE
  )

  poly_list <- gobject[["spatial_info", spat_unit]]
  if (length(poly_list) > 0L) {
    gpoly <- poly_list[[1L]] # extract from list
  } else {
    # if no polys available, return early
    if (isTRUE(return_gobject)) {
      return(gobject)
    } else {
      return(data.table::data.table(cell_ID = GiottoClass::spatIDs(gobject)))
    }
  }

  # Handle dbSpatial objects by converting to terra SpatVector
  if (inherits(gpoly@spatVector, "dbSpatial")) {
    sv <- terra::vect(gpoly@spatVector)
  } else {
    sv <- gpoly[]
  }

  # accumulate results values
  # results order must be identical to the order of sv
  all_res <- list(cell_ID = sv$poly_ID)

  if ("area" %in% stats) {
    terra::crs(sv) <- "local"
    a <- terra::expanse(sv, transform = FALSE)
    all_res$area <- a
  }

  res_dt <- do.call(data.table::data.table, all_res)

  if (isTRUE(return_gobject)) {
    # append results if there are any
    if (ncol(res_dt) > 1L) {
      gobject <- GiottoClass::addCellMetadata(
        gobject,
        new_metadata = res_dt,
        by_column = TRUE,
        column_cell_ID = "cell_ID"
      )
    }
    return(gobject)
  } else {
    return(res_dt)
  }
}
