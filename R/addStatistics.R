#' Add statistics for Giotto / GiottoDB
#'
#' S3 generic that delegates to `Giotto::addStatistics()` for plain giotto
#' objects, and adds DuckDB-native polygon area support for GiottoDB objects
#' with dbSpatial-backed polygons.
#'
#' @param gobject A giotto or GiottoDB object.
#' @param feat_type Feature type.
#' @param spat_unit Spatial unit.
#' @param stats Which statistics to compute.
#' @param expression_values Expression values to use.
#' @param detection_threshold Detection threshold.
#' @param return_gobject Whether to return the updated object.
#' @param verbose Verbosity.
#' @param ... Additional arguments (currently ignored).
#'
#' @export
addStatistics <- function(gobject, ...) {
  UseMethod("addStatistics")
}

#' @rdname addStatistics
#' @export
addStatistics.giotto <- function(gobject,
    feat_type = NULL,
    spat_unit = NULL,
    stats = c("feature", "cell", "area"),
    expression_values = c("normalized", "scaled", "custom"),
    detection_threshold = 0,
    return_gobject = TRUE,
    verbose = TRUE,
    ...) {
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

#' @rdname addStatistics
#' @export
addStatistics.GiottoDB <- function(gobject,
    feat_type = NULL,
    spat_unit = NULL,
    stats = c("feature", "cell", "area"),
    expression_values = c("normalized", "scaled", "custom"),
    detection_threshold = 0,
    return_gobject = TRUE,
    verbose = TRUE,
    ...) {
  stat_choices <- c("cell", "feature", "area")
  stats <- match.arg(
    tolower(stats),
    choices = stat_choices,
    several.ok = TRUE
  )
  expression_values <- match.arg(
    expression_values,
    unique(c("normalized", "scaled", "custom", expression_values))
  )

  if (!"area" %in% stats) {
    return(Giotto::addStatistics(
      gobject = gobject,
      feat_type = feat_type,
      spat_unit = spat_unit,
      stats = stats,
      expression_values = expression_values,
      detection_threshold = detection_threshold,
      return_gobject = return_gobject,
      verbose = verbose
    ))
  }

  spat_unit <- GiottoClass::set_default_spat_unit(
    gobject = gobject,
    spat_unit = spat_unit
  )
  feat_type <- GiottoClass::set_default_feat_type(
    gobject = gobject,
    feat_type = feat_type,
    spat_unit = spat_unit
  )

  poly_list <- gobject[["spatial_info", spat_unit]]
  gpoly <- if (length(poly_list) > 0L) poly_list[[1L]] else NULL
  sv <- if (!is.null(gpoly)) gpoly[] else NULL

  # If polygons are not dbSpatial-backed, fall back to Giotto's implementation.
  if (!inherits(sv, "dbSpatial")) {
    return(Giotto::addStatistics(
      gobject = gobject,
      feat_type = feat_type,
      spat_unit = spat_unit,
      stats = stats,
      expression_values = expression_values,
      detection_threshold = detection_threshold,
      return_gobject = return_gobject,
      verbose = verbose
    ))
  }

  # 1) Compute feature/cell stats via Giotto (skip area to avoid terra::crs/expanse)
  stats_no_area <- setdiff(stats, "area")
  base_res <- NULL
  if (length(stats_no_area) > 0L) {
    base_res <- Giotto::addStatistics(
      gobject = gobject,
      feat_type = feat_type,
      spat_unit = spat_unit,
      stats = stats_no_area,
      expression_values = expression_values,
      detection_threshold = detection_threshold,
      return_gobject = return_gobject,
      verbose = verbose
    )
  } else {
    base_res <- if (isTRUE(return_gobject)) gobject else list(
      feat_stats = NULL,
      cell_stats = NULL,
      poly_stats = NULL
    )
  }

  # 2) Compute polygon areas lazily in DuckDB and collect
  poly_stats <- NULL
  if (is.null(gpoly)) {
    if (isTRUE(return_gobject)) {
      return(base_res)
    }
    poly_stats <- data.table::data.table(cell_ID = GiottoClass::spatIDs(gobject))
  } else {
    poly_stats <- sf::st_area(sv)[] |>
      dplyr::select(poly_ID, area) |>
      dplyr::collect() |>
      data.table::as.data.table()
    data.table::setnames(poly_stats, old = "poly_ID", new = "cell_ID")

    poly_ids <- sv[] |>
      dplyr::select(poly_ID) |>
      dplyr::collect() |>
      dplyr::pull(poly_ID) |>
      as.character()
    poly_stats <- poly_stats[match(poly_ids, poly_stats$cell_ID), ]
  }

  if (isTRUE(return_gobject)) {
    base_res <- GiottoClass::addCellMetadata(
      gobject = base_res,
      spat_unit = spat_unit,
      feat_type = feat_type,
      new_metadata = poly_stats,
      by_column = TRUE,
      column_cell_ID = "cell_ID"
    )
    return(base_res)
  }

  base_res$poly_stats <- poly_stats
  return(base_res)
}

#' @rdname addStatistics
#' @export
addStatistics.default <- function(gobject, ...) {
  Giotto::addStatistics(gobject = gobject, ...)
}