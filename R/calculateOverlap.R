# * dbSpatial dbSpatial ####
#' @inheritParams GiottoClass::calculateOverlap
#' @export
setMethod(
  "calculateOverlap",
  signature(x = "dbSpatial", y = "dbSpatial"),
  function(
    x,
    y,
    poly_subset_ids = NULL,
    feat_subset_column = NULL,
    feat_subset_values = NULL,
    feat_count_column = NULL,
    feat_subset_ids = NULL,
    count_info_column = NULL,
    verbose = TRUE,
    ...
  ) {
    # Backwards compatibility with GiottoClass <= 0.5.0 argument names
    if (is.null(feat_subset_values) && !is.null(feat_subset_ids)) {
      feat_subset_values <- feat_subset_ids
    }
    if (is.null(feat_count_column) && !is.null(count_info_column)) {
      feat_count_column <- count_info_column
    }

    # Ensure we are doing polygon-to-points overlaps only.
    # This mirrors the GiottoClass intent (x = polygons, y = points) and
    # guarantees the overlap result keeps point geometry.
    x_geom_type <- dbSpatial::st_geometrytype(x, collect = TRUE, n = 1)
    y_geom_type <- dbSpatial::st_geometrytype(y, collect = TRUE, n = 1)

    if (!grepl("POLYGON", x_geom_type)) {
      stop(
        "calculateOverlap(dbSpatial, dbSpatial) expects `x` to contain polygon geometries",
        call. = FALSE
      )
    }
    if (!grepl("POINT", y_geom_type)) {
      stop(
        "calculateOverlap(dbSpatial, dbSpatial) expects `y` to contain point geometries",
        call. = FALSE
      )
    }

    # input validation
    if (!is.null(poly_subset_ids)) {
      checkmate::assert_character(poly_subset_ids)
    }

    .subset <- function(input, name, ids) {
      if (is.null(ids)) {
        # no subset
        return(input)
      }

      if (length(ids) == 0) {
        # empty vector
        return(input)
      }

      cols <- colnames(input[])
      if (!(name %in% cols)) {
        # column not found
        return(input)
      }

      input[] <- dplyr::filter(input[], .data[[name]] %in% ids) |>
        dbProject::to_view()

      return(input)
    }

    # subset
    x <- .subset(input = x, name = "poly_ID", ids = poly_subset_ids)
    y <- .subset(input = y, name = feat_subset_column, ids = feat_subset_values)

    # GiottoClass' SpatVector workflow returns overlapped POINTS (not polygons),
    # so we join points (y) to polygons (x) to keep point geometry.
    res <- sf::st_join(y, x, join = sf::st_intersects, overwrite = TRUE)

    return(res)
  }
)
