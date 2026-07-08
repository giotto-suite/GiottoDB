#' @title Create GiottoPolygon object using dbSpatial
#' @name createGiottoPolygon
#' @description Create a `giottoPolygon` object that wraps a dbSpatial polygons object
#' to support larger-than-memory spatial polygon data. This implementation extends
#' the standard GiottoClass implementation by providing specific methods for
#' dbSpatial objects.
#'
#' @param x dbSpatial object, SpatVector, or data.frame-like object with polygon coordinate
#' information (must include poly_ID column)
#' @param name character. Name for the polygon object
#' @param verbose be verbose
#' @param split_keyword list of character vectors of keywords to split the
#' giottoPolygon object based on their poly_ID. Keywords will be `grepl()`
#' matched against the polygon IDs information.
#' @param unique_IDs (optional) character vector of unique IDs present within
#' the spatVector data. Provided for cacheing purposes
#' @param calc_centroids logical. Whether to calculate centroids for the polygons
#'
#' @return giottoPolygon object wrapping a dbSpatial object
#'
#' @concept Subcellular workflow
#' @export
NULL

#' @rdname createGiottoPolygon
#' @export
setMethod(
  "createGiottoPolygon",
  signature("dbSpatial"),
  function(
    x,
    name = "polygons",
    verbose = TRUE,
    split_keyword = NULL,
    unique_IDs = NULL,
    calc_centroids = FALSE
  ) {
    # Validate input parameters
    checkmate::assert_character(name, len = 1, any.missing = FALSE)
    if (!is.null(split_keyword)) {
      checkmate::assert_list(split_keyword)
    }

    # Check if the dbSpatial object represents polygons
    # Use DuckDB-native geometry typing (no in-memory sfc materialization)
    geom_type_char <- dbSpatial::st_geometrytype(x, collect = TRUE, n = 1)

    if (!grepl("POLYGON", geom_type_char)) {
      stop("The dbSpatial object must contain polygon geometries")
    }

    # Check for required attributes (poly_ID)
    cols <- colnames(x[])
    if (!"poly_ID" %in% cols) {
      stop("The dbSpatial object must have a 'poly_ID' column")
    }

    # Create the giottoPolygon object with dbSpatial backend
    gpolygon <- create_giotto_polygon_object_db(
      name = name,
      dbSpatial = x,
      unique_IDs = unique_IDs,
      calc_centroids = calc_centroids
    )

    # If no split needed, return as is
    if (is.null(split_keyword)) {
      return(gpolygon)
    }

    # Handle splitting based on keywords
    if (verbose) {
      message(
        "Converting dbSpatial to data.frame for splitting based on keywords"
      )
    }

    # Get polygon IDs - collect to avoid DuckDB expression issues
    poly_ids <- x[] |>
      dplyr::pull(poly_ID)

    # Create boolean filters for splitting
    split_bools <- lapply(split_keyword, function(keyword) {
      grepl(paste(keyword, collapse = "|"), poly_ids)
    })

    # Default boolean is polygons not selected by any keyword
    default_bool <- list(!Reduce("|", split_bools))
    split_bools <- c(default_bool, split_bools)

    # Create names for the split objects (using name as a prefix)
    names_list <- paste0(name, "_", seq_along(split_bools))
    names(split_bools) <- names_list

    # Split the dbSpatial object and create list of giottoPolygons
    gpolygon_list <- lapply(seq_along(split_bools), function(i) {
      # Extract indices matching this boolean filter
      indices <- which(split_bools[[i]])

      if (length(indices) > 0) {
        # Get the poly_IDs for this filter instead of using row_number
        selected_poly_ids <- poly_ids[indices]

        # Filter dbSpatial object using %in% with poly_ID values
        filtered_db <- x[] |>
          dplyr::filter(poly_ID %in% selected_poly_ids) |>
          dbProject::to_view()

        # Create new dbSpatial object with filtered data
        filtered_x <- x
        filtered_x[] <- filtered_db

        # Create giottoPolygon with filtered dbSpatial
        gp <- create_giotto_polygon_object_db(
          name = names_list[[i]],
          dbSpatial = filtered_x,
          unique_IDs = NULL,
          calc_centroids = calc_centroids
        )

        return(gp)
      } else {
        return(NULL)
      }
    })

    # Remove any NULL entries (empty filters)
    gpolygon_list <- Filter(Negate(is.null), gpolygon_list)

    # If only one result, return it directly instead of as a list
    if (length(gpolygon_list) == 1) {
      return(gpolygon_list[[1]])
    }

    return(gpolygon_list)
  }
)

#' @title Create giotto polygon object with dbSpatial
#' @name create_giotto_polygon_object_db
#' @param name name for the polygon object
#' @param dbSpatial dbSpatial object containing polygon data
#' @param unique_IDs (optional) unique IDs for cacheing
#' @param calc_centroids logical. Whether to calculate centroids
#' @keywords internal
#' @returns giotto_polygon_object
create_giotto_polygon_object_db <- function(
  name = "polygons",
  dbSpatial = NULL,
  unique_IDs = NULL,
  calc_centroids = FALSE
) {
  # Create minimum giotto polygon object
  g_polygon <- GiottoClass::giottoPolygon(
    name = name,
    spatVector = NULL, # Initialize with NULL, will set to dbSpatial below
    spatVectorCentroids = NULL, # Will be calculated if calc_centroids = TRUE
    overlaps = NULL
  )

  # Verify dbSpatial object
  if (!inherits(dbSpatial, "dbSpatial")) {
    stop("dbSpatial needs to be a dbSpatial object")
  }

  # Store dbSpatial object directly in the spatVector slot
  g_polygon@spatVector <- dbSpatial

  # Handle centroids calculation if requested
  if (isTRUE(calc_centroids)) {
    if (requireNamespace("dbSpatial", quietly = TRUE)) {
      # Calculate centroids using sf::st_centroid which dispatches to dbSpatial
      centroids_db <- sf::st_centroid(dbSpatial)

      # Store centroids
      g_polygon@spatVectorCentroids <- centroids_db
    } else {
      warning("dbSpatial package required for centroid calculation. Skipping.")
    }
  }

  # ID cacheing
  if (is.null(unique_IDs)) {
    ids <- dbSpatial[] |>
      dplyr::pull(poly_ID)

    # Convert to character if necessary
    g_polygon@unique_ID_cache <- as.character(ids)
  } else {
    g_polygon@unique_ID_cache <- as.character(unique_IDs)
  }

  return(g_polygon)
}
