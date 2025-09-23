#' @title Create GiottoPoints object using dbSpatial
#' @name createGiottoPoints
#' @description Create a `giottoPoints` object that wraps a dbSpatial points object
#' to support larger-than-memory spatial point data. This implementation extends
#' the standard GiottoClass implementation by providing specific methods for
#' dbSpatial objects.
#'
#' @param x dbSpatial object, SpatVector, or data.frame-like object with points coordinate
#' information (x, y, feat_ID)
#' @param feat_type character. feature type. Provide more than one value if
#' using the `split_keyword` param. For each set of keywords to split by, an
#' additional feat_type should be provided in the same order.
#' @param verbose be verbose
#' @param split_keyword list of character vectors of keywords to split the
#' giottoPoints object based on their feat_ID. Keywords will be `grepl()`
#' matched against the feature IDs information.
#' @param unique_IDs (optional) character vector of unique IDs present within
#' the spatVector data. Provided for cacheing purposes
#'
#' @returns giottoPoints object wrapping a dbSpatial object
#'
#'
#' @export
NULL

#' @rdname createGiottoPoints
#' @export
setMethod(
  "createGiottoPoints",
  signature("dbSpatial"),
  function(
    x,
    feat_type = "rna",
    verbose = TRUE,
    split_keyword = NULL,
    unique_IDs = NULL
  ) {
    # Validate input parameters
    checkmate::assert_character(feat_type)
    if (!is.null(split_keyword)) {
      checkmate::assert_list(split_keyword)
    }

    # Check if the dbSpatial object represents points
    # Get geometry type and safely convert to character for comparison
    geom_type <- dbSpatial::st_geometrytype(x)
    # Extract the first geometry type as a character string
    geom_type_char <- geom_type[] |>
      head(n = 1) |>
      dplyr::pull(geom) |>
      as.character()

    if (!grepl("POINT", geom_type_char)) {
      stop("The dbSpatial object must contain point geometries")
    }

    # Check for required attributes (x, y, feat_ID, feat_ID_uniq)
    cols <- colnames(x[])
    if (!"feat_ID" %in% cols) {
      stop("The dbSpatial object must have a 'feat_ID' column")
    }

    # Add feat_ID_uniq if not present
    if (!"feat_ID_uniq" %in% cols) {
      if (verbose) {
        message("Adding 'feat_ID_uniq' column to dbSpatial object")
      }
      # Create a view with feat_ID_uniq added as ROW_NUMBER()
      x[] <- dplyr::mutate(x[], feat_ID_uniq = dplyr::row_number())
    }

    # Create the giottoPoints object with dbSpatial backend
    gpoints <- create_giotto_points_object_db(
      feat_type = feat_type[[1]],
      dbSpatial = x,
      unique_IDs = unique_IDs
    )

    # If no split needed, return as is
    if (is.null(split_keyword)) {
      return(gpoints)
    }

    # Handle splitting based on keywords
    # We need to convert the dbSpatial object to terra SpatVector to get feat IDs
    if (verbose) {
      message(
        "Converting dbSpatial to SpatVector for splitting based on keywords"
      )
    }

    # Get feature IDs
    feat_ids <- x[] |>
      dplyr::select(feat_ID) |>
      dplyr::collect() |>
      dplyr::pull(feat_ID)

    # Create boolean filters for splitting
    split_bools <- lapply(split_keyword, function(keyword) {
      grepl(paste(keyword, collapse = "|"), feat_ids)
    })

    # Default boolean is points not selected by any keyword
    default_bool <- list(!Reduce("|", split_bools))
    split_bools <- c(default_bool, split_bools)
    names(split_bools) <- feat_type

    # Split the dbSpatial object and create list of giottoPoints
    gpoints_list <- lapply(seq_along(split_bools), function(i) {
      # Extract indices matching this boolean filter
      indices <- which(split_bools[[i]])

      if (length(indices) > 0) {
        # Filter dbSpatial object
        filtered_db <- x[] |>
          dplyr::filter(dplyr::row_number() %in% indices) |>
          dbMatrix::to_view()

        # Create new dbSpatial object with filtered data
        filtered_x <- x
        filtered_x[] <- filtered_db

        # Create giottoPoints with filtered dbSpatial
        gp <- create_giotto_points_object_db(
          feat_type = feat_type[[i]],
          dbSpatial = filtered_x,
          unique_IDs = NULL
        )

        # Set object name
        objName(gp) <- feat_type[[i]]
        return(gp)
      } else {
        return(NULL)
      }
    })

    # Remove any NULL entries (empty filters)
    gpoints_list <- Filter(Negate(is.null), gpoints_list)

    # If only one result, return it directly instead of as a list
    if (length(gpoints_list) == 1) {
      return(gpoints_list[[1]])
    }

    return(gpoints_list)
  }
)

#' @title Create giotto points object with dbSpatial
#' @name create_giotto_points_object_db
#' @param feat_type feature type
#' @param dbSpatial dbSpatial object containing point data
#' @param networks (optional) feature network object
#' @param unique_IDs (optional) unique IDs for cacheing
#' @keywords internal
#' @returns giotto_points_object
create_giotto_points_object_db <- function(
  feat_type = "rna",
  dbSpatial = NULL,
  networks = NULL,
  unique_IDs = NULL
) {
  if (is.null(feat_type)) {
    feat_type <- NA
  } # compliance with featData class

  # Create minimum giotto points object
  g_points <- GiottoClass::giottoPoints(
    feat_type = feat_type,
    spatVector = NULL, # Initialize with NULL, will set to dbSpatial below
    networks = NULL
  )

  # Verify dbSpatial object
  if (!inherits(dbSpatial, "dbSpatial")) {
    stop("dbSpatial needs to be a dbSpatial object")
  }

  # Store dbSpatial object directly in the spatVector slot
  g_points@spatVector <- dbSpatial

  # Provide feature id
  g_points@feat_type <- feat_type

  # Feature_network object
  g_points@networks <- networks

  # feat_ID cacheing
  if (is.null(unique_IDs)) {
    ids <- dbSpatial[] |>
      dplyr::arrange(feat_ID_uniq) |>
      dplyr::distinct(feat_ID) |>
      dplyr::pull(feat_ID)

    g_points@unique_ID_cache <- as.character(ids)
  } else {
    g_points@unique_ID_cache <- as.character(unique_IDs)
  }

  return(g_points)
}
