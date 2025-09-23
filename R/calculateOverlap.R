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
    feat_subset_ids = NULL,
    count_info_column = NULL,
    verbose = TRUE,
    ...
  ) {
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

      name <- as.name(name)
      input[] <- dplyr::filter(input[], name %in% ids) |>
        dbMatrix::to_view() # TODO: migrate to dbProject

      return(input)
    }

    # subset
    x <- .subset(input = x, name = "poly_ID", ids = poly_subset_ids)
    y <- .subset(input = y, name = feat_subset_column, ids = feat_subset_ids)

    # intersect
    output_name <- paste0(
      'intersect_',
      paste(sample(LETTERS, 9), collapse = '')
    )
    g1_cols_keep <- x[] |>
      dplyr::select(!tidyselect::any_of("geom")) |>
      colnames()
    res <- dbSpatial::st_intersects(
      g1 = x,
      g1_cols_keep = g1_cols_keep,
      g2 = y,
      name = output_name,
      ...
    )

    return(res)
  }
)
