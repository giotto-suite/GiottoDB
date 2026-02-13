#' Find marker features one-vs-all with GiottoDB support
#'
#' S3 wrapper for \code{Giotto::findMarkers_one_vs_all()}.
#' For \code{GiottoDB} inputs, the specific expression matrix
#' (\code{spat_unit}/\code{feat_type}/\code{expression_values}) is
#' materialized from \code{dbMatrix} to \code{dgCMatrix} before
#' delegating to the Giotto implementation. Only the requested matrix
#' is converted; all other slots remain database-backed.
#'
#' @note For methods that call \code{subsetGiotto()} internally
#' (e.g. \code{"gini"}, \code{"mast"}), this wrapper creates a
#' minimal in-memory \code{giotto} marker object from the requested
#' expression matrix and metadata, instead of converting the full object.
#'
#' @param gobject A \code{giotto} or \code{GiottoDB} object
#' @param spat_unit spatial unit (default: object default)
#' @param feat_type feature type (default: object default)
#' @param expression_values expression values to use
#' @param ... Additional arguments passed to
#'   \code{Giotto::findMarkers_one_vs_all}
#' @return data.table of marker results
#' @export
findMarkers_one_vs_all <- function(gobject, ...) {
  UseMethod("findMarkers_one_vs_all")
}

#' @rdname findMarkers_one_vs_all
#' @export
findMarkers_one_vs_all.GiottoDB <- function(
    gobject,
    spat_unit = NULL,
    feat_type = NULL,
    expression_values = c("normalized", "scaled", "custom"),
    ...) {
  dots <- list(...)
  marker_method <- NULL
  if (!is.null(dots$method)) {
    marker_method <- tolower(as.character(dots$method)[1])
  }

  # Resolve defaults
  spat_unit <- GiottoClass::set_default_spat_unit(
    gobject = gobject, spat_unit = spat_unit
  )
  feat_type <- GiottoClass::set_default_feat_type(
    gobject = gobject, spat_unit = spat_unit, feat_type = feat_type
  )
  expression_values <- match.arg(
    expression_values,
    choices = unique(c("normalized", "scaled", "custom", expression_values))
  )

  # Get the specific exprObj

  expr_obj <- GiottoClass::getExpression(
    gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    values = expression_values,
    output = "exprObj"
  )
  expr_mat <- methods::slot(expr_obj, "exprMat")

  # Materialize dbMatrix -> dgCMatrix if needed

  if (inherits(expr_mat, "dbMatrix")) {
    # Temporarily lift mem limit for controlled coercion
    old_limit <- getOption("dbMatrix.max_mem_convert")
    old_verbose <- getOption("dbMatrix.verbose")
    options(dbMatrix.max_mem_convert = Inf, dbMatrix.verbose = FALSE)
    on.exit({
      options(
        dbMatrix.max_mem_convert = old_limit,
        dbMatrix.verbose = old_verbose
      )
    }, add = TRUE)

    methods::slot(expr_obj, "exprMat") <- methods::as(expr_mat, "dgCMatrix")
    expr_mat <- methods::slot(expr_obj, "exprMat")

  }

  if (!is.null(marker_method) && marker_method %in% c("gini", "mast")) {
    cell_metadata <- GiottoClass::getCellMetadata(
      gobject,
      spat_unit = spat_unit,
      feat_type = feat_type,
      output = "data.table",
      copy_obj = TRUE
    )
    feat_metadata <- GiottoClass::getFeatureMetadata(
      gobject,
      spat_unit = spat_unit,
      feat_type = feat_type,
      output = "data.table",
      copy_obj = TRUE
    )

    marker_gobject <- GiottoClass::createGiottoObject(
      expression = expr_mat,
      expression_feat = feat_type,
      cell_metadata = cell_metadata,
      feat_metadata = feat_metadata,
      verbose = FALSE
    )

    return(Giotto::findMarkers_one_vs_all(
      gobject = marker_gobject,
      expression_values = "raw",
      ...
    ))
  }

  if (inherits(expr_mat, "dgCMatrix")) {
    gobject <- GiottoClass::setExpression(
      gobject,
      x = expr_obj,
      spat_unit = spat_unit,
      feat_type = feat_type,
      name = expression_values,
      verbose = FALSE,
      initialize = FALSE
    )
  }

  Giotto::findMarkers_one_vs_all(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    expression_values = expression_values,
    ...
  )
}

#' @rdname findMarkers_one_vs_all
#' @export
findMarkers_one_vs_all.giotto <- function(gobject, ...) {
  Giotto::findMarkers_one_vs_all(gobject = gobject, ...)
}
