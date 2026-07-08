#' Normalize Expression for GiottoDB with dbMatrix support
#'
#' S3 generic that dispatches to `Giotto::normalizeGiotto` with optimizations
#' for [`dbMatrix`]-backed expression data.
#'
#' @description
#' For GiottoDB objects with dbMatrix expression, scaling (centering) is handled
#' implicitly during PCA via `db_svd`. If `scale_feats` or `scale_cells` are
#' `TRUE`, a warning is emitted and both are silently forced to `FALSE`. No
#' "scaled" expression slot is created.
#'
#' @param gobject A giotto or GiottoDB object
#' @param scale_feats Not supported for GiottoDB. Forced to `FALSE` with a
#'   warning. Centering is performed inside `runPCA` via `db_svd`.
#' @param scale_cells Not supported for GiottoDB. Forced to `FALSE` with a
#'   warning. Centering is performed inside `runPCA` via `db_svd`.
#' @param ... Additional arguments passed to `Giotto::normalizeGiotto`
#' @concept Expression processing
#' @importFrom Giotto normalizeGiotto
#' @export
normalizeGiotto <- function(gobject, ...) {
  UseMethod("normalizeGiotto")
}

#' @rdname normalizeGiotto
#' @export
normalizeGiotto.GiottoDB <- function(
    gobject,
    spat_unit = NULL,
    feat_type = NULL,
    expression_values = "raw",
    norm_methods = c("standard", "pearson_resid", "osmFISH", "quantile"),
    library_size_norm = TRUE,
    scalefactor = 6e3,
    log_norm = TRUE,
    log_offset = 1,
    logbase = 2,
    scale_feats = TRUE,
    scale_genes = lifecycle::deprecated(),
    scale_cells = TRUE,
    scale_order = c("first_feats", "first_cells"),
    theta = 100,
    name = "scaled",
    update_slot = lifecycle::deprecated(),
    verbose = TRUE,
    ...
) {
  # Always force FALSE for GiottoDB - centering is done in db_svd
  if (isTRUE(scale_feats) || isTRUE(scale_cells)) {
    cli::cli_warn(c(
      "!" = "{.arg scale_feats} and {.arg scale_cells} are not supported for GiottoDB objects."
    ))
  }
  
  Giotto::normalizeGiotto(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    expression_values = expression_values,
    norm_methods = norm_methods,
    library_size_norm = library_size_norm,
    scalefactor = scalefactor,
    log_norm = log_norm,
    log_offset = log_offset,
    logbase = logbase,
    scale_feats = FALSE,  # Force FALSE
    scale_genes = scale_genes,
    scale_cells = FALSE,  # Force FALSE
    scale_order = scale_order,
    theta = theta,
    name = name,
    update_slot = update_slot,
    verbose = verbose
  )
}

#' @rdname normalizeGiotto
#' @export
normalizeGiotto.giotto <- function(
    gobject,
    spat_unit = NULL,
    feat_type = NULL,
    expression_values = "raw",
    norm_methods = c("standard", "pearson_resid", "osmFISH", "quantile"),
    library_size_norm = TRUE,
    scalefactor = 6e3,
    log_norm = TRUE,
    log_offset = 1,
    logbase = 2,
    scale_feats = TRUE,
    scale_genes = lifecycle::deprecated(),
    scale_cells = TRUE,
    scale_order = c("first_feats", "first_cells"),
    theta = 100,
    name = "scaled",
    update_slot = lifecycle::deprecated(),
    verbose = TRUE,
    ...
) {
  # For giotto objects, pass through to Giotto with explicit parameters
  # to avoid match.call issues with GiottoUtils::get_args
  Giotto::normalizeGiotto(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    expression_values = expression_values,
    norm_methods = norm_methods,
    library_size_norm = library_size_norm,
    scalefactor = scalefactor,
    log_norm = log_norm,
    log_offset = log_offset,
    logbase = logbase,
    scale_feats = scale_feats,
    scale_genes = scale_genes,
    scale_cells = scale_cells,
    scale_order = scale_order,
    theta = theta,
    name = name,
    update_slot = update_slot,
    verbose = verbose
  )
}
