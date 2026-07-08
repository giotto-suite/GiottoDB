#' Calculate Highly Variable Features for GiottoDB
#'
#' S3 method that prevents accessing "scaled" expression to avoid materialization.
#'
#' @inheritParams Giotto::calculateHVF
#' @concept Expression processing
#' @importFrom Giotto calculateHVF
#' @export
calculateHVF <- function(gobject, ...) {
  UseMethod("calculateHVF")
}

#' @rdname calculateHVF
#' @export
calculateHVF.GiottoDB <- function(
    gobject,
    spat_unit = NULL,
    feat_type = NULL,
    expression_values = "normalized",
    method = c("cov_groups", "cov_loess", "var_p_resid"),
    reverse_log_scale = FALSE,
    logbase = 2,
    expression_threshold = 0,
    nr_expression_groups = 20,
    zscore_threshold = 1.5,
    HVFname = "hvf",
    difference_in_cov = 0.1,
    var_threshold = 1.5,
    var_number = NULL,
    random_subset = NULL,
    set_seed = TRUE,
    seed_number = 1234,
    show_plot = NULL,
    return_plot = NULL,
    save_plot = NULL,
    save_param = list(),
    default_save_name = "HVFplot",
    return_gobject = TRUE,
    calc_gini = TRUE,
    verbose = TRUE
) {
  if (expression_values == "scaled") {
    stop("'scaled' expression is not supported for GiottoDB. ",
         "Use expression_values = 'normalized'. ",
         "Scaling is handled implicitly in runPCA via db_svd.")
  }
  
  Giotto::calculateHVF(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    expression_values = expression_values,
    method = method,
    reverse_log_scale = reverse_log_scale,
    logbase = logbase,
    expression_threshold = expression_threshold,
    nr_expression_groups = nr_expression_groups,
    zscore_threshold = zscore_threshold,
    HVFname = HVFname,
    difference_in_cov = difference_in_cov,
    var_threshold = var_threshold,
    var_number = var_number,
    random_subset = random_subset,
    set_seed = set_seed,
    seed_number = seed_number,
    show_plot = show_plot,
    return_plot = return_plot,
    save_plot = save_plot,
    save_param = save_param,
    default_save_name = default_save_name,
    return_gobject = return_gobject,
    calc_gini = calc_gini,
    verbose = verbose
  )
}

#' @rdname calculateHVF
#' @export
calculateHVF.giotto <- function(
    gobject,
    spat_unit = NULL,
    feat_type = NULL,
    expression_values = c("normalized", "scaled", "custom"),
    method = c("cov_groups", "cov_loess", "var_p_resid"),
    reverse_log_scale = FALSE,
    logbase = 2,
    expression_threshold = 0,
    nr_expression_groups = 20,
    zscore_threshold = 1.5,
    HVFname = "hvf",
    difference_in_cov = 0.1,
    var_threshold = 1.5,
    var_number = NULL,
    random_subset = NULL,
    set_seed = TRUE,
    seed_number = 1234,
    show_plot = NULL,
    return_plot = NULL,
    save_plot = NULL,
    save_param = list(),
    default_save_name = "HVFplot",
    return_gobject = TRUE,
    calc_gini = TRUE,
    verbose = TRUE
) {
  Giotto::calculateHVF(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    expression_values = expression_values,
    method = method,
    reverse_log_scale = reverse_log_scale,
    logbase = logbase,
    expression_threshold = expression_threshold,
    nr_expression_groups = nr_expression_groups,
    zscore_threshold = zscore_threshold,
    HVFname = HVFname,
    difference_in_cov = difference_in_cov,
    var_threshold = var_threshold,
    var_number = var_number,
    random_subset = random_subset,
    set_seed = set_seed,
    seed_number = seed_number,
    show_plot = show_plot,
    return_plot = return_plot,
    save_plot = save_plot,
    save_param = save_param,
    default_save_name = default_save_name,
    return_gobject = return_gobject,
    calc_gini = calc_gini,
    verbose = verbose
  )
}
