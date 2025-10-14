#' @noRd
setMethod(
  "processData",
  signature(x = "dbMatrix", param = "pearsonResidNormParam"),
  function(x, param, ...) {
    stop(
      "Pearson residual normalization is not currently supported for dbMatrix objects.",
      call. = FALSE
    )
  }
)

#' @noRd
setMethod(
  "processData",
  signature(x = "dbSparseMatrix", param = "pearsonResidNormParam"),
  function(x, param, ...) {
    # Use the dbMatrix method
    callNextMethod()
  }
)
