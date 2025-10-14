#' @name processData-dbMatrix-methods
#' @title Count Matrix Processing for GiottoDB Objects
#' @description
#' processData contains methods for GiottoDB objects that provide various normalization
#' and scaling operations on count matrices as implemented in Giotto.
#'
#' Working methods delegate to existing implementations in Giotto:
#' * Library normalization (\code{libraryNormParam})
#' * Log normalization (\code{logNormParam})
#' * osmFISH normalization (\code{osmFISHNormParam})
#' * Arcsinh transformation (\code{arcsinhNormParam})
#' * L2 normalization (\code{l2NormParam})
#' * Z-score scaling (\code{zscoreScaleParam})
#' * Default normalization (\code{defaultNormParam}) - combines library + log
#' * List processing - enables composable operations
#'
#' Unsupported methods (TODO):
#' * TF-IDF normalization
#' * Quantile normalization
#' * Pearson residuals normalization
#'
#' @usage processData(x, param, ...)
#' @param x dbMatrix object
#' @param param S4 parameter class defining the transform operation. Can be:
#' * \code{libraryNormParam} - library size normalization
#' * \code{logNormParam} - log transformation
#' * \code{osmFISHNormParam} - osmFISH normalization
#' * \code{arcsinhNormParam} - arcsinh transformation
#' * \code{l2NormParam} - L2/Euclidean normalization
#' * \code{zscoreScaleParam} - z-score scaling
#' * \code{defaultNormParam} - default normalization (library + log)
#' * \code{list} - for chained operations
#' * \code{tfidfNormParam} - TODO
#' * \code{quantileNormParam} - TODO
#' * \code{pearsonResidNormParam} - TODO
#' @param \dots additional params to pass to the underlying methods
#' @returns A dbMatrix object
#' @details
#' All results are lazily evaluated. Please read [collapse.tbl_sql] to compute/save results to the db.
#' @examples
#' \dontrun{
#' # Create a dbMatrix object
#' library(dbMatrix)
#' con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
#' mat <- matrix(rpois(100, 5), nrow = 10)
#' dbmat <- dbMatrix(mat, con = con, name = "test")
#'
#' # Library normalization
#' lib_norm <- processData(dbmat, normParam("library"))
#'
#' # Log normalization
#' log_norm <- processData(dbmat, normParam("log"))
#'
#' # Chained operations
#' scaled <- processData(dbmat, list(
#'   normParam("library"),
#'   normParam("log"),
#'   scaleParam("zscore")
#' ))
#' }
#' @seealso
#' \code{\link[Giotto]{processData}} for the generic and other methods
#'
#' \code{\link[Giotto]{processExpression}} for use with giotto objects
#'
#' \code{\link[Giotto]{normParam}}, \code{\link[Giotto]{scaleParam}} for parameter creation
#' @export
setMethod(
  "processData",
  signature(x = "dbMatrix", param = "libraryNormParam"),
  function(x, param, ...) {
    # Get and call the allMatrix method directly to avoid recursion
    allMatrix_method <- getMethod(
      "processData",
      signature("allMatrix", "libraryNormParam")
    )
    allMatrix_method(x, param, ...)
  }
)

#' @noRd
setMethod(
  "processData",
  signature(x = "dbSparseMatrix", param = "libraryNormParam"),
  function(x, param, ...) {
    # Use the dbMatrix method
    callNextMethod()
  }
)

# List processing method for dbMatrix (needed for defaultNormParam)
#' @noRd
setMethod(
  "processData",
  signature(x = "dbMatrix", param = "list"),
  function(x, param, ...) {
    # Get and call the allMatrix method directly
    allMatrix_method <- getMethod("processData", signature("allMatrix", "list"))
    allMatrix_method(x, param, ...)
  }
)

#' @noRd
setMethod(
  "processData",
  signature(x = "dbSparseMatrix", param = "list"),
  function(x, param, ...) {
    # Use the dbMatrix method
    callNextMethod()
  }
)
# L2 Normalization delegation method for dbMatrix
#' @noRd
setMethod(
  "processData",
  signature(x = "dbMatrix", param = "l2NormParam"),
  function(x, param, ...) {
    # Get and call the allMatrix method directly
    allMatrix_method <- getMethod(
      "processData",
      signature("allMatrix", "l2NormParam")
    )
    allMatrix_method(x, param, ...)
  }
)

#' @noRd
setMethod(
  "processData",
  signature(x = "dbSparseMatrix", param = "l2NormParam"),
  function(x, param, ...) {
    # Use the dbMatrix method
    callNextMethod()
  }
)


# TF-IDF Normalization - informative error (computational differences detected)
#' @noRd
setMethod(
  "processData",
  signature(x = "dbMatrix", param = "tfidfNormParam"),
  function(x, param, ...) {
    stop(
      "TF-IDF normalization is not currently supported for dbMatrix objects.",
      call. = FALSE
    )
  }
)

#' @noRd
setMethod(
  "processData",
  signature(x = "dbSparseMatrix", param = "tfidfNormParam"),
  function(x, param, ...) {
    # Use the same error as dbMatrix
    callNextMethod()
  }
)

# Quantile Normalization - informative error (missing colRanks support)
#' @noRd
setMethod(
  "processData",
  signature(x = "dbMatrix", param = "quantileNormParam"),
  function(x, param, ...) {
    stop(
      "Quantile normalization is not currently supported for dbMatrix objects.",
      call. = FALSE
    )
  }
)

#' @noRd
setMethod(
  "processData",
  signature(x = "dbSparseMatrix", param = "quantileNormParam"),
  function(x, param, ...) {
    # Use the same error as dbMatrix
    callNextMethod()
  }
)

# osmFISH Normalization delegation method for dbMatrix
#' @noRd
setMethod(
  "processData",
  signature(x = "dbMatrix", param = "osmFISHNormParam"),
  function(x, param, ...) {
    # Get and call the allMatrix method directly
    allMatrix_method <- getMethod(
      "processData",
      signature("allMatrix", "osmFISHNormParam")
    )
    allMatrix_method(x, param, ...)
  }
)

#' @noRd
setMethod(
  "processData",
  signature(x = "dbSparseMatrix", param = "osmFISHNormParam"),
  function(x, param, ...) {
    # Use the dbMatrix method
    callNextMethod()
  }
)

# Arcsinh Normalization delegation method for dbMatrix
#' @noRd
setMethod(
  "processData",
  signature(x = "dbMatrix", param = "arcsinhNormParam"),
  function(x, param, ...) {
    # Get and call the allMatrix method directly
    allMatrix_method <- getMethod(
      "processData",
      signature("allMatrix", "arcsinhNormParam")
    )
    allMatrix_method(x, param, ...)
  }
)

#' @noRd
setMethod(
  "processData",
  signature(x = "dbSparseMatrix", param = "arcsinhNormParam"),
  function(x, param, ...) {
    # Use the dbMatrix method
    callNextMethod()
  }
)

# Z-score Scaling delegation method for dbMatrix
#' @noRd
setMethod(
  "processData",
  signature(x = "dbMatrix", param = "zscoreScaleParam"),
  function(x, param, ...) {
    # Get and call the allMatrix method directly
    allMatrix_method <- getMethod(
      "processData",
      signature("allMatrix", "zscoreScaleParam")
    )
    allMatrix_method(x, param, ...)
  }
)

#' @noRd
setMethod(
  "processData",
  signature(x = "dbSparseMatrix", param = "zscoreScaleParam"),
  function(x, param, ...) {
    # Use the dbMatrix method
    callNextMethod()
  }
)
