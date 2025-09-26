#' @noRd
setMethod("processData",
    signature(x = "dbMatrix", param = "logNormParam"),
    function(x, param, ...) {
        # Apply log normalization using database operations
        # First add offset, then apply log transformation
        x[] <- x[] |>
            dplyr::mutate(x = log(x + !!param$offset) / log(!!param$base))
        return(x)
    }
)

#' @noRd
setMethod("processData",
    signature(x = "dbSparseMatrix", param = "logNormParam"),
    function(x, param, ...) {
        # Use the dbMatrix method
        callNextMethod()
    }
)