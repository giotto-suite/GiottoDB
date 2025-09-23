#' Calculate Highly Variable Features (HVF) with [`dbMatrix`] support
#'
#' Wraps `Giotto::calculateHVF` to dispatch on [`dbMatrix`]-backed data.
#'
#' @param gobject A [`Giotto`] object
#' @param ... Additional arguments passed to `Giotto::calculateHVF` or [`.calculateHVF_dbMatrix`]
#' @importFrom Giotto calculateHVF
#' @export
calculateHVF <- function(gobject, ...) {
  UseMethod("calculateHVF")
}

#' @rdname calculateHVF
#' @export
calculateHVF.giotto <- function(gobject, ...) {
  dots <- list(...)

  mat <- GiottoClass::getExpression(
    gobject,
    values = "normalized",
    output = "matrix"
  )

  if (inherits(mat, "dbMatrix")) {
    return(do.call(".calculateHVF_dbMatrix", c(list(gobject = gobject), dots)))
  }

  #TODO: replace with S4 methods
  formal_ok <- names(formals(Giotto::calculateHVF))
  call <- match.call()
  call[[1L]] <- quote(Giotto::calculateHVF)
  call <- call[c(TRUE, names(call)[-1L] %in% formal_ok)]
  eval(call, envir = parent.frame())
}
