#' Internal helpers for 2D dimension reduction plotting
#'
#' These wrappers reuse the existing deck.gl and Mosaic implementations for
#' generic `plotUMAP`/`plotPCA` so that the `_2D` S3 methods can dispatch to
#' backend-specific functions without duplicating logic.
#'
#' @keywords internal
#' @noRd
.plotUMAP_2D_deckgl <- function(...) {
  .plotUMAP_deckgl(...)
}


#' @keywords internal
#' @noRd
.plotUMAP_2D_mosaic <- function(...) {
  .plotUMAP_mosaic(...)
}


#' @keywords internal
#' @noRd
.plotPCA_2D_deckgl <- function(...) {
  .plotPCA_deckgl(...)
}


#' @keywords internal
#' @noRd
.plotPCA_2D_mosaic <- function(...) {
  .plotPCA_mosaic(...)
}
