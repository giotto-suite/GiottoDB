#' @include imports.R
NULL

# [.dbSpatial ####

#' Boolean row-subset a dbSpatial object
#'
#' Filters the underlying DuckDB table to rows whose \code{poly_ID} or
#' \code{feat_ID} column matches the selected positions, returning a
#' \code{dbSpatial} with a filtered \code{@value}. This supports
#' \code{filterGiotto} subsetting via GiottoClass internals.
#'
#' All other call signatures fall through to the parent \code{dbData} \code{[}
#' method (which returns the raw \code{tbl_duckdb_connection}).
#'
#' @param x A \code{dbSpatial} object.
#' @param i Logical vector of rows to keep.
#' @return A \code{dbSpatial} (logical \code{i}) or a
#'   \code{tbl_duckdb_connection} (other cases).
#' @rawNamespace exportMethods("[")
#' @noRd
setMethod(
    "[", "dbSpatial",
    function(x, i, j, ..., drop = TRUE) {
        if (missing(i) || !is.logical(i)) {
            return(callNextMethod())
        }
        tbl <- x@value
        id_col <- intersect(c("poly_ID", "feat_ID"), colnames(tbl))
        if (length(id_col) == 0L) {
            return(callNextMethod())
        }
        id_col   <- id_col[[1L]]
        selected <- dplyr::pull(tbl, id_col)[i]
        x@value  <- dplyr::filter(tbl, .data[[id_col]] %in% selected)
        x
    }
)

# spatIDs.giottoPolygon ####

#' spatIDs for dbSpatial-backed giottoPolygon
#'
#' Overrides \pkg{GiottoClass}'s method so that when the \code{spatVector}
#' slot holds a \code{dbSpatial} object (set by \code{as_giottodb}), IDs are
#' pulled from DuckDB rather than via \code{terra::as.list()}.  Falls through
#' to \code{callNextMethod()} for ordinary \code{SpatVector}-backed objects.
#'
#' @importMethodsFrom GiottoClass spatIDs
#' @importClassesFrom GiottoClass giottoPolygon
#' @noRd
setMethod(
    "spatIDs", "giottoPolygon",
    function(x, use_cache = TRUE, uniques = TRUE, ...) {
        if (!all(is.na(x@unique_ID_cache)) &&
            isTRUE(use_cache) &&
            isTRUE(uniques)) {
            return(as.character(x@unique_ID_cache))
        }
        if (inherits(x@spatVector, "dbSpatial")) {
            out <- as.character(dplyr::pull(x@spatVector@value, "poly_ID"))
        } else {
            # Inline GiottoClass logic (callNextMethod() has no target here)
            out <- as.character(terra::as.list(x@spatVector)$poly_ID)
        }
        if (isTRUE(uniques)) out <- unique(out)
        out
    }
)

# featIDs.giottoPoints ####

#' featIDs for dbSpatial-backed giottoPoints
#'
#' Mirror of the \code{spatIDs} override above, for \code{giottoPoints} objects
#' whose \code{spatVector} slot was converted to \code{dbSpatial} by
#' \code{as_giottodb}.
#'
#' @importMethodsFrom GiottoClass featIDs
#' @importClassesFrom GiottoClass giottoPoints
#' @noRd
setMethod(
    "featIDs", "giottoPoints",
    function(x, use_cache = TRUE, uniques = TRUE, ...) {
        if (!all(is.na(x@unique_ID_cache)) &&
            isTRUE(use_cache) &&
            isTRUE(uniques)) {
            return(as.character(x@unique_ID_cache))
        }
        if (inherits(x@spatVector, "dbSpatial")) {
            out <- as.character(dplyr::pull(x@spatVector@value, "feat_ID"))
        } else {
            # Inline GiottoClass logic (callNextMethod() has no target here)
            out <- as.character(terra::as.list(x@spatVector)$feat_ID)
        }
        if (isTRUE(uniques)) out <- unique(out)
        out
    }
)
