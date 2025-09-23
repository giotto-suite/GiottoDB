#' @title Tessellate a \code{\link{dbSpatial}}  object
#' @name tessellate
#' @description
#' Creates a tessellation on the extent of \code{\link{dbSpatial}}  with specified parameters.
#' @param \code{\link{dbSpatial}}  object
#' @param name \code{character string} name of table to add to \code{\link{dbSpatial}} object. Default: "tessellation".
#' @param geomName \code{character string}. The geometry column name in the  \code{\link{dbSpatial}}  object. Default: `"geom"`.
#' @param shape \code{character string}. A character string indicating the shape of the tessellation.
#'   Options are "hexagon" or "square".
#' @param shape_size \code{numeric}. the size of the shape in the tessellation.
#'   If `NULL`, a default size is calculated. See `GiottoClass::tessellate` for details.
#' @param gap \code{numeric}. Value indicating the gap between tessellation shapes. Defaults to 0.
#' @param radius \code{numeric}. Value specifying the radius for hexagonal tessellation.
#'   This parameter is ignored for square tessellations.
#' @param overwrite \code{logical}. Boolean value indicating whether to overwrite an
#' existing tessellation with the same name. Default: `FALSE`.
#' @param ... Additional arguments passed to methods.
#'
#' @return \code{\link{dbSpatial}} object
#' @family geom_construction
#' @export
#' @examples
#' coordinates <- data.frame(x = c(100, 200, 300), y = c(500, 600, 700))
#' attributes <- data.frame(id = 1:3, name = c("A", "B", "C"))
#'
#' # Combine the coordinates and attributes
#' dummy_data <- cbind(coordinates, attributes)
#'
#' # Create a duckdb connection
#' con = DBI::dbConnect(duckdb::duckdb(), ":memory:")
#'
#' # Create a duckdb table with spatial points
#' db_points = dbSpatial(conn = con,
#'                       value = dummy_data,
#'                       x_colName = "x",
#'                       y_colName = "y",
#'                       name = "foo",
#'                       overwrite = TRUE)
#'
#' tessellate(db_points, name = "my_tessellation", shape = "hexagon", shape_size = 60)
setGeneric(
  "tessellate",
  function(
    dbSpatial,
    geomName = "geom",
    name = "tessellation",
    shape = c("hexagon", "square"),
    shape_size = NULL,
    gap = 0,
    radius = NULL,
    overwrite = FALSE,
    ...
  ) {
    standardGeneric("tessellate")
  }
)

#' @describeIn tessellate Method for `dbSpatial` object
setMethod(
  "tessellate",
  signature(dbSpatial = "dbSpatial"),
  function(dbSpatial, ...) .tessellate(dbSpatial, ...)
)

#' @keywords internal
.tessellate <- function(
  dbSpatial,
  geomName = "geom",
  name = "tessellation",
  shape = c("hexagon", "square"),
  shape_size = NULL,
  gap = 0,
  radius = NULL,
  overwrite = FALSE,
  ...
) {
  tbl <- dbSpatial[]
  con <- dbplyr::remote_con(tbl)
  dbSpatial:::.check_con(conn = con) #FIXME: dbProject update
  dbSpatial:::.check_name(name = name) #FIXME: dbProject update
  dbSpatial:::.check_tbl(tbl = tbl) #FIXME: dbProject update
  dbSpatial:::.check_geomName(value = tbl, geomName = geomName) #FIXME: dbProject update
  dbSpatial:::.check_overwrite(conn = con, overwrite = overwrite, name = name) #FIXME: dbProject update

  if (!shape %in% c("hexagon", "square")) {
    stop("shape must be either 'hexagon' or 'square'")
  }

  # ensure shape_size, gap and radius are numerical values
  # TODO: move to GiottoClass::tessellate
  if (!is.null(shape_size)) {
    if (!is.numeric(shape_size)) {
      stop("shape_size must be a numerical value")
    }
  }
  if (!is.null(gap)) {
    if (!is.numeric(gap)) {
      stop("gap must be a numerical value")
    }
  }
  if (!is.null(radius)) {
    if (!is.numeric(radius)) {
      stop("radius must be a numerical value")
    }
  }

  # in-memory processing -------------------------------------------------------

  # dbSpatial parameters
  ext = dbSpatial::st_extent(dbSpatial, geomName = geomName)

  # retrieve tessellations
  # TODO: compute in db
  gpolys <- GiottoClass::tessellate(extent = ext, shape = shape, shape_size)

  # convert terra geoms to dbSpatial
  res <- gpolys[] |>
    dbSpatial::as_dbSpatial(conn = con, overwrite = overwrite, name = name)

  return(res)
}
