#' @title Tessellate a \code{\link{dbSpatial}} object
#' @name tessellate
#' @description
#' Creates a tessellation on the extent of a \code{\link{dbSpatial}} object with specified parameters.
#' Other input types are forwarded to \code{GiottoClass::tessellate()}.
#' @details
#' \code{tessellate(x = dbSpatial, ...)} creates a tessellation over the bounding box of the
#' \code{\link{dbSpatial}} geometry column and stores the result as a \code{\link{dbSpatial}} table.
#' \code{tessellate(extent = extent, ...)} and \code{tessellate(extent, ...)} preserve the
#' \code{GiottoClass::tessellate()} extent-based behavior. Supply either \code{x} or
#' \code{extent}, but not both.
#' @param x \code{\link{dbSpatial}} object, or a positional extent-like object supported by
#'   \code{GiottoClass::tessellate()}.
#' @param extent Optional named extent-like object supported by \code{GiottoClass::tessellate()}.
#' @param name \code{character string} name of table to add to \code{\link{dbSpatial}} object. Default: "tessellation".
#' @param geomName \code{character string}. The geometry column name in the \code{\link{dbSpatial}} object. Default: `"geom"`.
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
#' @return \code{\link{dbSpatial}} object for dbSpatial input, otherwise the result of \code{GiottoClass::tessellate()}.
#' @family geom_construction
#' @concept Subcellular workflow
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
#' db_points = dbSpatial::dbSpatial(conn = con,
#'                                  value = dummy_data,
#'                                  x_colName = "x",
#'                                  y_colName = "y",
#'                                  name = "foo",
#'                                  overwrite = TRUE)
#'
#' tessellate(db_points, name = "my_tessellation", shape = "hexagon", shape_size = 60)
setGeneric(
  "tessellate",
  function(x, extent, ...) {
    standardGeneric("tessellate")
  }
)

#' @describeIn tessellate Method for `dbSpatial` objects
setMethod(
  "tessellate",
  signature(x = "dbSpatial", extent = "missing"),
  function(
    x,
    extent,
    geomName = "geom",
    name = "tessellation",
    shape = c("hexagon", "square"),
    shape_size = NULL,
    gap = 0,
    radius = NULL,
    overwrite = FALSE,
    ...
  ) {
    .tessellate(
      dbSpatial = x,
      geomName = geomName,
      name = name,
      shape = shape,
      shape_size = shape_size,
      gap = gap,
      radius = radius,
      overwrite = overwrite,
      ...
    )
  }
)

# Preserve GiottoClass-style named extent calls when GiottoDB is attached.
setMethod(
  "tessellate",
  signature(x = "missing", extent = "ANY"),
  function(x, extent, ...) {
    GiottoClass::tessellate(extent = extent, ...)
  }
)

# Preserve GiottoClass-style positional extent calls when GiottoDB is attached.
setMethod(
  "tessellate",
  signature(x = "ANY", extent = "missing"),
  function(x, extent, ...) {
    GiottoClass::tessellate(extent = x, ...)
  }
)

setMethod(
  "tessellate",
  signature(x = "ANY", extent = "ANY"),
  function(x, extent, ...) {
    stop("Provide either `x` or `extent`, not both.", call. = FALSE)
  }
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
  dbProject::.check_con(conn = con)
  dbProject::.check_name(name = name)
  dbProject::.check_tbl(tbl = tbl)
  dbProject::.check_overwrite(conn = con, overwrite = overwrite, name = name)

  shape <- match.arg(shape)

  # ensure shape_size, gap and radius are numerical values
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
  ext <- terra::ext(sf::st_bbox(dbSpatial, geomName = geomName))

  # retrieve tessellations
  # TODO: compute in db
  gpolys <- GiottoClass::tessellate(
    extent = ext,
    shape = shape,
    shape_size = shape_size,
    gap = gap,
    radius = radius,
    name = name
  )

  # convert terra geoms to dbSpatial
  res <- gpolys[] |>
    dbSpatial::as_dbSpatial(conn = con, overwrite = overwrite, name = name)

  return(res)
}
