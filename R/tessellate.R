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
    .tessellate(
      dbSpatial = dbSpatial,
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
  if (!inherits(con, "DBIConnection") || !DBI::dbIsValid(con)) {
    stop("dbSpatial must be backed by a valid DBI connection.")
  }
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    stop("'name' must be a single non-empty string.")
  }
  if (!inherits(tbl, "tbl")) {
    stop("dbSpatial must expose a lazy table through `[]`.")
  }
  if (!geomName %in% colnames(tbl)) {
    stop("Geometry column '", geomName, "' was not found in dbSpatial.")
  }
  if (!isTRUE(overwrite) && name %in% DBI::dbListTables(con)) {
    stop("Table '", name, "' already exists. Use overwrite = TRUE to replace it.")
  }

  shape <- match.arg(shape)

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
  ext = sf::st_bbox(dbSpatial)

  # retrieve tessellations
  # TODO: compute in db
  gpolys <- GiottoClass::tessellate(
    extent = ext,
    shape = shape,
    shape_size = shape_size
  )

  # convert terra geoms to dbSpatial
  res <- gpolys[] |>
    dbSpatial::as_dbSpatial(conn = con, overwrite = overwrite, name = name)

  return(res)
}
