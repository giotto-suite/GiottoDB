#' @title GiottoDB Class
#' @name GiottoDB-class
#' @description S4 class that extends the giotto class to provide a database-backed
#' implementation of Giotto objects using [dbMatrix] and [dbSpatial].
#'
#' @slot conn A `DBIConnection` object to a [duckdb::duckdb] database
#'
#' @details The GiottoDB class extends the standard giotto class, replacing in-memory
#' objects with database-backed alternatives where appropriate:
#'
#' * Expression matrices ([matrix], [Matrix]) are replaced with [dbMatrix] objects
#' * Spatial objects (points, polygons) are replaced with [dbSpatial] objects
#'
#' This allows Giotto to scale to larger-than-memory datasets while maintaining
#' API compatibility with existing Giotto workflows.
#'
#' @return A GiottoDB object
#' @importFrom methods setClass setValidity
#' @importClassesFrom GiottoClass giotto
#' @export
#'
setClass(
  "GiottoDB",
  slots = c(
    conn = "ANY" # Changed from DBIConnection to ANY to allow NULL during initialization
  ),
  contains = "giotto"
)

#' @name GiottoDB
#' @rdname GiottoDB-class
#' @importFrom methods validObject
#' @export
setValidity("GiottoDB", function(object) {
  is_valid <- TRUE
  msg <- character()

  # Allow NULL connection during initialization - this will be set later
  if (is.null(object@conn)) {
    is_valid <- FALSE
    msg <- c(
      msg,
      "The 'conn' slot is NULL. A valid DBI connection must be set before using this object."
    )
    return(msg) # Early return with warning but don't fail validation completely
  }

  # Check that the connection slot is properly set
  if (!inherits(object@conn, "DBIConnection")) {
    is_valid <- FALSE
    msg <- c(msg, "The 'conn' slot must be a DBI connection object")
  }

  # Check if connection is still valid
  if (inherits(object@conn, "DBIConnection")) {
    tryCatch(
      {
        res <- DBI::dbIsValid(object@conn)
        if (!res) {
          is_valid <- FALSE
          msg <- c(msg, "The database connection is invalid or closed")
        }
      },
      error = function(e) {
        is_valid <- FALSE
        msg <- c(msg, paste0("Error checking database connection: ", e$message))
      }
    )
  }

  if (is_valid) TRUE else msg
})

#' Create a new GiottoDB object
#'
#' @description Create a new GiottoDB object, which is a database-backed
#' implementation of the Giotto object.
#'
#' @param con A `DBIConnection` object to a [duckdb::duckdb] database
#' @param ... Additional arguments passed to the giotto constructor
#'
#' @return A GiottoDB object
#' @export
#' @examples
#' \dontrun{
#' # Create a new GiottoDB object with a new database connection
#' library(GiottoDB)
#' library(duckdb)
#' library(DBI)
#'
#' # Create a connection to a DuckDB database
#' con <- dbConnect(duckdb(), dbdir = ":memory:")
#' dbSpatial::loadSpatial(con) # Load spatial extension
#'
#' # Create a new GiottoDB object
#' gobj_db <- GiottoDB(con = con)
#'
#' # Don't forget to close the connection when done
#' dbDisconnect(con, shutdown = TRUE)
#' }
GiottoDB <- function(con, ...) {
  # Check that the connection is valid
  if (!inherits(con, "DBIConnection")) {
    stop("The 'con' argument must be a DBI connection object")
  }
  if (!DBI::dbIsValid(con)) {
    stop("The provided database connection is invalid or closed")
  }

  # Load spatial extension if not already loaded
  tryCatch(
    {
      dbSpatial::loadSpatial(con)
    },
    error = function(e) {
      warning("Failed to load spatial extension: ", e$message)
    }
  )

  # Create a new Giotto object with the provided arguments
  gobject <- methods::new("giotto", ...)

  # Create a new GiottoDB object
  gdb <- methods::new("GiottoDB", gobject, conn = con)

  return(gdb)
}
