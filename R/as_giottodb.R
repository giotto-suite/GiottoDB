#' Convert giotto Object to GiottoDB
#'
#' @description
#' A convenience function that coerces a giotto object to a GiottoDB object
#' with a database backend. Expression matrices are converted to dbMatrix objects
#' and spatial data are converted to dbSpatial objects.
#'
#' @param x A giotto object
#' @param con A `DBIConnection` object from a [duckdb::duckdb] connection object
#' @param db_path Path to the database file if creating a new persistent connection
#' @param prefix A string prefix to add to database table names
#' @param overwrite Whether to overwrite existing tables
#' @param verbose Whether to print progress messages
#' @param temporary if TRUE (default), will create a temporary table that is
#' local to this connection and will be automatically deleted when con expires
#'
#' @return A [`GiottoDB`] object
#' @export
#'
#' @examples
#' \dontrun{
#' library(GiottoDB)
#' library(dbSpatial)
#'
#' # Create connection
#' con <- dbConnect(duckdb(), dbdir = ":memory:")
#' loadSpatial(con)
#'
#' # Convert Giotto object
#' my_giotto_db <- as_giottodb(my_giotto, con = con)
#'
#' # Don't forget to close the connection when done
#' dbDisconnect(con, shutdown = TRUE)
#' }
as_giottodb <- function(
  x,
  con = NULL,
  db_path = NULL,
  prefix = "gdb_",
  overwrite = FALSE,
  verbose = TRUE,
  temporary = TRUE
) {
  # Check inputs
  if (!inherits(x, "giotto")) {
    stop("Input must be a giotto object")
  }

  # Handle connection
  if (is.null(con)) {
    if (is.null(db_path)) {
      db_dir <- ":memory:"
      if (verbose) message("Creating in-memory DuckDB connection.")
    } else {
      db_dir <- db_path
      if (verbose) message("Creating persistent DuckDB connection at: ", db_dir)
    }
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_dir)
    on.exit(
      warning(
        "Database connection was created internally.
                    Ensure it's closed appropriately."
      ),
      add = TRUE
    )
  } else {
    if (!inherits(con, "DBIConnection")) {
      stop("'con' must be a valid DBIConnection object.")
    }
    if (verbose) message("Using provided database connection.")
  }

  # Load spatial extension
  tryCatch(
    {
      suppressMessages(dbSpatial::loadSpatial(con))
    },
    error = function(e) {
      stop("Failed to load DuckDB spatial extension: ", e$message)
    }
  )

  # Create a new giotto object that will be transformed
  # This is more efficient than making a full copy of x
  giotto_new <- x

  # Process expression objects
  if (verbose) {
    message("Converting expression matrices to dbMatrix objects...")
  }

  # Process each spatial unit
  for (spat_unit in names(x@expression)) {
    # Process each feature type
    for (feat_type in names(x@expression[[spat_unit]])) {
      # Process each expression object
      for (expr_name in names(x@expression[[spat_unit]][[feat_type]])) {
        expr_obj <- x@expression[[spat_unit]][[feat_type]][[expr_name]]

        if (!methods::.hasSlot(expr_obj, "exprMat")) {
          if (verbose) {
            warning(
              "Expression object (",
              spat_unit,
              "/",
              feat_type,
              "/",
              expr_name,
              ") does not have exprMat slot. Skipping."
            )
          }
          next
        }

        original_matrix <- methods::slot(expr_obj, "exprMat")
        db_table_name <- paste0(
          prefix,
          paste(spat_unit, feat_type, expr_name, sep = '_')
        )

        if (inherits(original_matrix, c("Matrix", "matrix"))) {
          if (verbose) {
            message("  Converting: ", db_table_name, " to dbMatrix")
          }
          tryCatch(
            {
              class <- if (inherits(original_matrix, "sparseMatrix")) {
                "dbSparseMatrix"
              } else {
                "dbDenseMatrix"
              }

              db_matrix <- dbMatrix::dbMatrix(
                value = original_matrix,
                con = con,
                name = db_table_name,
                class = class,
                overwrite = overwrite,
                temporary = temporary
              )

              # Update the new giotto object with the dbMatrix
              methods::slot(
                giotto_new@expression[[spat_unit]][[feat_type]][[expr_name]],
                "exprMat"
              ) <- db_matrix
            },
            error = function(e) {
              warning("  Failed to convert '", db_table_name, "': ", e$message)
            }
          )
        } else if (inherits(original_matrix, "HDF5Matrix")) {
          warning("  Skipping HDF5Matrix '", db_table_name, "'. Not supported.")
        } else if (!is.null(original_matrix)) {
          warning(
            "  Skipping expression matrix '",
            db_table_name,
            "' with unsupported class: ",
            class(original_matrix)[1]
          )
        }
      }
    }
  }

  # Process spatial objects
  if (verbose) {
    message("Converting spatial objects to dbSpatial objects...")
  }

  # Process each spatial unit directly
  for (spat_unit in names(x@spatial_info)) {
    spatial_obj <- x@spatial_info[[spat_unit]]

    if (!methods::.hasSlot(spatial_obj, "spatVector")) {
      if (verbose) {
        warning(
          "Spatial object for spatial unit '",
          spat_unit,
          "' does not have spatVector slot. Skipping."
        )
      }
      next
    }

    original_spatvector <- methods::slot(spatial_obj, "spatVector")

    # Determine prefix based on object type
    obj_type <- class(spatial_obj)[1]
    prefix_type <- if (grepl("Points", obj_type)) "points_" else "poly_"
    db_table_name <- paste0(prefix, prefix_type, spat_unit)

    if (inherits(original_spatvector, "SpatVector")) {
      if (verbose) {
        message("  Converting: ", db_table_name, " to dbSpatial")
      }
      tryCatch(
        {
          db_spatial <- dbSpatial::as_dbSpatial(
            rSpatial = original_spatvector,
            conn = con,
            name = db_table_name,
            overwrite = overwrite
          )

          # If temporary=FALSE, persist the unique spatial table
          if (!temporary && inherits(db_spatial, "dbSpatial")) {
            # Check if db_spatial has a table slot
            table_slot_exists <- "table" %in% methods::slotNames(db_spatial)
            if (table_slot_exists) {
              # db_spatial@table is a dplyr tbl; persist it if needed
              tbl <- methods::slot(db_spatial, "table")
              # Only persist if not already a persistent table
              if (
                !inherits(tbl, "tbl_duckdb") || !isTRUE(attr(tbl, "persistent"))
              ) {
                new_tbl <- dplyr::compute(
                  tbl,
                  temporary = FALSE,
                  name = db_table_name,
                  overwrite = TRUE
                )
                methods::slot(db_spatial, "table") <- new_tbl
              }
            } else if (verbose) {
              message(
                "  Note: dbSpatial object for '",
                db_table_name,
                "' does not have a 'table' slot"
              )
            }
          }

          # Update the new giotto object with the dbSpatial object
          methods::slot(
            giotto_new@spatial_info[[spat_unit]],
            "spatVector"
          ) <- db_spatial
        },
        error = function(e) {
          warning("  Failed to convert '", db_table_name, "': ", e$message)
        }
      )
    } else if (!is.null(original_spatvector)) {
      warning(
        "  Spatial object '",
        db_table_name,
        "' contains data of class '",
        class(original_spatvector)[1],
        "', not 'SpatVector'. Skipping conversion."
      )
    }
  }

  # Process feature point objects (giottoPoints in feat_info)
  for (feat_type in names(x@feat_info)) {
    feat_obj <- x@feat_info[[feat_type]]

    if (!methods::.hasSlot(feat_obj, "spatVector")) {
      if (verbose) {
        warning(
          "Feature object for feature type '",
          feat_type,
          "' does not have spatVector slot. Skipping."
        )
      }
      next
    }

    original_spatvector <- methods::slot(feat_obj, "spatVector")
    db_table_name <- paste0(prefix, "feat_points_", feat_type)

    if (inherits(original_spatvector, "SpatVector")) {
      if (verbose) {
        message("  Converting: ", db_table_name, " to dbSpatial")
      }
      tryCatch(
        {
          db_spatial <- dbSpatial::as_dbSpatial(
            rSpatial = original_spatvector,
            conn = con,
            name = db_table_name,
            overwrite = overwrite
          )

          # If temporary=FALSE, persist the unique spatial table
          if (!temporary && inherits(db_spatial, "dbSpatial")) {
            table_slot_exists <- "table" %in% methods::slotNames(db_spatial)
            if (table_slot_exists) {
              tbl <- methods::slot(db_spatial, "table")
              if (
                !inherits(tbl, "tbl_duckdb") || !isTRUE(attr(tbl, "persistent"))
              ) {
                new_tbl <- dplyr::compute(
                  tbl,
                  temporary = FALSE,
                  name = db_table_name,
                  overwrite = TRUE
                )
                methods::slot(db_spatial, "table") <- new_tbl
              }
            } else if (verbose) {
              message(
                "  Note: dbSpatial object for '",
                db_table_name,
                "' does not have a 'table' slot"
              )
            }
          }

          methods::slot(
            giotto_new@feat_info[[feat_type]],
            "spatVector"
          ) <- db_spatial
        },
        error = function(e) {
          warning("  Failed to convert '", db_table_name, "': ", e$message)
        }
      )
    } else if (
      !is.null(original_spatvector) &&
        !inherits(original_spatvector, "dbSpatial")
    ) {
      warning(
        "  Feature object '",
        db_table_name,
        "' contains data of class '",
        class(original_spatvector)[1],
        "', not 'SpatVector'. Skipping conversion."
      )
    }
  }

  # Create the GiottoDB object from the transformed giotto_new object
  result <- methods::new("GiottoDB", giotto_new, conn = con)

  if (verbose) {
    message("Conversion to GiottoDB complete.")
  }
  return(result)
}
