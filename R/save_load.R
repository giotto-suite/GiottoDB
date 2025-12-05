#' @title Save Giotto object
#' @name saveGiotto
#' @description Generic function to save Giotto or GiottoDB objects
#' @param gobject giotto or GiottoDB object
#' @param ... additional parameters passed to methods
#' @export
saveGiotto <- function(gobject, ...) {
  UseMethod("saveGiotto")
}

#' @describeIn saveGiotto Save a GiottoDB object
#' @param foldername Folder name
#' @param dir Directory where to create the folder
#' @param method method to save main object
#' @param method_params additional method parameters for RDS or qs
#' @param overwrite Overwrite existing folders
#' @param export_image logical. Write out an image when saving giottoLargeImage
#' @param image_filetype the image filetype to use. Default is "PNG"
#' @param include_feat_coord logical. Whether to keep feature coordinates
#' @param verbose be verbose
#' @returns Creates a directory with GiottoDB object information including database files
#' @details This method extends GiottoClass::saveGiotto to handle
#' database-backed objects. The database file (.db) is moved to the save
#' directory, and connection information is updated to point to the new location.
#' Note: The original database file will no longer exist at its original location after saving.
#' @export
saveGiotto.GiottoDB <- function(
  gobject,
  foldername = "saveGiottoDir",
  dir = getwd(),
  method = c("RDS", "qs"),
  method_params = list(),
  overwrite = FALSE,
  export_image = TRUE,
  image_filetype = "PNG",
  include_feat_coord = TRUE,
  verbose = TRUE,
  ...
) {
  # Validate input
  checkmate::assert_class(gobject, "GiottoDB")

  # Set directory path and folder
  dir <- normalizePath(dir)
  final_dir <- file.path(dir, foldername)

  if (dir.exists(final_dir) && !overwrite) {
    stop(wrap_txt(
      "Folder already exists and overwrite = FALSE. Abort saving."
    ))
  }

  if (dir.exists(final_dir) && overwrite) {
    if (verbose) {
      message("Folder already exists and overwrite = TRUE, overwriting...")
    }
  }

  # Create directory structure
  # dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  # We let GiottoClass::saveGiotto create the directory to avoid "Folder exists" error

  ## Store database path and check if already in target location
  db_con <- gobject@conn
  db_path_to_move <- NULL
  db_already_in_place <- FALSE

  if (!is.null(db_con) && DBI::dbIsValid(db_con)) {
    # Safely get database path using DBI method
    db_info <- DBI::dbGetInfo(db_con)
    db_path <- db_info$dbname

    if (db_path != ":memory:") {
      # Check if database is already in the target location
      db_dir <- file.path(final_dir, "Database")
      db_filename <- basename(db_path)
      new_db_path <- file.path(db_dir, db_filename)

      db_path_normalized <- normalizePath(db_path, mustWork = FALSE)
      new_db_path_normalized <- normalizePath(new_db_path, mustWork = FALSE)

      if (db_path_normalized == new_db_path_normalized) {
        # Database is already in the correct location
        db_already_in_place <- TRUE
        if (verbose) message("Database already in target location")
      } else {
        db_path_to_move <- db_path
      }
    } else {
      warning(
        "GiottoDB uses an in-memory database. ",
        "Cannot save database file. ",
        "Consider using a file-based database for persistent storage.",
        call. = FALSE
      )
    }
  }

  ## Persist dbSpatial tables and track metadata for reconnection
  # Temporary tables would be lost when connection closes, so we need to
  # materialize them as permanent tables in the database
  if (verbose) {
    message("Preparing spatial objects for save...")
  }

  spatial_backup <- list()
  feature_backup <- list()
  dbspatial_metadata <- list() # Store metadata for robust reconnection

  # Handle spatial_info - persist dbSpatial tables and then set to NULL
  for (spat_unit in names(gobject@spatial_info)) {
    if (verbose) {
      message(sprintf("Processing spatial_info: %s", spat_unit))
    }

    # Safely get spatVector
    spat_vec <- tryCatch(
      {
        gobject@spatial_info[[spat_unit]]@spatVector
      },
      error = function(e) {
        if (verbose) {
          message(sprintf(
            "  Warning: Could not access spatVector for %s: %s",
            spat_unit,
            e$message
          ))
        }
        NULL
      }
    )

    if (inherits(spat_vec, "dbSpatial")) {
      # Generate unique table name for this dbSpatial object
      table_name <- paste0("giottodb_spatial_", spat_unit)

      # Use dplyr::compute() to persist table
      # This handles materialization efficiently
      if (verbose) {
        message(sprintf("  Persisting table %s...", table_name))
      }

      # Update the dbSpatial object with the persisted table
      underlying_table <- spat_vec[]
      persisted_table <- dplyr::compute(
        underlying_table,
        name = table_name,
        temporary = FALSE,
        overwrite = TRUE
      )

      # Store metadata for reconnection
      dbspatial_metadata$spatial_info[[spat_unit]] <- list(
        table_name = table_name,
        slot_path = c("spatial_info", spat_unit)
      )

      # Backup the entire spatial object
      spatial_backup[[spat_unit]] <- gobject@spatial_info[[spat_unit]]

      # Convert dbSpatial to regular terra::SpatVector for serialization
      # This avoids corrupted C++ pointers in the saved RDS
      # NOTE: For large data, this might be heavy.
      # Ideally we would set it to NULL or a placeholder, but GiottoClass might expect a SpatVector.
      # For now, we materialize. If it's too large, we might need a dummy SpatVector.
      safe_obj <- gobject@spatial_info[[spat_unit]]
      safe_obj@spatVector <- dbSpatial::vect(spat_vec)
      gobject@spatial_info[[spat_unit]] <- safe_obj
    }
  }

  # Handle feat_info - persist dbSpatial tables and then set to NULL
  for (feat_type in names(gobject@feat_info)) {
    spat_vec <- gobject@feat_info[[feat_type]]@spatVector
    if (inherits(spat_vec, "dbSpatial")) {
      # Generate unique table name for this dbSpatial object
      table_name <- paste0("giottodb_feature_", feat_type)

      if (verbose) {
        message(sprintf("  Persisting table %s...", table_name))
      }

      underlying_table <- spat_vec[]
      persisted_table <- dplyr::compute(
        underlying_table,
        name = table_name,
        temporary = FALSE,
        overwrite = TRUE
      )

      # Store metadata for reconnection
      dbspatial_metadata$feat_info[[feat_type]] <- list(
        table_name = table_name,
        slot_path = c("feat_info", feat_type)
      )

      # Backup the entire feature object
      feature_backup[[feat_type]] <- gobject@feat_info[[feat_type]]

      # Convert dbSpatial to regular terra::SpatVector for serialization
      safe_obj <- gobject@feat_info[[feat_type]]
      safe_obj@spatVector <- dbSpatial::vect(spat_vec)
      gobject@feat_info[[feat_type]] <- safe_obj
    }
  }

  ## Handle expression slot - persist dbMatrix tables
  expression_backup <- list()
  dbmatrix_metadata <- list()

  for (spat_unit in names(gobject@expression)) {
    for (feat_type in names(gobject@expression[[spat_unit]])) {
      for (expr_name in names(gobject@expression[[spat_unit]][[feat_type]])) {
        expr_obj <- gobject@expression[[spat_unit]][[feat_type]][[expr_name]]

        # Check if the expression matrix is a dbMatrix
        if (inherits(expr_obj[], "dbMatrix")) {
          if (verbose) {
            message(sprintf(
              "  Processing expression: [%s][%s][%s]",
              spat_unit,
              feat_type,
              expr_name
            ))
          }

          db_mat <- expr_obj[]
          table_name <- paste0(
            "giottodb_expr_",
            spat_unit,
            "_",
            feat_type,
            "_",
            expr_name
          )

          if (verbose) {
            message(sprintf("    Persisting view/table %s...", table_name))
          }

          # Use dplyr::compute() to persist table
          # IMPORTANT: Pass the dbMatrix OBJECT (db_mat), not the table (db_mat[]).
          # This ensures dispatch to compute.dbMatrix, which saves dimension names.

          # Materialize the table and save dimnames
          new_db_mat <- dplyr::compute(
            db_mat,
            name = table_name,
            temporary = FALSE,
            overwrite = TRUE
          )

          # Store metadata for reconnection using dbLoad
          if (is.null(dbmatrix_metadata[[spat_unit]])) {
            dbmatrix_metadata[[spat_unit]] <- list()
          }
          if (is.null(dbmatrix_metadata[[spat_unit]][[feat_type]])) {
            dbmatrix_metadata[[spat_unit]][[feat_type]] <- list()
          }
          dbmatrix_metadata[[spat_unit]][[feat_type]][[expr_name]] <- list(
            table_name = table_name,
            class = class(new_db_mat)[1], # "dbSparseMatrix" or "dbDenseMatrix"
            slot_path = c("expression", spat_unit, feat_type, expr_name)
          )

          # Backup the original exprObj
          backup_key <- paste(spat_unit, feat_type, expr_name, sep = ".")
          expression_backup[[backup_key]] <- expr_obj

          # Convert dbMatrix to regular Matrix for serialization
          # This uses dbMatrix's as.matrix/as.Matrix method
          regular_mat <- methods::as(new_db_mat, "Matrix")
          expr_obj[] <- regular_mat
          gobject@expression[[spat_unit]][[feat_type]][[expr_name]] <- expr_obj
        }
      }
    }
  }

  ## Call parent saveGiotto for standard Giotto object saving
  if (verbose) {
    message("Saving Giotto object structure...")
  }

  # Cast to parent class temporarily to avoid recursion
  # Keep only giotto class, removing GiottoDB
  original_class <- class(gobject)
  class(gobject) <- "giotto"

  GiottoClass::saveGiotto(
    gobject = gobject,
    foldername = basename(final_dir), # just the folder name
    dir = dirname(final_dir), # parent directory
    method = method,
    method_params = method_params,
    overwrite = overwrite,
    export_image = export_image,
    image_filetype = image_filetype,
    include_feat_coord = include_feat_coord,
    verbose = verbose,
    ...
  )

  # Restore dbSpatial-backed objects after save
  for (spat_unit in names(spatial_backup)) {
    gobject@spatial_info[[spat_unit]] <- spatial_backup[[spat_unit]]
  }
  for (feat_type in names(feature_backup)) {
    gobject@feat_info[[feat_type]] <- feature_backup[[feat_type]]
  }

  # Restore dbMatrix-backed expression objects after save
  for (backup_key in names(expression_backup)) {
    path_parts <- strsplit(backup_key, "\\.")[[1]]
    spat_unit <- path_parts[1]
    feat_type <- path_parts[2]
    expr_name <- path_parts[3]
    gobject@expression[[spat_unit]][[feat_type]][[
      expr_name
    ]] <- expression_backup[[backup_key]]
  }

  # Restore original class
  class(gobject) <- original_class

  ## Now handle database file after parent saveGiotto is done
  if (!is.null(db_path_to_move) || db_already_in_place) {
    if (verbose) {
      message("Saving database-backed objects...")
    }

    # Create database subdirectory
    db_dir <- file.path(final_dir, "Database")
    dir.create(db_dir, showWarnings = FALSE)

    # Save metadata to RDS files for robust reconnection
    dbspatial_metadata_file <- file.path(db_dir, "dbspatial_metadata.rds")
    saveRDS(dbspatial_metadata, file = dbspatial_metadata_file)

    dbmatrix_metadata_file <- file.path(db_dir, "dbmatrix_metadata.rds")
    saveRDS(dbmatrix_metadata, file = dbmatrix_metadata_file)

    if (verbose && length(dbspatial_metadata) > 0) {
      message(sprintf(
        "  Saved metadata for %d dbSpatial objects",
        length(unlist(dbspatial_metadata, recursive = FALSE))
      ))
    }

    if (!is.null(db_path_to_move)) {
      # Need to move database to new location
      db_filename <- basename(db_path_to_move)
      new_db_path <- file.path(db_dir, db_filename)

      if (verbose) {
        message(sprintf(
          "  Moving database from %s to %s",
          db_path_to_move,
          new_db_path
        ))
      }

      # Properly disconnect with shutdown to ensure all data is flushed to disk
      DBI::dbDisconnect(gobject@conn, shutdown = TRUE)

      # Move database file (not copy) to avoid leaving orphaned database
      file.rename(from = db_path_to_move, to = new_db_path)

      # Reconnect to new database location
      new_con <- DBI::dbConnect(
        duckdb::duckdb(),
        dbdir = new_db_path,
        read_only = FALSE
      )

      # Load spatial extension
      dbSpatial::loadSpatial(new_con)

      # Update connection in gobject
      gobject@conn <- new_con
    } else {
      # Database is already in the correct location, use existing connection
      new_con <- gobject@conn
    }

    # Reconnect dbSpatial objects to the (potentially new) connection
    # This is critical for enabling multiple saves of the same object
    if (length(dbspatial_metadata) > 0) {
      if (verbose) {
        message("  Reconnecting dbSpatial objects to new database location...")
      }

      # Reconnect spatial_info dbSpatial objects
      if (!is.null(dbspatial_metadata$spatial_info)) {
        for (spat_unit in names(dbspatial_metadata$spatial_info)) {
          metadata <- dbspatial_metadata$spatial_info[[spat_unit]]
          table_name <- metadata$table_name

          # Reconnect to the persisted table with new connection
          tbl_ref <- dplyr::tbl(new_con, table_name)
          gobject@spatial_info[[spat_unit]]@spatVector <- dbSpatial::dbSpatial(
            value = tbl_ref,
            name = table_name
          )
        }
      }

      # Reconnect feat_info dbSpatial objects
      if (!is.null(dbspatial_metadata$feat_info)) {
        for (feat_type in names(dbspatial_metadata$feat_info)) {
          metadata <- dbspatial_metadata$feat_info[[feat_type]]
          table_name <- metadata$table_name

          # Reconnect to the persisted table with new connection
          tbl_ref <- dplyr::tbl(new_con, table_name)
          gobject@feat_info[[feat_type]]@spatVector <- dbSpatial::dbSpatial(
            value = tbl_ref,
            name = table_name
          )
        }
      }
    }

    if (verbose) {
      message("  Database successfully saved and reconnected")
    }
  }

  if (verbose) {
    message(sprintf(
      "\nGiottoDB object successfully saved to: %s",
      final_dir
    ))
  }

  # Return the gobject with updated connection
  invisible(gobject)
}

#' @describeIn saveGiotto Default method - delegates to GiottoClass
#' @export
saveGiotto.default <- function(
  gobject,
  foldername = "saveGiottoDir",
  dir = getwd(),
  method = c("RDS", "qs"),
  method_params = list(),
  overwrite = FALSE,
  export_image = TRUE,
  image_filetype = "PNG",
  include_feat_coord = TRUE,
  verbose = TRUE,
  ...
) {
  GiottoClass::saveGiotto(
    gobject = gobject,
    foldername = foldername,
    dir = dir,
    method = method,
    method_params = method_params,
    overwrite = overwrite,
    export_image = export_image,
    image_filetype = image_filetype,
    include_feat_coord = include_feat_coord,
    verbose = verbose,
    ...
  )
}

#' @title Load Giotto object
#' @name loadGiotto
#' @description Function to load Giotto or GiottoDB objects
#' @param path_to_folder path to folder where object was stored
#' @param ... additional parameters passed to methods
#' @export
loadGiotto <- function(path_to_folder, ...) {
  # Try to determine if it's a GiottoDB by checking for Database directory
  db_dir <- file.path(path_to_folder, "Database")

  if (dir.exists(db_dir)) {
    # Likely a GiottoDB, use GiottoDB method
    .loadGiotto.GiottoDB(path_to_folder, ...)
  } else {
    # Standard Giotto, use default method
    .loadGiotto.default(path_to_folder, ...)
  }
}

#' @title Load GiottoDB object with explicit connection
#' @description Helper function to load a Giotto object and convert it to GiottoDB
#' using an existing connection.
#' @param path_to_folder path to folder where object was stored
#' @param con DBI connection to use
#' @param ... additional parameters passed to loadGiotto
#' @export
loadGiottoDB <- function(path_to_folder, con, ...) {
  gobject <- GiottoClass::loadGiotto(path_to_folder, ...)
  as_giottodb(gobject, con = con)
}

# Internal function for loading GiottoDB
.loadGiotto.GiottoDB <- function(
  path_to_folder,
  load_params = list(),
  reconnect_giottoImage = TRUE,
  python_path = NULL,
  init_gobject = TRUE,
  verbose = TRUE
) {
  path_to_folder <- path.expand(path_to_folder)

  if (!file.exists(path_to_folder)) {
    stop("path_to_folder does not exist\n")
  }

  ## 1. Load standard Giotto object
  if (verbose) {
    message("Loading GiottoDB object...")
  }

  gobject <- GiottoClass::loadGiotto(
    path_to_folder = path_to_folder,
    load_params = load_params,
    reconnect_giottoImage = reconnect_giottoImage,
    python_path = python_path,
    init_gobject = FALSE, # we'll initialize after reconnecting db
    verbose = verbose
  )

  ## 2. Reconnect database
  if (verbose) {
    message("Reconnecting to database...")
  }

  db_dir <- file.path(path_to_folder, "Database")

  if (dir.exists(db_dir)) {
    # Find .db or .duckdb file(s)
    db_files <- list.files(
      db_dir,
      pattern = "\\.(db|duckdb)$",
      full.names = TRUE
    )

    if (length(db_files) == 0) {
      warning(
        "Database directory exists but no .db file found. ",
        "Database connection will not be restored.",
        call. = FALSE
      )
    } else if (length(db_files) > 1) {
      warning(
        "Multiple .db files found. Using the first one: ",
        basename(db_files[1]),
        call. = FALSE
      )
      db_path <- db_files[1]
    } else {
      db_path <- db_files[1]
    }

    if (exists("db_path")) {
      if (verbose) {
        message(sprintf(
          "  Connecting to database: %s",
          basename(db_path)
        ))
      }

      # Create new connection
      new_con <- DBI::dbConnect(
        duckdb::duckdb(),
        dbdir = db_path,
        read_only = FALSE
      )

      # Load spatial extension
      dbSpatial::loadSpatial(new_con)

      # Convert to GiottoDB class with connection
      if (!"GiottoDB" %in% class(gobject)) {
        # Copy all slots from giotto to new GiottoDB object with connection
        gdb <- new("GiottoDB", conn = new_con)
        # Copy giotto slots
        for (slot_name in slotNames("giotto")) {
          slot(gdb, slot_name) <- slot(gobject, slot_name)
        }
        gobject <- gdb
      } else {
        gobject@conn <- new_con
      }

      # Reconnect dbSpatial objects from database using metadata
      if (verbose) {
        message("Reconnecting spatial objects from database...")
      }

      # Load metadata file for robust reconnection
      metadata_file <- file.path(db_dir, "dbspatial_metadata.rds")

      if (file.exists(metadata_file)) {
        dbspatial_metadata <- readRDS(metadata_file)

        # Reconnect spatial_info dbSpatial objects
        if (!is.null(dbspatial_metadata$spatial_info)) {
          for (spat_unit in names(dbspatial_metadata$spatial_info)) {
            metadata <- dbspatial_metadata$spatial_info[[spat_unit]]
            table_name <- metadata$table_name

            # Verify table exists in database
            if (DBI::dbExistsTable(new_con, table_name)) {
              # Create fresh dbSpatial object from table
              tbl_ref <- dplyr::tbl(new_con, table_name)
              new_dbspatial <- dbSpatial::dbSpatial(
                value = tbl_ref,
                name = table_name
              )

              # Replace the materialized SpatVector with dbSpatial
              gobject@spatial_info[[spat_unit]]@spatVector <- new_dbspatial

              if (verbose) {
                message(sprintf("  Reconnected spatial_info: %s", spat_unit))
              }
            } else {
              warning(
                sprintf(
                  "Table '%s' not found in database for spatial_info: %s",
                  table_name,
                  spat_unit
                ),
                call. = FALSE
              )
            }
          }
        }

        # Reconnect feat_info dbSpatial objects
        if (!is.null(dbspatial_metadata$feat_info)) {
          for (feat_type in names(dbspatial_metadata$feat_info)) {
            metadata <- dbspatial_metadata$feat_info[[feat_type]]
            table_name <- metadata$table_name

            # Verify table exists in database
            if (DBI::dbExistsTable(new_con, table_name)) {
              # Create fresh dbSpatial object from table
              tbl_ref <- dplyr::tbl(new_con, table_name)
              new_dbspatial <- dbSpatial::dbSpatial(
                value = tbl_ref,
                name = table_name
              )

              # Replace the materialized SpatVector with dbSpatial
              gobject@feat_info[[feat_type]]@spatVector <- new_dbspatial

              if (verbose) {
                message(sprintf("  Reconnected feat_info: %s", feat_type))
              }
            } else {
              warning(
                sprintf(
                  "Table '%s' not found in database for feat_info: %s",
                  table_name,
                  feat_type
                ),
                call. = FALSE
              )
            }
          }
        }
      } else {
        warning(
          "dbSpatial metadata file not found. ",
          "dbSpatial objects will not be reconnected.",
          call. = FALSE
        )
      }

      # Reconnect dbMatrix objects from database using metadata
      if (verbose) {
        message("Reconnecting expression matrices from database...")
      }

      # Load metadata file for dbMatrix reconnection
      dbmatrix_metadata_file <- file.path(db_dir, "dbmatrix_metadata.rds")

      if (file.exists(dbmatrix_metadata_file)) {
        dbmatrix_metadata <- readRDS(dbmatrix_metadata_file)

        # Iterate through expression structure to reconnect dbMatrix objects
        for (spat_unit in names(dbmatrix_metadata)) {
          for (feat_type in names(dbmatrix_metadata[[spat_unit]])) {
            for (expr_name in names(dbmatrix_metadata[[spat_unit]][[
              feat_type
            ]])) {
              metadata <- dbmatrix_metadata[[spat_unit]][[feat_type]][[
                expr_name
              ]]
              table_name <- metadata$table_name
              mat_class <- metadata$class

              # Verify table exists in database
              if (DBI::dbExistsTable(new_con, table_name)) {
                # Use dbLoad to reconstruct full dbMatrix with dimension names
                new_dbmatrix <- dbMatrix::dbLoad(
                  conn = new_con,
                  name = table_name,
                  class = mat_class
                )

                # Get the current exprObj and replace its matrix with dbMatrix
                expr_obj <- gobject@expression[[spat_unit]][[feat_type]][[
                  expr_name
                ]]
                expr_obj[] <- new_dbmatrix
                gobject@expression[[spat_unit]][[feat_type]][[
                  expr_name
                ]] <- expr_obj

                if (verbose) {
                  message(sprintf(
                    "  Reconnected expression: [%s][%s][%s]",
                    spat_unit,
                    feat_type,
                    expr_name
                  ))
                }
              } else {
                warning(
                  sprintf(
                    "Table '%s' not found in database for expression: [%s][%s][%s]",
                    table_name,
                    spat_unit,
                    feat_type,
                    expr_name
                  ),
                  call. = FALSE
                )
              }
            }
          }
        }
      } else {
        if (verbose) {
          message(
            "  No dbMatrix metadata found, skipping expression reconnection"
          )
        }
      }

      if (verbose) {
        message("  Database successfully reconnected")
      }
    }
  } else {
    warning(
      "No Database directory found. ",
      "This may not be a saved GiottoDB object, or database was not saved.",
      call. = FALSE
    )
  }

  ## 3. Initialize if requested
  if (init_gobject) {
    gobject <- methods::initialize(gobject)
  }

  if (verbose) {
    message("\nGiottoDB object successfully loaded from: ", path_to_folder)
  }

  return(gobject)
}

# Internal function for loading standard Giotto
.loadGiotto.default <- function(
  path_to_folder,
  load_params = list(),
  reconnect_giottoImage = TRUE,
  python_path = NULL,
  init_gobject = TRUE,
  verbose = TRUE
) {
  GiottoClass::loadGiotto(
    path_to_folder = path_to_folder,
    load_params = load_params,
    reconnect_giottoImage = reconnect_giottoImage,
    python_path = python_path,
    init_gobject = init_gobject,
    verbose = verbose
  )
}
