context("Testing as_giottodb function")
library(testthat)

as.list.giotto <- function(gobject) {
  raw_list <- GiottoClass::as.list(gobject)

  result <- list()

  # Process expression objects
  expr_objects <- Filter(function(obj) inherits(obj, "exprObj"), raw_list)
  for (obj in expr_objects) {
    # All exprObj should have these slots
    key <- paste("exprObj", obj@spat_unit, obj@feat_type, obj@name, sep = ".")
    result[[key]] <- obj
  }

  # Process spatial objects using spatUnit() for robust access
  spatial_objects <- Filter(
    function(obj) {
      inherits(obj, "giottoPoints") || inherits(obj, "giottoPolygon")
    },
    raw_list
  )

  for (obj in spatial_objects) {
    class_name <- class(obj)[1]

    # Use spatUnit() to get spatial unit reliably
    spat_unit <- GiottoClass::spatUnit(obj)

    # Get object name
    slot_names <- methods::slotNames(obj)
    obj_name <- if ("name" %in% slot_names) obj@name else "spatInfo"

    key <- paste(class_name, spat_unit, obj_name, sep = ".")
    result[[key]] <- obj
  }

  return(result)
}

test_that("Conversion from giotto to GiottoDB works", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  # Setup: Load a mini Giotto object
  gobject <- GiottoData::loadGiottoMini("visium")
  expect_s4_class(gobject, "giotto")

  # Create temporary file for the database
  temp_db <- tempfile(fileext = ".duckdb")

  # Create DuckDB connection
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  dbSpatial::loadSpatial(con)

  # Test the coercion
  tryCatch(
    {
      # Convert to GiottoDB
      gdb <- as_giottodb(gobject, con = con, verbose = TRUE, overwrite = TRUE)

      # Check that the result is a GiottoDB class
      expect_s4_class(gdb, "GiottoDB")

      # Test that expression objects were converted
      # Get list of expression objects
      obj_list <- as.list.giotto(gobject)
      expr_patterns <- grep("exprObj", names(obj_list), value = TRUE)

      # At least one expression object should be converted
      if (length(expr_patterns) > 0) {
        # Extract first expression object info
        obj_name <- expr_patterns[1]
        name_parts <- strsplit(obj_name, "\\.")[[1]]

        if (length(name_parts) >= 4) {
          spat_unit <- name_parts[2]
          feat_type <- name_parts[3]
          expr_name <- name_parts[4]

          # Check if the expression matrix was converted to dbMatrix
          expr_obj_db <- gdb@expression[[spat_unit]][[feat_type]][[expr_name]]
          expect_true(inherits(
            expr_obj_db@exprMat,
            c("dbDenseMatrix", "dbSparseMatrix")
          ))
        }
      }

      # Test spatial objects were converted
      spatial_patterns <- grep(
        "giottoPoints|giottoPolygon",
        names(obj_list),
        value = TRUE
      )

      if (length(spatial_patterns) > 0) {
        # Extract first spatial object info
        obj_name <- spatial_patterns[1]
        name_parts <- strsplit(obj_name, "\\.")[[1]]

        if (length(name_parts) >= 2) {
          obj_type <- name_parts[1]
          spat_unit <- name_parts[2]
          # spatial_name <- name_parts[3]

          # Check if the spatial object was converted to dbSpatial
          spatial_obj_db <- gdb@spatial_info[[spat_unit]] #[[spatial_name]]
          expect_true(inherits(spatial_obj_db@spatVector, "dbSpatial"))
        }
      }

      # Test methods functionality on the converted object
      # This will depend on what methods are implemented for GiottoDB objects
      # For example:
      # expect_no_error(showGiotto(gdb))
    },
    finally = {
      # Clean up: close connection
      DBI::dbDisconnect(con, shutdown = TRUE)

      # Remove temporary database file if it exists
      if (file.exists(temp_db)) {
        file.remove(temp_db)
      }
    }
  )
})

test_that("as_giottodb fails with informative message for non-Giotto input", {
  # Should throw an error for non-Giotto objects
  expect_error(as_giottodb(list()), "Input must be a giotto object")
})

test_that("as_giottodb validates connection correctly", {
  skip_if_not_installed("GiottoData")
  gobject <- GiottoData::loadGiottoMini("visium")

  # Test with invalid connection
  invalid_con <- list(connection = "not a real connection")
  expect_error(
    as_giottodb(gobject, con = invalid_con),
    "'con' must be a valid DBIConnection object"
  )
})
