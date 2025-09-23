#' @import methods
#' @importFrom utils packageVersion

# This file ensures that packages are loaded in the correct order
# to avoid class inheritance issues between dbProject and dbMatrix,dbSpatial

.onLoad <- function(libname, pkgname) {
  # First, ensure all required packages are loaded
  # Load dbverse packages in the right order
  pkgs_to_load <- c("dbProject", "dbMatrix", "dbSpatial", "Giotto")

  # Load each package
  for (pkg in pkgs_to_load) {
    if (!isNamespaceLoaded(pkg)) {
      requireNamespace(pkg, quietly = TRUE)
    }

    # Explicitly attach Giotto to the search path
    if (pkg == "Giotto" && !pkg %in% .packages()) {
      library(pkg, character.only = TRUE)
    }
  }

  # Explicitly ensure dbMatrix is included in the allMatrix class union
  # This fixes the issue where processData doesn't recognize dbMatrix as part of allMatrix
  tryCatch(
    {
      if (isClass("allMatrix") && isClass("dbMatrix")) {
        current_members <- getClassDef("allMatrix")@subclasses
        if (!"dbMatrix" %in% names(current_members)) {
          message("Updating allMatrix class union to include dbMatrix...")
          # Modify setClassUnion to ensure it takes effect
          methods::setClassUnion(
            "allMatrix",
            members = c("matrix", "Matrix", "dbMatrix")
          )
        }
      }
    },
    error = function(e) {
      warning("Could not update allMatrix class union: ", e$message)
    }
  )

  invisible()
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "GiottoDB v",
    utils::packageVersion("GiottoDB")
  )
}
