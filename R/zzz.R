#' @import methods
#' @importFrom utils packageVersion

# This file ensures that packages are loaded in the correct order
# to avoid class inheritance issues between dbProject and dbMatrix,dbSpatial

.onLoad <- function(libname, pkgname) {
  # Load dbverse packages in the right order
  pkgs_to_load <- c("dbProject", "dbMatrix", "dbSpatial", "Giotto")

  for (pkg in pkgs_to_load) {
    if (!isNamespaceLoaded(pkg)) {
      requireNamespace(pkg, quietly = TRUE)
    }
  }

  # Update allMatrix class union to include dbMatrix
  if (isClass("allMatrix") && isClass("dbMatrix") && isClass("Matrix")) {
    current_members <- getClassDef("allMatrix")@subclasses
    if (!"dbMatrix" %in% names(current_members)) {
      methods::setClassUnion(
        "allMatrix",
        members = c("matrix", "Matrix", "dbMatrix")
      )
    }
  }

  # Enable automatic materialization for dbMatrix normalization
  options(giotto.dbmatrix_compute = TRUE)

  invisible()
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "GiottoDB v",
    utils::packageVersion("GiottoDB")
  )
}

