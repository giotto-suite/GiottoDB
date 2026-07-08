#' Convert GiottoDB Object to giotto
#'
#' @description
#' A convenience function that coerces a `GiottoDB` object to an in-memory
#' `giotto` object. Expression matrices are converted to in-memory matrices
#' using dbverse coercions:
#'
#' * `dbSparseMatrix`/`dbMatrix` -> `dgCMatrix`
#' * `dbDenseMatrix` -> base `matrix`
#'
#' Spatial data stored as `dbSpatial` are converted to `terra::SpatVector`.
#'
#' @param x A [`GiottoDB`] object
#' @param verbose Whether to print progress messages
#'
#' @return A [`giotto`] object
#' @concept Object creation
#' @export
#'
#' @examples
#' \dontrun{
#' library(GiottoDB)
#' library(GiottoData)
#' library(duckdb)
#' library(DBI)
#'
#' g <- GiottoData::loadGiottoMini("visium")
#' con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
#' gdb <- as_giottodb(g, con = con)
#'
#' g_inmem <- as_giotto(gdb)
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
as_giotto <- function(x, verbose = TRUE) {
  if (!inherits(x, "GiottoDB")) {
    stop("Input must be a GiottoDB object")
  }

  if (
    is.null(x@conn) ||
      !inherits(x@conn, "DBIConnection") ||
      !DBI::dbIsValid(x@conn)
  ) {
    stop(
      "GiottoDB connection is NULL, invalid, or closed. ",
      "Re-load or reconnect the object before converting to giotto."
    )
  }

  giotto_new <- methods::as(x, "giotto")

  mem_limit <- getOption("dbMatrix.max_mem_convert", default = 8 * 1024^3)
  mem_blocked_labels <- character()
  mem_blocked_sizes <- numeric()

  old_dbmatrix_verbose <- getOption("dbMatrix.verbose", default = TRUE)
  options(dbMatrix.verbose = FALSE)
  on.exit(options(dbMatrix.verbose = old_dbmatrix_verbose), add = TRUE)

  if (verbose) {
    message("Converting expression matrices to in-memory...")
  }

  for (spat_unit in names(giotto_new@expression)) {
    for (feat_type in names(giotto_new@expression[[spat_unit]])) {
      for (expr_name in names(giotto_new@expression[[spat_unit]][[
        feat_type
      ]])) {
        expr_obj <- giotto_new@expression[[spat_unit]][[feat_type]][[expr_name]]

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

        expr_mat <- methods::slot(expr_obj, "exprMat")

        if (inherits(expr_mat, "dbDenseMatrix")) {
          if (verbose) {
            message(
              "  Coercing ",
              spat_unit,
              "/",
              feat_type,
              "/",
              expr_name,
              ": dbDenseMatrix -> matrix"
            )
          }
          tryCatch(
            {
              methods::slot(expr_obj, "exprMat") <- methods::as(
                expr_mat,
                "matrix"
              )
              giotto_new@expression[[spat_unit]][[feat_type]][[
                expr_name
              ]] <- expr_obj
            },
            error = function(e) {
              warning(
                "  Failed to coerce dbDenseMatrix to matrix for ",
                spat_unit,
                "/",
                feat_type,
                "/",
                expr_name,
                ": ",
                e$message
              )
            }
          )
        } else if (inherits(expr_mat, c("dbSparseMatrix", "dbMatrix"))) {
          if (verbose) {
            message(
              "  Coercing ",
              spat_unit,
              "/",
              feat_type,
              "/",
              expr_name,
              ": dbMatrix -> dgCMatrix"
            )
          }

          est_size <- {
            dims <- dim(expr_mat)
            as.numeric(dims[1]) * as.numeric(dims[2]) * 8
          }

          tryCatch(
            {
              methods::slot(expr_obj, "exprMat") <- methods::as(
                expr_mat,
                "dgCMatrix"
              )
              giotto_new@expression[[spat_unit]][[feat_type]][[
                expr_name
              ]] <- expr_obj
            },
            error = function(e) {
              emsg <- conditionMessage(e)
              label <- paste0(spat_unit, "/", feat_type, "/", expr_name)

              if (
                grepl(
                  "Implicit conversion to in-memory matrix blocked",
                  emsg,
                  fixed = TRUE
                )
              ) {
                mem_blocked_labels <<- c(mem_blocked_labels, label)
                mem_blocked_sizes <<- c(mem_blocked_sizes, est_size)
                return(invisible(NULL))
              }

              if (grepl("scan\\(\\) expected 'an integer'", emsg)) {
                fallback <- tryCatch(
                  {
                    dat <- dplyr::collect(expr_mat[])

                    if (!all(c("i", "j", "x") %in% colnames(dat))) {
                      stop(
                        "Triplet columns not found in dbMatrix query result. Expected columns: i, j, x"
                      )
                    }

                    i_vals <- suppressWarnings(as.integer(dat$i))
                    j_vals <- suppressWarnings(as.integer(dat$j))
                    bad_i <- !is.na(dat$i) & is.na(i_vals)
                    bad_j <- !is.na(dat$j) & is.na(j_vals)

                    if (any(bad_i) || any(bad_j)) {
                      stop(
                        "Non-integer i/j values encountered while reconstructing sparse matrix from dbMatrix triplets"
                      )
                    }

                    Matrix::sparseMatrix(
                      i = i_vals,
                      j = j_vals,
                      x = as.numeric(dat$x),
                      dims = dim(expr_mat),
                      dimnames = dimnames(expr_mat)
                    )
                  },
                  error = function(e2) e2
                )

                if (!inherits(fallback, "error")) {
                  methods::slot(expr_obj, "exprMat") <- fallback
                  giotto_new@expression[[spat_unit]][[feat_type]][[
                    expr_name
                  ]] <- expr_obj
                  if (verbose) {
                    warning(
                      "  Coercion fallback used for ",
                      label,
                      " by reconstructing sparse matrix from i/j/x triplets."
                    )
                  }
                  return(invisible(NULL))
                }

                warning(
                  "  Failed to coerce dbMatrix to dgCMatrix for ",
                  label,
                  ": ",
                  emsg,
                  "\nFallback via dense conversion also failed: ",
                  conditionMessage(fallback)
                )
                return(invisible(NULL))
              }

              warning(
                "  Failed to coerce dbMatrix to dgCMatrix for ",
                label,
                ": ",
                emsg
              )
            }
          )
        }
      }
    }
  }

  if (length(mem_blocked_labels) > 0) {
    i_max <- which.max(mem_blocked_sizes)[1]
    warning(
      "Failed to coerce ",
      length(mem_blocked_labels),
      " dbMatrix object(s) due to dbMatrix.max_mem_convert (limit: ",
      format(structure(mem_limit, class = "object_size"), units = "auto"),
      "). Largest blocked object: ",
      mem_blocked_labels[i_max],
      " (est. ",
      format(
        structure(mem_blocked_sizes[i_max], class = "object_size"),
        units = "auto"
      ),
      "). Increase 'dbMatrix.max_mem_convert' to override."
    )
  }

  if (verbose) {
    message("Converting spatial objects to in-memory...")
  }

  for (spat_unit in names(giotto_new@spatial_info)) {
    spatial_obj <- giotto_new@spatial_info[[spat_unit]]

    if (methods::.hasSlot(spatial_obj, "spatVector")) {
      spat_vec <- methods::slot(spatial_obj, "spatVector")
      if (inherits(spat_vec, "dbSpatial")) {
        if (verbose) {
          message(
            "  Coercing spatial_info[",
            spat_unit,
            "] spatVector: dbSpatial -> SpatVector"
          )
        }
        tryCatch(
          {
            methods::slot(spatial_obj, "spatVector") <- methods::as(
              spat_vec,
              "SpatVector"
            )
          },
          error = function(e) {
            warning(
              "  Failed to coerce dbSpatial to SpatVector for spatial unit '",
              spat_unit,
              "': ",
              e$message
            )
          }
        )
      }
    }

    if (methods::.hasSlot(spatial_obj, "spatVectorCentroids")) {
      centroids <- methods::slot(spatial_obj, "spatVectorCentroids")
      if (inherits(centroids, "dbSpatial")) {
        if (verbose) {
          message(
            "  Coercing spatial_info[",
            spat_unit,
            "] spatVectorCentroids: dbSpatial -> SpatVector"
          )
        }
        tryCatch(
          {
            methods::slot(spatial_obj, "spatVectorCentroids") <- methods::as(
              centroids,
              "SpatVector"
            )
          },
          error = function(e) {
            warning(
              "  Failed to coerce dbSpatial centroids to SpatVector for spatial unit '",
              spat_unit,
              "': ",
              e$message
            )
          }
        )
      }
    }

    giotto_new@spatial_info[[spat_unit]] <- spatial_obj
  }

  for (feat_type in names(giotto_new@feat_info)) {
    feat_obj <- giotto_new@feat_info[[feat_type]]

    if (!methods::.hasSlot(feat_obj, "spatVector")) {
      next
    }

    feat_spat <- methods::slot(feat_obj, "spatVector")
    if (inherits(feat_spat, "dbSpatial")) {
      if (verbose) {
        message(
          "  Coercing feat_info[",
          feat_type,
          "] spatVector: dbSpatial -> SpatVector"
        )
      }
      tryCatch(
        {
          methods::slot(feat_obj, "spatVector") <- methods::as(
            feat_spat,
            "SpatVector"
          )
        },
        error = function(e) {
          warning(
            "  Failed to coerce dbSpatial to SpatVector for feat_type '",
            feat_type,
            "': ",
            e$message
          )
        }
      )
    }

    giotto_new@feat_info[[feat_type]] <- feat_obj
  }

  if (verbose) {
    message("Conversion to giotto complete.")
  }

  giotto_new
}
