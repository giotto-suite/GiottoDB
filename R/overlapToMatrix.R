# points
#' @inheritParams GiottoClass::overlapToMatrix
#' @export
setMethod(
  "overlapToMatrix",
  signature("dbSpatial"),
  function(
    x,
    col_names = NULL,
    row_names = NULL,
    feat_count_column = NULL,
    count_info_column = NULL,
    output = c("dbSparseMatrix", "Matrix", "data.table"),
    verbose = TRUE,
    ...
  ) {
    output <- match.arg(
      toupper(output),
      choices = c("DBSPARSEMATRIX", "MATRIX", "DATA.TABLE")
    )

    # NSE vars
    poly_ID <- NULL

    # Backwards compatibility with older argument name
    if (is.null(feat_count_column) && !is.null(count_info_column)) {
      feat_count_column <- count_info_column
    }

    # 1. convert to DT and cleanup
    # not needed because NA values are automatically removed in DB

    # 2. Perform aggregation
    tmp_name <- paste0("dbMatrix_", paste(sample(LETTERS, 9), collapse = ""))
    if (!is.null(feat_count_column) && !isFALSE(feat_count_column)) {
      if (!feat_count_column %in% colnames(x[])) {
        stop(
          "feat_count_column '",
          feat_count_column,
          "' not found in overlap table",
          call. = FALSE
        )
      }

      # Coerce to numeric to ensure SUM() works on DB backends
      tbl_weighted <- x[] |>
        dplyr::mutate(.db_count = as.numeric(.data[[feat_count_column]]))

      aggr_dtoverlap <- dbMatrix::dbMatrix_from_tbl(
        tbl = tbl_weighted,
        rownames_colName = "feat_ID",
        colnames_colName = "poly_ID",
        value_colName = ".db_count",
        name = tmp_name,
        overwrite = TRUE
      )
    } else {
      # Unweighted: count occurrences per (feat_ID, poly_ID)
      aggr_dtoverlap <- dbMatrix::dbMatrix_from_tbl(
        tbl = x[],
        rownames_colName = "feat_ID",
        colnames_colName = "poly_ID",
        name = tmp_name,
        overwrite = TRUE
      )
    }

    # 3. missing IDs repair
    # For dbSpatial we can only reliably repair missing rows/cols after
    # casting to an in-memory sparse Matrix.
    if (!is.null(col_names) && !is.null(row_names) && output != "MATRIX") {
      stop(
        "col_names/row_names repair is only implemented for output='Matrix'",
        call. = FALSE
      )
    }
    if (
      (is.null(col_names) || is.null(row_names)) &&
        isTRUE(verbose) &&
        output == "MATRIX"
    ) {
      warning(
        GiottoUtils::wrap_txt(
          "[overlapToMatrix] expected col_names and row_names
                    not provided together. Points aggregation Matrix output
                    may be missing some cols and rows where no detections
                    were found."
        ),
        call. = FALSE
      )
    }

    # 4. return
    switch(
      output,
      "DBSPARSEMATRIX" = {
        return(aggr_dtoverlap)
      },
      "DATA.TABLE" = {
        stop("Not yet implemented for dbSpatial objects")
      },
      "MATRIX" = {
        # dbMatrix intentionally errors on 1x1 objects for as.matrix();
        # downstream expects a matrix-like object, so special-case scalars.
        if (all(dim(aggr_dtoverlap) == c(1L, 1L))) {
          # Collect scalar value from ijx representation
          ijx <- aggr_dtoverlap[] |>
            dplyr::collect()
          val <- if (nrow(ijx) == 0) 0 else ijx$x[[1]]
          res <- Matrix::Matrix(val, nrow = 1L, ncol = 1L, sparse = TRUE)
          dnames <- dimnames(aggr_dtoverlap)
          if (!is.null(dnames)) {
            dimnames(res) <- dnames
          }
          res <- as(as(res, "generalMatrix"), "CsparseMatrix")
        } else {
          res <- as.matrix(aggr_dtoverlap, sparse = TRUE, names = TRUE)
        }

        if (!is.null(col_names) && !is.null(row_names)) {
          full <- Matrix::Matrix(
            0,
            nrow = length(row_names),
            ncol = length(col_names),
            sparse = TRUE,
            dimnames = list(row_names, col_names)
          )
          full[rownames(res), colnames(res)] <- res
          res <- full
        }
        return(res)
      }
    )
  }
)
