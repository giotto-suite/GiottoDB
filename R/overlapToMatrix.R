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

    # 1. convert to DT and cleanup
    # not needed because NA values are automatically removed in DB

    # 2. Perform aggregation to counts DT
    if (!is.null(count_info_column)) {
      #TODO
      stop("Not yet implemented for dbSpatial objects")

      # if (!count_info_column %in% colnames(dtoverlap)) {
      #     .gstop("count_info_column ", count_info_column,
      #         " does not exist",
      #         .n = 2L
      #     )
      # }

      # # aggregate counts of features
      # dtoverlap[, c(count_info_column) := as.numeric(
      #     get(count_info_column)
      # )]
      # aggr_dtoverlap <- dtoverlap[, base::sum(get(count_info_column)),
      #     by = c("poly_ID", "feat_ID")
      # ]
      # data.table::setnames(aggr_dtoverlap, "V1", "N")
    } else {
      # if no counts col
      # aggregate individual features
      tmp_name <- paste0('dbMatrix_', paste(sample(LETTERS, 9), collapse = ''))
      aggr_dtoverlap <- dbMatrix:::dbMatrix_from_tbl(
        #FIXME: export this function
        tbl = x[],
        rownames_colName = "feat_ID",
        colnames_colName = "poly_ID",
        name = tmp_name,
        overwrite = TRUE
      )
    }

    # 3. missing IDs repair
    if (!is.null(col_names) && !is.null(row_names)) {
      stop("Not yet implemented for dbSpatial objects")
    } else {
      if (isTRUE(verbose) && output == "MATRIX") {
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
        #TODO: throw warning before casting for possible memory failure
        res <- as.matrix(aggr_dtoverlap, sparse = TRUE, names = TRUE)
        return(res)
      }
    )
  }
)
