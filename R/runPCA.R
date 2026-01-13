#' @title Run PCA on GiottoDB with \code{\link[dbMatrix:dbMatrix-class]{dbMatrix::dbMatrix}} support
#' @name runPCA
#' @description
#' S3 generic for PCA that dispatches to \code{dbMatrix::db_svd} when expression
#' data is stored as \code{\link[dbMatrix:dbMatrix-class]{dbMatrix::dbMatrix}}.
#' @param gobject A \code{\link[GiottoClass:giotto-class]{GiottoClass::giotto}} or \code{\link{GiottoDB}} object
#' @param ... Additional arguments passed to underlying PCA methods
#' @export
runPCA <- function(gobject, ...) {
  UseMethod("runPCA")
}

#' @rdname runPCA
#' @export
runPCA.GiottoDB <- function(gobject, ...) {
  runPCA.giotto(gobject, ...)
}

#' @rdname runPCA
#' @param spat_unit spatial unit
#' @param feat_type feature type
#' @param expression_values expression values to use
#' @param name name of PCA dimension reduction
#' @param feats_to_use features to use for PCA
#' @param return_gobject whether to return giotto object
#' @param ncp number of principal components
#' @param center center data before PCA
#' @param scale_unit scale features before PCA
#' @param verbose verbosity
#' @export
runPCA.giotto <- function(
    gobject,
    spat_unit = NULL,
    feat_type = NULL,
    expression_values = "normalized",
    name = "pca",
    feats_to_use = "hvf",
    return_gobject = TRUE,
    ncp = 100,
    center = TRUE,
    scale_unit = FALSE,
    verbose = TRUE,
    ...
) {
  # Set defaults
  spat_unit <- set_default_spat_unit(gobject = gobject, spat_unit = spat_unit)
  feat_type <- set_default_feat_type(gobject = gobject, spat_unit = spat_unit, feat_type = feat_type)

  # Get expression matrix
  expr_obj <- getExpression(
    gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    values = expression_values,
    output = "exprObj"
  )
  expr_mat <- expr_obj[]

  # Subset to features if specified
  if (!is.null(feats_to_use)) {
    if (length(feats_to_use) == 1 && feats_to_use %in% colnames(fDataDT(gobject, spat_unit, feat_type))) {
      # feats_to_use is a column name in feature metadata
      fmeta <- fDataDT(gobject, spat_unit, feat_type)
      hvf_col <- fmeta[[feats_to_use]]
      if (is.character(hvf_col)) {
        feat_ids <- fmeta$feat_ID[hvf_col == "yes"]
      } else if (is.logical(hvf_col)) {
        feat_ids <- fmeta$feat_ID[hvf_col]
      } else {
        feat_ids <- fmeta$feat_ID[as.logical(hvf_col)]
      }
      expr_mat <- expr_mat[feat_ids, ]
    } else if (is.character(feats_to_use) && length(feats_to_use) > 1) {
      # feats_to_use is a character vector of feature IDs
      feats_to_use <- intersect(feats_to_use, rownames(expr_mat))
      expr_mat <- expr_mat[feats_to_use, ]
    }
  }

  if (verbose) {
    message("Running PCA on ", nrow(expr_mat), " features x ", ncol(expr_mat), " cells")
  }

  # Dispatch to db_svd if dbMatrix, otherwise use Giotto's runPCA
  if (inherits(expr_mat, "dbMatrix")) {
    # db_svd expects genes x cells matrix
    # For PCA on cells, we center by rows (genes)
    pca_result <- dbMatrix::db_svd(
      dbm = expr_mat,
      k = min(ncp, nrow(expr_mat) - 1, ncol(expr_mat) - 1),
      center = center,
      scale = scale_unit,
      center_rows = TRUE
    )

    # Extract PCA coordinates (cells x PCs)
    # SVD: X = U * D * V'
    # For PCA on cells: coords = V * D (or just V scaled by singular values)
    pca_coords <- pca_result$v %*% diag(pca_result$d)
    rownames(pca_coords) <- colnames(expr_mat)
    colnames(pca_coords) <- paste0("PC", seq_len(ncol(pca_coords)))

    # Gene loadings
    pca_loadings <- pca_result$u
    rownames(pca_loadings) <- rownames(expr_mat)
    colnames(pca_loadings) <- paste0("PC", seq_len(ncol(pca_loadings)))

    # Eigenvalues (variance explained)
    eigenvalues <- pca_result$d^2 / (ncol(expr_mat) - 1)

    if (return_gobject) {
      # Create dimObj and add to gobject
      pca_obj <- GiottoClass::create_dim_obj(
        name = name,
        spat_unit = spat_unit,
        feat_type = feat_type,
        reduction = "pca",
        reduction_method = "db_svd",
        coordinates = pca_coords,
        misc = list(
          loadings = pca_loadings,
          eigenvalues = eigenvalues
        )
      )

      gobject <- setDimReduction(
        gobject,
        x = pca_obj,
        spat_unit = spat_unit,
        feat_type = feat_type,
        reduction = "cells",
        reduction_method = "pca",
        name = name
      )

      return(gobject)
    } else {
      return(list(
        coords = pca_coords,
        loadings = pca_loadings,
        eigenvalues = eigenvalues
      ))
    }
  } else {
    # Fall back to Giotto's runPCA for non-dbMatrix
    Giotto::runPCA(
      gobject = gobject,
      spat_unit = spat_unit,
      feat_type = feat_type,
      expression_values = expression_values,
      name = name,
      feats_to_use = feats_to_use,
      return_gobject = return_gobject,
      ncp = ncp,
      center = center,
      scale_unit = scale_unit,
      verbose = verbose,
      ...
    )
  }
}
