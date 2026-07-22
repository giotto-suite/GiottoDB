# Tests for runPCA S3 generic

test_that("runPCA.GiottoDB uses db_svd for dbSparseMatrix expression", {
  skip_if_not_installed("GiottoClass")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  temp_dir <- tempfile("giottodb_pca_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(temp_dir, "test.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create sparse count expression matrix
  set.seed(42)
  expr <- Matrix::rsparsematrix(100, 50, density = 0.2, rand.x = function(n) {
    rpois(n, 5) + 1
  })
  rownames(expr) <- paste0("gene_", 1:100)
  colnames(expr) <- paste0("cell_", 1:50)

  # Create GiottoDB with dbSparseMatrix
  g <- GiottoClass::createGiottoObject(expression = expr)
  gdb <- as_giottodb(g, con = con)

  # Verify raw is dbSparseMatrix
  raw_mat <- Giotto::getExpression(gdb, values = "raw", output = "matrix")
  expect_s4_class(raw_mat, "dbSparseMatrix")

  # Run PCA on raw with explicit spat_unit/feat_type
  gdb <- runPCA(
    gdb,
    spat_unit = "cell",
    feat_type = "rna",
    expression_values = "raw",
    ncp = 10,
    feats_to_use = NULL,
    verbose = FALSE
  )

  # Verify PCA was created
  pca <- Giotto::getDimReduction(gdb, reduction = "cells", name = "pca")
  expect_true(!is.null(pca))
  expect_equal(ncol(pca@coordinates), 10)
  expect_equal(nrow(pca@coordinates), 50)
})

test_that("runPCA generic dispatches correctly", {
  skip_if_not_installed("GiottoClass")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  temp_dir <- tempfile("giottodb_dispatch_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(temp_dir, "test.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  set.seed(42)
  expr <- Matrix::rsparsematrix(100, 50, density = 0.2, rand.x = function(n) {
    rpois(n, 5) + 1
  })
  rownames(expr) <- paste0("gene_", 1:100)
  colnames(expr) <- paste0("cell_", 1:50)

  g <- GiottoClass::createGiottoObject(expression = expr)
  gdb <- as_giottodb(g, con = con)

  # Check S3 dispatch
  expect_true(inherits(gdb, "GiottoDB"))
  expect_true("runPCA.GiottoDB" %in% methods("runPCA"))
})

test_that("db_svd works on dbSparseMatrix", {
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Create dbSparseMatrix directly
  set.seed(42)
  mat <- Matrix::rsparsematrix(50, 20, density = 0.3, rand.x = function(n) {
    rpois(n, 5) + 1
  })
  rownames(mat) <- paste0("gene_", 1:50)
  colnames(mat) <- paste0("cell_", 1:20)

  dbm <- dbMatrix::as.dbMatrix(mat, con = con)
  expect_s4_class(dbm, "dbSparseMatrix")

  # Run db_svd
  result <- dbMatrix::db_svd(dbm, k = 5, center = TRUE, center_rows = TRUE)

  expect_true("u" %in% names(result))
  expect_true("v" %in% names(result))
  expect_true("d" %in% names(result))
  expect_equal(ncol(result$v), 5)
})

test_that("runPCA produces equivalent results for giotto and GiottoDB", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("GiottoClass")
  skip_if_not_installed("Giotto")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  temp_dir <- tempfile("giottodb_equiv_")
  dir.create(temp_dir, recursive = TRUE)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(temp_dir, "test.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Load mini visium
  g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)

  # Convert to GiottoDB
  gdb <- as_giottodb(g, con = con)

  # Run PCA on both with same parameters
  g <- Giotto::runPCA(
    g,
    ncp = 10,
    feats_to_use = NULL,
    center = TRUE,
    scale_unit = FALSE,
    verbose = FALSE
  )
  gdb <- runPCA(
    gdb,
    ncp = 10,
    feats_to_use = NULL,
    center = TRUE,
    scale_unit = FALSE,
    verbose = FALSE
  )

  # Get PCA results
  pca_g <- Giotto::getDimReduction(g, reduction = "cells", name = "pca")
  pca_gdb <- Giotto::getDimReduction(gdb, reduction = "cells", name = "pca")

  # Both should exist
  expect_true(!is.null(pca_g))
  expect_true(!is.null(pca_gdb))

  # Same dimensions
  expect_equal(dim(pca_g@coordinates), dim(pca_gdb@coordinates))

  # Correlations should be high (handles sign flips)
  common_cells <- intersect(
    rownames(pca_g@coordinates),
    rownames(pca_gdb@coordinates)
  )
  coords_g <- pca_g@coordinates[common_cells, ]
  coords_gdb <- pca_gdb@coordinates[common_cells, ]

  cors <- sapply(seq_len(ncol(coords_g)), function(i) {
    abs(cor(coords_g[, i], coords_gdb[, i]))
  })

  # All PCs should have correlation > 0.99
  expect_true(
    all(cors > 0.99),
    info = paste("PC correlations:", paste(round(cors, 4), collapse = ", "))
  )
})

test_that("runPCA direct PCA output handles ncp=1", {
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  set.seed(42)
  mat <- Matrix::rsparsematrix(30, 20, density = 0.3,
    rand.x = function(n) rpois(n, 5) + 1
  )
  rownames(mat) <- paste0("gene_", seq_len(30))
  colnames(mat) <- paste0("cell_", seq_len(20))

  g <- GiottoClass::createGiottoObject(expression = mat)
  gdb <- as_giottodb(g, con = con, verbose = FALSE)

  gdb_pca <- runPCA(gdb,
    expression_values = "raw", feats_to_use = NULL,
    ncp = 1, center = TRUE, verbose = FALSE
  )

  pca <- Giotto::getDimReduction(gdb_pca, reduction = "cells",
    reduction_method = "pca", name = "pca"
  )
  expect_equal(ncol(pca@coordinates), 1L)
  expect_equal(nrow(pca@coordinates), 20L)
})

test_that("runPCA feats_to_use as character vector subsets correctly", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("duckdb")

  g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  gdb <- as_giottodb(g, con = con, verbose = FALSE)

  # Pick 50 arbitrary gene IDs
  all_feats <- GiottoClass::featIDs(gdb)
  selected_feats <- all_feats[seq_len(50)]

  gdb_pca <- runPCA(gdb,
    expression_values = "normalized",
    feats_to_use = selected_feats,
    ncp = 5, center = TRUE, verbose = FALSE
  )

  pca <- Giotto::getDimReduction(gdb_pca, reduction = "cells",
    reduction_method = "pca", name = "pca"
  )
  expect_false(is.null(pca))
  expect_equal(ncol(pca@coordinates), 5L)

  # Giotto baseline on the same feature subset
  g_pca <- Giotto::runPCA(g,
    expression_values = "normalized",
    feats_to_use = selected_feats,
    ncp = 5, center = TRUE, scale_unit = FALSE, verbose = FALSE
  )
  pca_g <- Giotto::getDimReduction(g_pca, reduction = "cells",
    reduction_method = "pca", name = "pca"
  )

  common_cells <- intersect(rownames(pca@coordinates), rownames(pca_g@coordinates))
  cors <- sapply(seq_len(5), function(i) {
    abs(cor(pca@coordinates[common_cells, i], pca_g@coordinates[common_cells, i]))
  })
  expect_true(all(cors > 0.99),
    info = paste("PC correlations:", paste(round(cors, 4), collapse = ", "))
  )
})
