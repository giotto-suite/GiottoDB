# End-to-end pipeline equivalence: GiottoDB vs Giotto
# filter -> normalizeGiotto (standard) -> calculateHVF -> runPCA

library(GiottoDB)
library(Giotto)

test_that("full pipeline produces equivalent results for GiottoDB vs Giotto", {
  skip_if_not_installed("GiottoData")
  skip_if_not_installed("dbMatrix")
  skip_if_not_installed("dbSpatial")
  skip_if_not_installed("duckdb")

  g <- GiottoData::loadGiottoMini("visium", verbose = FALSE)

  temp_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = temp_db)
  on.exit({
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
    if (file.exists(temp_db)) file.remove(temp_db)
  }, add = TRUE)

  gdb <- as_giottodb(g, con = con, verbose = FALSE)

  # Step 1: filterGiotto on the GiottoDB object directly.
  # spatIDs/featIDs for dbSpatial-backed spatial objects are handled by
  # GiottoDB's S4 method overrides in R/dbSpatial_methods.R.
  g_f <- filterGiotto(g,
    spat_unit = "cell", feat_type = "rna",
    expression_values = "raw", verbose = FALSE
  )
  gdb_f <- filterGiotto(gdb,
    spat_unit = "cell", feat_type = "rna",
    expression_values = "raw", verbose = FALSE
  )

  expect_equal(
    sort(GiottoClass::spatIDs(g_f)),
    sort(GiottoClass::spatIDs(gdb_f))
  )
  expect_equal(
    sort(GiottoClass::featIDs(g_f)),
    sort(GiottoClass::featIDs(gdb_f))
  )

  # Step 2: normalizeGiotto (standard: library + log)
  g_n <- normalizeGiotto(g_f,
    spat_unit = "cell", feat_type = "rna",
    norm_methods = "standard", verbose = FALSE
  )
  gdb_n <- normalizeGiotto(gdb_f,
    spat_unit = "cell", feat_type = "rna",
    norm_methods = "standard", verbose = FALSE
  )

  mat_g <- getExpression(g_n, values = "normalized", output = "matrix")
  mat_gdb_raw <- getExpression(gdb_n, values = "normalized", output = "matrix")
  mat_gdb <- as.matrix(mat_gdb_raw, sparse = TRUE, names = TRUE)

  # Reorder to same row/col order for comparison
  mat_gdb <- mat_gdb[rownames(mat_g), colnames(mat_g)]
  expect_equal(mat_g, mat_gdb, tolerance = 1e-10)

  # Step 3: calculateHVF — both should mark the same features
  g_h <- suppressWarnings(calculateHVF(g_n,
    spat_unit = "cell", feat_type = "rna",
    expression_values = "normalized",
    method = "cov_groups",
    show_plot = FALSE, return_plot = FALSE, save_plot = FALSE,
    verbose = FALSE
  ))
  gdb_h <- suppressWarnings(calculateHVF(gdb_n,
    spat_unit = "cell", feat_type = "rna",
    expression_values = "normalized",
    method = "cov_groups",
    show_plot = FALSE, return_plot = FALSE, save_plot = FALSE,
    verbose = FALSE
  ))

  fmeta_g <- GiottoClass::fDataDT(g_h, spat_unit = "cell", feat_type = "rna")
  fmeta_gdb <- GiottoClass::fDataDT(gdb_h, spat_unit = "cell", feat_type = "rna")
  data.table::setorder(fmeta_g, feat_ID)
  data.table::setorder(fmeta_gdb, feat_ID)

  expect_equal(fmeta_g$hvf, fmeta_gdb$hvf)

  # Step 4: runPCA using hvf column
  g_pca <- Giotto::runPCA(g_h,
    spat_unit = "cell", feat_type = "rna",
    expression_values = "normalized",
    feats_to_use = "hvf",
    ncp = 20, center = TRUE, scale_unit = FALSE,
    verbose = FALSE
  )
  gdb_pca <- runPCA(gdb_h,
    spat_unit = "cell", feat_type = "rna",
    expression_values = "normalized",
    feats_to_use = "hvf",
    ncp = 20, center = TRUE, scale_unit = FALSE,
    verbose = FALSE
  )

  pca_g <- Giotto::getDimReduction(g_pca,
    reduction = "cells", reduction_method = "pca", name = "pca"
  )
  pca_gdb <- Giotto::getDimReduction(gdb_pca,
    reduction = "cells", reduction_method = "pca", name = "pca"
  )

  expect_false(is.null(pca_g))
  expect_false(is.null(pca_gdb))
  expect_equal(dim(pca_g@coordinates), dim(pca_gdb@coordinates))

  common_cells <- intersect(
    rownames(pca_g@coordinates), rownames(pca_gdb@coordinates)
  )
  coords_g <- pca_g@coordinates[common_cells, ]
  coords_gdb <- pca_gdb@coordinates[common_cells, ]

  cors <- sapply(seq_len(ncol(coords_g)), function(i) {
    abs(cor(coords_g[, i], coords_gdb[, i]))
  })

  expect_true(
    all(cors > 0.99),
    info = paste("PC correlations:", paste(round(cors, 4), collapse = ", "))
  )
})
