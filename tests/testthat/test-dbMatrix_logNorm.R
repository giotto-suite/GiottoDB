# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")
library(GiottoDB)
library(Giotto)

# ---------------------------------------------------------------------------- #
# Setup data
visium <- GiottoData::loadGiottoMini(dataset = "visium")
dgc <- getExpression(visium, output = "matrix")

con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

dbsm <- dbMatrix::dbMatrix(
  value = dgc,
  con = con,
  name = "dgc",
  class = "dbSparseMatrix",
  overwrite = TRUE
)

# Create exprObj with dbsm
expObj_db <- createExprObj(
  expression_data = dbsm,
  expression_matrix_class = "dbMatrix",
  name = "raw"
)

# Create giotto object
gobject_db <- suppressWarnings(createGiottoObject(expression = expObj_db))

# ---------------------------------------------------------------------------- #
# Perform filtering
visium_filtered <- filterGiotto(
  visium,
  spat_unit = "cell",
  feat_type = "rna",
  expression_values = "raw"
)

gobject_db_filtered <- filterGiotto(
  gobject_db,
  spat_unit = "cell",
  feat_type = "rna",
  expression_values = "raw"
)

# ---------------------------------------------------------------------------- #
# Perform library normalization and scaling
visium_filtered <- normalizeGiotto(
  gobject = visium_filtered,
  spat_unit = "cell",
  feat_type = "rna",
  expression_values = "raw",
  library_size_norm = FALSE,
  log_norm = TRUE,
  scale_feats = FALSE,
  scale_cells = FALSE
)


gobject_db_filtered <- normalizeGiotto(
  gobject = gobject_db_filtered,
  spat_unit = "cell",
  feat_type = "rna",
  expression_values = "raw",
  library_size_norm = FALSE,
  log_norm = TRUE,
  scale_feats = FALSE,
  scale_cells = FALSE
)
# Get normalized matrix
dgc_visium <- getExpression(
  visium_filtered,
  output = "matrix",
  values = "normalized"
)
mat_db <- getExpression(
  gobject_db_filtered,
  output = "matrix",
  values = "normalized"
)
dgc_db <- as.matrix(mat_db, sparse = TRUE, names = TRUE)

# ---------------------------------------------------------------------------- #
# Test normalizeGiotto() equivalence between dbMatrix and dgCMatrix
test_that("dbMatrix equivalent to dgCMatrix after normalizeGiotto(log_norm=TRUE)", {
  expect_equal(dgc_visium, dgc_db)
})

# ---------------------------------------------------------------------------- #
# Test offset guard for dbSparseMatrix (log(0 + offset) != 0 when offset != 1)

test_that(".log_norm_giotto produces equivalent results for dbSparseMatrix", {
    mat <- Matrix::rsparsematrix(
        nrow = 10,
        ncol = 8,
        density = 0.15,
        rand.x = function(n) sample.int(10L, n, replace = TRUE) - 1L
    )
    con2 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)

    db_sparse <- dbMatrix::dbMatrix(
        value = mat,
        con = con2,
        name = "test_matrix",
        class = "dbSparseMatrix",
        overwrite = TRUE
    )

    log_norm <- Giotto:::.log_norm_giotto
    res_mat <- log_norm(mat, base = 2, offset = 1)
    res_db <- log_norm(db_sparse, base = 2, offset = 1)

    expect_equal(as.matrix(res_mat), as.matrix(res_db), ignore_attr = TRUE)
})

test_that(".log_norm_giotto rejects offset != 1 for dbSparseMatrix", {
    mat <- Matrix::rsparsematrix(
        nrow = 10,
        ncol = 8,
        density = 0.15,
        rand.x = function(n) sample.int(10L, n, replace = TRUE) - 1L
    )
    con2 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)

    db_sparse <- dbMatrix::dbMatrix(
        value = mat,
        con = con2,
        name = "test_matrix",
        class = "dbSparseMatrix",
        overwrite = TRUE
    )

    expect_error(
        Giotto:::.log_norm_giotto(mymatrix = db_sparse, base = 2, offset = 0.5),
        regexp = "offset != 1"
    )
})

test_that("processData logNormParam produces equivalent results for dbSparseMatrix", {
    mat <- Matrix::rsparsematrix(
        nrow = 10,
        ncol = 8,
        density = 0.15,
        rand.x = function(n) sample.int(10L, n, replace = TRUE) - 1L
    )
    con2 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
    on.exit(DBI::dbDisconnect(con2, shutdown = TRUE), add = TRUE)

    db_sparse <- dbMatrix::dbMatrix(
        value = mat,
        con = con2,
        name = "test_matrix",
        class = "dbSparseMatrix",
        overwrite = TRUE
    )

    param <- Giotto::normParam("log", base = 2, offset = 1)

    res_mat <- processData(mat, param)
    res_db <- processData(db_sparse, param)

    expect_equal(as.matrix(res_mat), as.matrix(res_db), ignore_attr = TRUE)
})

# ---------------------------------------------------------------------------- #
# Test standard normalization (library_size_norm = TRUE AND log_norm = TRUE combined)

test_that("dbMatrix equivalent to dgCMatrix after normalizeGiotto(norm_methods='standard')", {
  con3 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con3, shutdown = TRUE), add = TRUE)

  visium3 <- GiottoData::loadGiottoMini(dataset = "visium", verbose = FALSE)
  dgc3 <- getExpression(visium3, output = "matrix")

  dbsm3 <- dbMatrix::dbMatrix(
    value = dgc3, con = con3, name = "dgc3",
    class = "dbSparseMatrix", overwrite = TRUE
  )
  expObj3 <- createExprObj(
    expression_data = dbsm3, expression_matrix_class = "dbMatrix", name = "raw"
  )
  gobject_db3 <- suppressWarnings(createGiottoObject(expression = expObj3))

  visium3_f <- filterGiotto(visium3,
    spat_unit = "cell", feat_type = "rna", expression_values = "raw",
    verbose = FALSE
  )
  gobject_db3_f <- filterGiotto(gobject_db3,
    spat_unit = "cell", feat_type = "rna", expression_values = "raw",
    verbose = FALSE
  )

  visium3_n <- normalizeGiotto(visium3_f,
    spat_unit = "cell", feat_type = "rna",
    norm_methods = "standard", scale_feats = FALSE, scale_cells = FALSE,
    verbose = FALSE
  )
  gobject_db3_n <- normalizeGiotto(gobject_db3_f,
    spat_unit = "cell", feat_type = "rna",
    norm_methods = "standard", scale_feats = FALSE, scale_cells = FALSE,
    verbose = FALSE
  )

  mat_g <- getExpression(visium3_n, output = "matrix", values = "normalized")
  mat_db3 <- getExpression(gobject_db3_n, output = "matrix", values = "normalized")
  mat_db3 <- as.matrix(mat_db3, sparse = TRUE, names = TRUE)
  mat_db3 <- mat_db3[rownames(mat_g), colnames(mat_g)]

  expect_equal(mat_g, mat_db3, tolerance = 1e-10)
})
