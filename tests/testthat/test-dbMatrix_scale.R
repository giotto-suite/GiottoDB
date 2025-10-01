# silence deprecated internal functions
rlang::local_options(lifecycle_verbosity = "quiet")
library(Giotto)

# ---------------------------------------------------------------------------- #
# Setup data
visium <- GiottoData::loadGiottoMini(dataset = "visium")
dgc <- getExpression(visium, output = "matrix")

tmpfile <- file.path(tempdir(), "test.db")
con <- DBI::dbConnect(duckdb::duckdb(), tmpfile)

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
gobject_db <- suppressWarnings(suppressMessages(
  createGiottoObject(expression = expObj_db)
))

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
  log_norm = FALSE,
  scale_feats = TRUE,
  scale_cells = TRUE
)

gobject_db_filtered <- normalizeGiotto(
  gobject = gobject_db_filtered,
  spat_unit = "cell",
  feat_type = "rna",
  expression_values = "raw",
  library_size_norm = FALSE,
  log_norm = FALSE,
  scale_feats = TRUE,
  scale_cells = TRUE
)
# Get normalized matrix
dgc_visium <- getExpression(
  visium_filtered,
  output = "matrix",
  values = "scaled"
) |>
  as.matrix()
mat_db <- getExpression(
  gobject_db_filtered,
  output = "matrix",
  values = "scaled"
)
dgc_db <- as.matrix(mat_db, names = TRUE)

# ---------------------------------------------------------------------------- #
# Test normalizeGiotto() equivalence between dbMatrix and dgCMatrix
test_that("dbMatrix equivalent to dgCMatrix after normalizeGiotto(scale_feats=T,scale=cells=T)", {
  expect_equal(dgc_visium, dgc_db)
})
