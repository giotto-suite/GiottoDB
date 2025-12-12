# GiottoDB In Situ Plot Examples
# Demonstrates spatInSituPlotPoints with regular Giotto objects (GiottoVisuals),
# and GiottoDB objects using deck.gl and Mosaic backends.
# Note: the Visium mini example below does not include subcellular points, so the
# safe_plot helper will skip in-situ calls. Use a dataset with giottoPoints
# (e.g., GiottoData::getSpatialDataset(\"merfish_preoptic\") or CosMx) to see plots.

library(GiottoDB)
library(GiottoData)
library(GiottoClass)
library(duckdb)
library(DBI)

cat("Loading Giotto mini dataset...\n")
gobject <- GiottoData::loadGiottoMini("visium", verbose = FALSE)

cat("Creating in-memory DuckDB connection...\n")
con <- dbConnect(duckdb(), dbdir = ":memory:")

cat("Converting to GiottoDB object...\n")
gobject_db <- as_giottodb(gobject, con = con, verbose = TRUE)

# Grab a couple of existing features to plot
feat_meta <- GiottoClass::getFeatureMetadata(
  gobject,
  output = "data.table",
  copy_obj = TRUE
)
feats_to_plot <- head(feat_meta$feat_ID, 2)

safe_plot <- function(expr, label) {
  res <- tryCatch(
    expr,
    error = function(e) {
      message(label, " skipped: ", e$message)
      NULL
    }
  )
  if (!is.null(res)) {
    print(res)
  }
}

cat("\n1) GiottoVisuals backend on regular giotto object\n")
safe_plot(
  spatInSituPlotPoints(
    gobject,
    feats = feats_to_plot,
    sdimx = "sdimx",
    sdimy = "sdimy",
    point_size = 1.5,
    point_alpha = 0.8
  ),
  "GiottoVisuals in-situ"
)

cat("\n2) GiottoDB deck.gl backend\n")
safe_plot(
  GiottoDB::spatInSituPlotPoints(
    gobject_db,
    plot_method = "deckgl",
    feats = feats_to_plot,
    sdimx = "sdimx",
    sdimy = "sdimy",
    point_size = 2,
    point_alpha = 0.8
  ),
  "GiottoDB deck.gl in-situ"
)

cat("\n3) GiottoDB Mosaic backend\n")
safe_plot(
  GiottoDB::spatInSituPlotPoints(
    gobject_db,
    plot_method = "mosaic",
    feats = feats_to_plot,
    sdimx = "sdimx",
    sdimy = "sdimy",
    point_size = 2,
    point_alpha = 0.8
  ),
  "GiottoDB Mosaic in-situ"
)

cat("\nDone. Remember to close the DuckDB connection when finished.\n")
# dbDisconnect(con, shutdown = TRUE)

