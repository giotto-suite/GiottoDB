# GiottoDB Dimension Reduction Plot Examples
# Demonstrates plotUMAP / plotPCA with GiottoDB objects using deck.gl and Mosaic.
# Note: ensure rDeckgl and rMosaic are installed and the GiottoDB dev version is loaded.

library(GiottoDB)
library(GiottoData)
library(Giotto)
library(duckdb)
library(DBI)

cat("Loading Giotto mini dataset...\n")
gobject <- GiottoData::loadGiottoMini("visium", verbose = FALSE)

cat("Running PCA and UMAP on the giotto object...\n")
gobject <- Giotto::runPCA(gobject, scale_unit = TRUE, center = TRUE)
gobject <- Giotto::runUMAP(gobject, dimensions_to_use = 1:10, n_components = 3)

cat("\n0) Native GiottoVisuals plots (ggplot backend)\n")
p_umap_native <- Giotto::plotUMAP(
  gobject,
  cell_color = "leiden_clus",
  point_size = 3,
  show_legend = TRUE,
  title = "UMAP (GiottoVisuals)"
)
print(p_umap_native)

p_pca_native <- Giotto::plotPCA(
  gobject,
  cell_color = "nr_feats",
  color_as_factor = FALSE,
  point_size = 3,
  show_legend = TRUE,
  title = "PCA (GiottoVisuals)"
)
print(p_pca_native)

cat("Creating in-memory DuckDB connection...\n")
con <- dbConnect(duckdb(), dbdir = ":memory:")

cat("Converting to GiottoDB object (dimension reductions preserved)...\n")
gobject_db <- as_giottodb(gobject, con = con, verbose = TRUE)


cat("\n1) UMAP deck.gl\n")
p_umap_deck <- GiottoDB::plotUMAP(
  gobject_db,
  plot_method = "deckgl",
  cell_color = "leiden_clus",
  point_size = 4,
  point_alpha = 0.8,
   initial_zoom = 4,      # new: override auto zoom if desired
  title = "UMAP (deck.gl)"
)
print(p_umap_deck)

cat("\n2) UMAP Mosaic\n")
p_umap_mosaic <- GiottoDB::plotUMAP(
  gobject_db,
  plot_method = "mosaic",
  cell_color = "leiden_clus",
  point_size = 3,
  point_alpha = 0.9,
  # Mosaic uses ggproto-like params; try numeric coloring
  color_as_factor = TRUE,
  title = "UMAP (Mosaic)"
)
print(p_umap_mosaic)

cat("\n3) PCA deck.gl\n")
p_pca_deck <- GiottoDB::plotPCA(
  gobject_db,
  plot_method = "deckgl",
  cell_color = "nr_feats",
  color_as_factor = FALSE,
  point_size = 4,
  cell_color_gradient = c("#0d0887", "#32b67a", "#f0f921"),
  initial_zoom = 4,
  title = "PCA (deck.gl)"
)
print(p_pca_deck)

cat("\n4) PCA Mosaic\n")
p_pca_mosaic <- GiottoDB::plotPCA(
  gobject_db,
  plot_method = "mosaic",
  cell_color = "nr_feats",
  color_as_factor = FALSE,
  point_size = 3,
  cell_color_gradient = c("#0d0887", "#32b67a", "#f0f921"),
  title = "PCA (Mosaic)"
)
print(p_pca_mosaic)

cat("\n5) Alternate dimensions & selections\n")
# Demonstrate using different dimensions and zoom padding
p_umap_alt <- GiottoDB::plotUMAP(
  gobject_db,
  plot_method = "deckgl",
  dim1_to_use = 2,
  dim2_to_use = 3,
  cell_color = "leiden_clus",
  point_size = 3,
  point_alpha = 0.7,
  zoom_padding = 0.25,
  title = "UMAP (deck.gl) dim2 vs dim3"
)
print(p_umap_alt)

p_pca_alt <- GiottoDB::plotPCA(
  gobject_db,
  plot_method = "mosaic",
  dim1_to_use = 1,
  dim2_to_use = 3,
  cell_color = "leiden_clus",
  point_size = 2.5,
  point_alpha = 0.8,
  title = "PCA (Mosaic) dim1 vs dim3"
)
print(p_pca_alt)

cat("\nDone. Remember to close the DuckDB connection when finished.\n")
# dbDisconnect(con, shutdown = TRUE)
