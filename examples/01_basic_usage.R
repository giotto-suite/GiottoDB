# GiottoDB Basic Usage Examples
# This script demonstrates the main features of the GiottoDB package
# including spatPlot2D visualization with the new S3 dispatch system

# Load required packages
library(GiottoDB)
library(GiottoData)
library(duckdb)
library(DBI)
# ===================================================================
# Example 1: Create GiottoDB Object from Giotto Object
# ===================================================================

cat("Loading Giotto mini dataset...\n")
gobject <- GiottoData::loadGiottoMini("visium", verbose = FALSE)

# Create in-memory DuckDB connection
cat("Creating in-memory DuckDB connection...\n")
con <- dbConnect(duckdb(), dbdir = ":memory:")

# Convert to GiottoDB
cat("Converting to GiottoDB object...\n")
gobject_db <- as_giottodb(gobject, con = con, verbose = TRUE)

# Inspect the object
cat("\nGiottoDB object created:\n")
print(gobject_db)

cat("\nAvailable tables in database:\n")
print(DBI::dbListTables(con))


# ===================================================================
# Example 2: Basic Spatial Plots with explicit backends
# ===================================================================

cat("\n\n=== EXAMPLE 2: Explicit backend calls ===\n")

# 2.1 Native Giotto plotting (regular giotto object)
cat("\n2.1 Creating native Giotto plot (using regular giotto object)...\n")
p1 <- spatPlot2D(
  gobject,  # Regular giotto object
  cell_color = "leiden_clus",
  point_size = 2,
  show_legend = TRUE,
  title = "Giotto Method (giotto object)"
)

# 2.2 Mosaic interactive visualization (GiottoDB backend)
cat("\n2.2 Creating Mosaic plot (GiottoDB backend)...\n")
p2 <- GiottoDB::spatPlot2D(
  gobject_db,  # GiottoDB object
  plot_method = "mosaic",
  cell_color = "leiden_clus",
  point_size = 3,
  title = "Mosaic Method (GiottoDB)"
)
print(p2)

# 2.3 deck.gl WebGL rendering (GiottoDB backend)
cat("\n2.3 Creating deck.gl plot (GiottoDB backend)...\n")
p3 <- GiottoDB::spatPlot2D(
  gobject_db,  # GiottoDB object
  plot_method = "deckgl",
  cell_color = "leiden_clus",
  point_size = 4,
  point_alpha = 0.8,
  title = "Deck.gl Method (GiottoDB)"
)
print(p3)

# ===================================================================
# Example 3: Continuous Color Mapping
# ===================================================================

cat("\n\n=== EXAMPLE 3: Continuous Color Mapping ===\n")

# 3.1 Number of features per cell (continuous) - deckgl
cat("\n3.1 Plotting number of features per cell (deck.gl)...\n")
p4 <- GiottoDB::spatPlot2D(
  gobject_db,
  plot_method = "deckgl",
  cell_color = "nr_feats",
  color_as_factor = FALSE,
  cell_color_gradient = c("#440154FF", "#21908CFF", "#FDE724FF"),  # Viridis
  point_size = 4,
  title = "Number of Features (deck.gl)"
)
print(p4)

# 3.2 Total expression per cell - mosaic
cat("\n3.2 Plotting total expression per cell (Mosaic)...\n")
p5 <- GiottoDB::spatPlot2D(
  gobject_db,
  plot_method = "mosaic",
  cell_color = "total_expr",
  color_as_factor = FALSE,
  point_size = 3,
  title = "Total Expression (Mosaic)"
)
print(p5)

# 3.3 Using Giotto method with gradient (regular giotto object)
cat("\n3.3 Creating Giotto plot with continuous colors...\n")
p6 <- spatPlot2D(
  gobject,  # Regular giotto object
  cell_color = "nr_feats",
  color_as_factor = FALSE,
  gradient_style = "sequential",
  point_size = 2,
  title = "Number of Features (Giotto)"
)
print(p6)


# ===================================================================
# Example 4: Cell Selection and Filtering
# ===================================================================

cat("\n\n=== EXAMPLE 4: Cell Selection and Filtering ===\n")

# 4.1 Select specific clusters
cat("\n4.1 Selecting specific clusters (1, 2, 3)...\n")
p7 <- GiottoDB::spatPlot2D(
  gobject_db,
  plot_method = "deckgl",
  cell_color = "leiden_clus",
  select_cell_groups = c("1", "2", "3"),
  point_size = 6,
  title = "Selected Clusters"
)
print(p7)

# 4.2 Highlight cells with high expression
cat("\n4.2 Highlighting high-expression cells...\n")
cell_meta <- GiottoClass::getCellMetadata(
  gobject_db,
  output = "data.table",
  copy_obj = TRUE
)
high_expr_threshold <- quantile(cell_meta$total_expr, 0.75)
selected_cells <- cell_meta[total_expr > high_expr_threshold]$cell_ID

p8 <- spatPlot2D(
  gobject,  # Regular giotto object for this example
  cell_color = "leiden_clus",
  select_cells = selected_cells,
  show_other_cells = TRUE,
  other_cell_color = "lightgrey",
  other_cells_alpha = 0.2,
  point_size = 2,
  title = "High Expression Cells"
)
print(p8)

# 4.3 Show only selected cells (hide others)
cat("\n4.3 Showing only selected cells...\n")
p9 <- GiottoDB::spatPlot2D(
  gobject_db,
  plot_method = "mosaic",
  cell_color = "leiden_clus",
  select_cell_groups = c("4", "5"),
  point_size = 4,
  title = "Clusters 4 and 5 Only"
)
print(p9)


# ===================================================================
# Example 5: Custom Color Palettes
# ===================================================================

cat("\n\n=== EXAMPLE 5: Custom Color Palettes ===\n")

# Define custom colors for clusters
custom_colors <- c(
  "1" = "#E41A1C",
  "2" = "#377EB8",
  "3" = "#4DAF4A",
  "4" = "#984EA3",
  "5" = "#FF7F00",
  "6" = "#FFFF33"
)

# 5.1 Mosaic with custom colors
cat("\n5.1 Creating Mosaic plot with custom colors...\n")
p10 <- GiottoDB::spatPlot2D(
  gobject_db,
  plot_method = "mosaic",
  cell_color = "leiden_clus",
  cell_color_code = custom_colors,
  point_size = 3,
  title = "Custom Color Palette (Mosaic)"
)
print(p10)

# 5.2 Deck.gl with custom colors
cat("\n5.2 Creating deck.gl plot with custom colors...\n")
p11 <- GiottoDB::spatPlot2D(
  gobject_db,
  plot_method = "deckgl",
  cell_color = "leiden_clus",
  cell_color_code = custom_colors,
  point_size = 4,
  point_alpha = 0.9,
  title = "Custom Color Palette (deck.gl)"
)
print(p11)


# ===================================================================
# Example 6: Different Point Sizes and Transparency
# ===================================================================

cat("\n\n=== EXAMPLE 6: Point Sizes and Transparency ===\n")

# 6.1 Small points, high transparency
cat("\n6.1 Small points with high transparency...\n")
p12 <- GiottoDB::spatPlot2D(
  gobject_db,
  plot_method = "deckgl",
  cell_color = "leiden_clus",
  point_size = 2,
  point_alpha = 0.5,
  title = "Small Points, Transparent"
)
print(p12)

# 6.2 Large points, full opacity
cat("\n6.2 Large points with full opacity...\n")
p13 <- GiottoDB::spatPlot2D(
  gobject_db,
  plot_method = "deckgl",
  cell_color = "leiden_clus",
  point_size = 8,
  point_alpha = 1.0,
  title = "Large Points, Opaque"
)
print(p13)


# ===================================================================
# Example 7: Persistent Database and SQL Rooms Integration
# ===================================================================

cat("\n\n=== EXAMPLE 7: Persistent Database and SQL Rooms ===\n")

# 7.1 Create a persistent database
cat("\n7.1 Creating persistent database...\n")
db_path <- file.path(tempdir(), "giottodb_example.duckdb")
con_persistent <- dbConnect(duckdb(), dbdir = db_path)

# Convert to GiottoDB with persistent storage
cat("Converting to GiottoDB with persistent storage...\n")
gobject_db_persistent <- as_giottodb(
  gobject,
  con = con_persistent,
  temporary = FALSE,  # Store as persistent tables
  verbose = TRUE
)

cat("\nPersistent database created at:", db_path, "\n")
cat("Tables in persistent database:\n")
print(DBI::dbListTables(con_persistent))

# Visualize from persistent database
cat("\n7.2 Visualizing from persistent database...\n")
p14 <- GiottoDB::spatPlot2D(
  gobject_db_persistent,
  plot_method = "deckgl",
  cell_color = "leiden_clus",
  point_size = 4,
  title = "From Persistent Database"
)
print(p14)

# 7.3 Launch SQL Rooms (if DBVisuals is available)
if (requireNamespace("DBVisuals", quietly = TRUE)) {
  cat("\n7.3 Launching SQL Rooms interface...\n")
  cat("NOTE: This will open a browser window with SQL Rooms.\n")
  cat("      Press Enter to launch, or Ctrl+C to skip...\n")

  # Uncomment the following lines to actually launch SQL Rooms
  # readline(prompt = "Press [Enter] to launch SQL Rooms: ")
  #
  # dashboard <- DBVisuals::launchSQLRooms(db_path = db_path)
  #
  # cat("\nSQL Rooms is running. Explore your data in the browser!\n")
  # cat("Press [Enter] when done to clean up...\n")
  # readline()
  #
  # # Clean up processes
  # if (!is.null(dashboard$r_process) && dashboard$r_process$is_alive()) {
  #   dashboard$r_process$kill()
  # }
  # if (!is.null(dashboard$frontend_controller) &&
  #     !is.null(dashboard$frontend_controller$process) &&
  #     dashboard$frontend_controller$process$is_alive()) {
  #   dashboard$frontend_controller$process$kill()
  # }
  # cat("SQL Rooms processes terminated.\n")
} else {
  cat("\n7.3 DBVisuals package not available. Skipping SQL Rooms demo.\n")
  cat("    Install with: devtools::install_local('../DBVisuals')\n")
}

# Close persistent connection
dbDisconnect(con_persistent, shutdown = TRUE)


# ===================================================================
# Example 8: Comparing Methods Side by Side
# ===================================================================

cat("\n\n=== EXAMPLE 8: Method Comparison ===\n")

cat("\n8.1 Same data, three different approaches:\n")

# Create a comparison with the same parameters
comparison_params <- list(
  cell_color = "leiden_clus",
  point_size = 3,
  point_alpha = 0.8
)

cat("\nMethod 1: Giotto (static ggplot) - uses regular giotto object\n")
p_giotto <- spatPlot2D(
  gobject,  # Regular giotto object
  cell_color = comparison_params$cell_color,
  point_size = comparison_params$point_size,
  point_alpha = comparison_params$point_alpha,
  title = "Giotto Static Plot"
)
print(p_giotto)

cat("\nMethod 2: Mosaic (interactive, linked views) - GiottoDB backend\n")
p_mosaic <- GiottoDB::spatPlot2D(
  gobject_db,  # GiottoDB object
  plot_method = "mosaic",
  cell_color = comparison_params$cell_color,
  point_size = comparison_params$point_size,
  point_alpha = comparison_params$point_alpha,
  title = "Mosaic Interactive"
)
print(p_mosaic)

cat("\nMethod 3: Deck.gl (WebGL, high performance) - GiottoDB backend\n")
p_deckgl <- GiottoDB::spatPlot2D(
  gobject_db,  # GiottoDB object
  plot_method = "deckgl",
  cell_color = comparison_params$cell_color,
  point_size = comparison_params$point_size,
  point_alpha = comparison_params$point_alpha,
  title = "Deck.gl WebGL"
)
print(p_deckgl)


# ===================================================================
# Clean Up
# ===================================================================

cat("\n\n=== Cleaning Up ===\n")

# Close in-memory connection
dbDisconnect(con, shutdown = TRUE)
cat("In-memory database connection closed.\n")

# Remove persistent database file if it exists
if (file.exists(db_path)) {
  file.remove(db_path)
  cat("Persistent database file removed.\n")
}

cat("\n=== Examples Complete! ===\n")
cat("\nKey Takeaways (S3 Dispatch System):\n")
cat("1. Call backends with GiottoDB::spatPlot2D(gobject_db, plot_method = 'deckgl' or 'mosaic')\n")
cat("2. Regular giotto objects still use spatPlot2D() from GiottoVisuals\n")
cat("3. Supports both discrete and continuous color mappings\n")
cat("4. Cell filtering and selection capabilities\n")
cat("5. Custom color palettes\n")
cat("6. Integration with SQL Rooms for interactive exploration\n")
cat("\nFor more information, see: ?GiottoDB::spatPlot2D\n")
