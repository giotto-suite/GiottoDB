# GiottoDB with SQL Rooms Integration
# Demonstrates how to use DBVisuals SQL Rooms with GiottoDB objects
# Updated for S3 dispatch system

library(GiottoDB)
library(GiottoData)
library(duckdb)
library(DBI)


# ===================================================================
# Step 1: Create GiottoDB Object with Persistent Database
# ===================================================================

cat("Step 1: Creating GiottoDB object with persistent database...\n\n")

# Load Giotto data
cat("Loading Giotto mini dataset...\n")
gobject <- GiottoData::loadGiottoMini("visium")

# Create persistent database file
db_path <- file.path(tempdir(), "giottodb_sqlrooms_demo.duckdb")
cat("Database will be stored at:", db_path, "\n")

# Create connection
con <- dbConnect(duckdb(), dbdir = db_path)

# Convert to GiottoDB with persistent storage
cat("Converting to GiottoDB (this may take a moment)...\n")
gobject_db <- as_giottodb(
  gobject,
  con = con,
  temporary = FALSE,  # Important: use persistent tables for SQL Rooms
  overwrite = TRUE,
  verbose = TRUE
)

cat("\nGiottoDB object created!\n")
cat("Tables in database:\n")
print(DBI::dbListTables(con))

# ===================================================================
# Step 2: Verify Visualizations Work from Database
# ===================================================================

cat("\n\nStep 2: Testing visualizations from persistent database...\n")

# Test Giotto method with regular giotto object
cat("\nTesting Giotto method (using regular giotto object)...\n")
p1 <- spatPlot2D(
  gobject,  # Regular giotto object
  cell_color = "leiden_clus",
  point_size = 2
)
print(p1)

# Test Mosaic method with GiottoDB (direct backend call)
cat("\nTesting Mosaic method (GiottoDB backend)...\n")
p2 <- GiottoDB:::.spatPlot2D_mosaic(
  gobject_db,  # GiottoDB object
  cell_color = "leiden_clus"
)
print(p2)

# Test deck.gl method with GiottoDB (direct backend call)
cat("\nTesting deck.gl method (GiottoDB backend)...\n")
p3 <- GiottoDB:::.spatPlot2D_deckgl(
  gobject_db,  # GiottoDB object
  cell_color = "leiden_clus",
  point_size = 4
)
print(p3)

# Close the connection (SQL Rooms will create its own)
dbDisconnect(con, shutdown = TRUE)

# ===================================================================
# Step 3: Launch SQL Rooms
# ===================================================================


# Launch SQL Rooms
dashboard <- DBVisuals::launchSQLRooms(db_path = db_path)


# ===================================================================
# Step 4: Clean Up
# ===================================================================

cat("\nCleaning up...\n")

# Kill R server process
if (!is.null(dashboard$r_process) && dashboard$r_process$is_alive()) {
  dashboard$r_process$kill()
  cat("R server process terminated.\n")
}

# Kill frontend process
if (!is.null(dashboard$frontend_controller) &&
    !is.null(dashboard$frontend_controller$process) &&
    dashboard$frontend_controller$process$is_alive()) {
  dashboard$frontend_controller$process$kill()
  cat("Frontend process terminated.\n")
}

# Optionally remove database file
cat("\nDo you want to remove the database file? (y/n): ")
response <- tolower(trimws(readline()))

if (response == "y" || response == "yes") {
  if (file.exists(db_path)) {
    file.remove(db_path)
    cat("Database file removed:", db_path, "\n")
  }
} else {
  cat("Database file kept at:", db_path, "\n")
  cat("You can explore it later with:\n")
  cat("  con <- dbConnect(duckdb(), dbdir='", db_path, "')\n", sep = "")
cat("  dbListTables(con)\n")
}

cat("\n=== SQL Rooms Demo Complete! ===\n")
cat("\nNote: This demo uses explicit backend calls:\n")
cat("- spatPlot2D(giotto_obj) -> GiottoVisuals::spatPlot2D()\n")
cat("- GiottoDB:::.spatPlot2D_deckgl(giottodb_obj, ...) for deck.gl\n")
cat("- GiottoDB:::.spatPlot2D_mosaic(giottodb_obj, ...) for Mosaic\n")
