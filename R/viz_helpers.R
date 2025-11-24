#' Extract DuckDB Connection from GiottoDB Object
#'
#' @param gobject GiottoDB object
#' @keywords internal
#' @return DBI connection or NULL
.extract_duckdb_connection <- function(gobject) {
  if (!inherits(gobject, "GiottoDB")) {
    warning("Object is not a GiottoDB object")
    return(NULL)
  }

  tryCatch(
    {
      # GiottoDB objects have a conn slot
      if (methods::.hasSlot(gobject, "conn")) {
        conn <- methods::slot(gobject, "conn")
        if (inherits(conn, "DBIConnection")) {
          return(conn)
        }
      }
      return(NULL)
    },
    error = function(e) {
      warning("Failed to extract connection: ", e$message)
      return(NULL)
    }
  )
}


#' Fetch Visualization Data
#'
#' @param gobject GiottoDB object
#' @param spat_unit Spatial unit
#' @param feat_type Feature type
#' @param spat_loc_name Spatial locations name
#' @param sdimx X dimension name
#' @param sdimy Y dimension name
#' @param cell_color Column to color by
#' @param select_cells Cell IDs to select
#' @param select_cell_groups Cell groups to select
#' @keywords internal
#' @return List with spatial_coords, cell_metadata, and polygon_data
.fetch_viz_data <- function(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  spat_loc_name = NULL,
  sdimx = "sdimx",
  sdimy = "sdimy",
  cell_color = NULL,
  select_cells = NULL,
  select_cell_groups = NULL
) {
  # 1. Set defaults using GiottoClass getters if NULL
  if (is.null(spat_unit)) {
    spat_unit <- GiottoClass::activeSpatUnit(gobject)
  }
  if (is.null(feat_type)) {
    feat_type <- GiottoClass::activeFeatType(gobject)
  }
  if (is.null(spat_loc_name)) {
    spat_loc_name <- "raw"
  } # Default to raw if not specified

  # 2. Extract Spatial Coordinates
  # GiottoClass::getSpatialLocations handles defaults and extraction
  spat_locs <- GiottoClass::getSpatialLocations(
    gobject,
    spat_unit = spat_unit,
    name = spat_loc_name,
    output = "data.table",
    copy_obj = TRUE
  )

  spatial_coords <- as.data.frame(spat_locs)

  # Validate coordinates
  if (
    !sdimx %in% colnames(spatial_coords) || !sdimy %in% colnames(spatial_coords)
  ) {
    stop(sprintf(
      "Coordinates '%s' and/or '%s' not found in spatial locations. Available columns: %s",
      sdimx,
      sdimy,
      paste(colnames(spatial_coords), collapse = ", ")
    ))
  }

  # 3. Extract Cell Metadata
  cell_meta <- GiottoClass::getCellMetadata(
    gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    output = "data.table",
    copy_obj = TRUE
  )

  cell_metadata <- as.data.frame(cell_meta)

  # 4. Merge spatial coordinates with metadata
  if (
    "cell_ID" %in%
      colnames(spatial_coords) &&
      "cell_ID" %in% colnames(cell_metadata)
  ) {
    combined_data <- merge(
      spatial_coords,
      cell_metadata,
      by = "cell_ID",
      all.x = TRUE
    )
  } else {
    combined_data <- cbind(spatial_coords, cell_metadata)
  }

  # 5. Apply cell selection if specified
  if (!is.null(select_cells)) {
    combined_data <- combined_data[combined_data$cell_ID %in% select_cells, ]
  }

  # 6. Apply group selection if specified
  if (!is.null(select_cell_groups) && !is.null(cell_color)) {
    if (cell_color %in% colnames(combined_data)) {
      combined_data <- combined_data[
        combined_data[[cell_color]] %in% select_cell_groups,
      ]
    }
  }

  # 7. Extract Polygon Data (if available)
  polygon_data <- NULL
  tryCatch(
    {
      poly_info <- GiottoClass::getPolygonInfo(
        gobject,
        polygon_name = spat_unit,
        return_giottoPolygon = TRUE
      )

      if (!is.null(poly_info) && inherits(poly_info, "giottoPolygon")) {
        if (!is.null(poly_info@spatVector)) {
          polygon_data <- poly_info@spatVector
        } else if (!is.null(poly_info@spatVectorCentroids)) {
          polygon_data <- poly_info@spatVectorCentroids
        }
      }
    },
    error = function(e) {
      # Ignore errors in polygon extraction, just return NULL
    }
  )

  return(list(
    spatial_coords = spatial_coords,
    cell_metadata = cell_metadata,
    combined_data = combined_data,
    polygon_data = polygon_data
  ))
}


#' Generate Color Palette
#'
#' @param values Vector of values to color
#' @param color_as_factor Treat as factor
#' @param cell_color_code Color codes
#' @param cell_color_gradient Color gradient
#' @keywords internal
#' @return Named vector of colors
.generate_color_palette <- function(
  values,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  cell_color_gradient = NULL
) {
  if (color_as_factor || is.character(values) || is.factor(values)) {
    # Discrete colors
    unique_vals <- unique(values)
    n_colors <- length(unique_vals)

    if (!is.null(cell_color_code)) {
      if (is.null(names(cell_color_code))) {
        # Unnamed vector - use first n colors
        colors <- cell_color_code[1:min(n_colors, length(cell_color_code))]
        names(colors) <- unique_vals
      } else {
        # Named vector - use as is
        colors <- cell_color_code
      }
    } else {
      # Use default palette
      if (n_colors <= 8) {
        palette_colors <- c(
          "#E41A1C",
          "#377EB8",
          "#4DAF4A",
          "#984EA3",
          "#FF7F00",
          "#FFFF33",
          "#A65628",
          "#F781BF"
        )
      } else {
        palette_colors <- grDevices::rainbow(n_colors)
      }
      colors <- palette_colors[1:n_colors]
      names(colors) <- unique_vals
    }
  } else {
    # Continuous colors
    if (!is.null(cell_color_gradient)) {
      colors <- cell_color_gradient
    } else {
      colors <- c("#440154FF", "#21908CFF", "#FDE724FF") # Viridis
    }
  }

  return(colors)
}
