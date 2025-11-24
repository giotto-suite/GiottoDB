#' Internal: Spatial Plot using deck.gl Method for GiottoDB
#'
#' @keywords internal
#' @noRd
.spatPlot2D_deckgl <- function(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  sdimx = "sdimx",
  sdimy = "sdimy",
  cell_color = NULL,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  cell_color_gradient = NULL,
  gradient_midpoint = NULL,
  gradient_style = c("divergent", "sequential"),
  select_cell_groups = NULL,
  select_cells = NULL,
  point_size = 3,
  point_alpha = 1,
  show_network = FALSE,
  spatial_network_name = "Delaunay_network",
  network_color = NULL,
  title = NULL,
  ...
) {
  # Check if rDeckgl is available
  if (!requireNamespace("rDeckgl", quietly = TRUE)) {
    stop(
      "Package 'rDeckgl' is required for plot_method = 'deckgl'. ",
      "Please install it from DBVisuals package."
    )
  }

  # Validate GiottoDB object
  if (!inherits(gobject, "GiottoDB")) {
    stop(
      "spatPlot2D for GiottoDB only accepts GiottoDB objects. ",
      "Use as_giottodb() to convert a giotto object to GiottoDB."
    )
  }

  # Prepare spatial data
  data_list <- .fetch_viz_data(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    sdimx = sdimx,
    sdimy = sdimy,
    cell_color = cell_color,
    select_cells = select_cells,
    select_cell_groups = select_cell_groups
  )

  combined_data <- data_list$combined_data
  polygon_data <- data_list$polygon_data

  # Invert Y coordinates for proper spatial orientation
  # Spatial data has origin at top-left, but we need to flip it
  y_range <- range(combined_data[[sdimy]], na.rm = TRUE)
  combined_data[[paste0(sdimy, "_inverted")]] <- y_range[1] +
    y_range[2] -
    combined_data[[sdimy]]
  sdimy_plot <- paste0(sdimy, "_inverted")

  # For now, always use scatterplot for deck.gl
  # Polygon rendering requires more complex DuckDB integration that we'll add later
  # TODO: Implement proper polygon support for deck.gl with DuckDB
  result <- .generate_deckgl_scatterplot_spec(
    combined_data = combined_data,
    sdimx = sdimx,
    sdimy = sdimy_plot, # Use inverted Y
    cell_color = cell_color,
    color_as_factor = color_as_factor,
    cell_color_code = cell_color_code,
    cell_color_gradient = cell_color_gradient,
    point_size = point_size,
    point_alpha = point_alpha,
    title = title
  )

  # Create rDeckgl visualization with modified data that includes color columns
  rDeckgl::deckgl(spec = result$spec, data = list(cells = result$data))
}


#' Generate deck.gl Scatterplot Spec
#'
#' @keywords internal
#' @noRd
.generate_deckgl_scatterplot_spec <- function(
  combined_data,
  sdimx = "sdimx",
  sdimy = "sdimy",
  cell_color = NULL,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  cell_color_gradient = NULL,
  point_size = 3,
  point_alpha = 1,
  title = NULL
) {
  # Calculate viewport bounds and center point for initial view
  # This ensures the visualization is properly centered on the data
  x_range <- range(combined_data[[sdimx]], na.rm = TRUE)
  y_range <- range(combined_data[[sdimy]], na.rm = TRUE)

  x_center <- mean(x_range)
  y_center <- mean(y_range)

  # Define color accessor using deck.gl expression syntax
  # The @@ prefix indicates a deck.gl expression that will be evaluated in JavaScript
  fill_color_accessor <- "@@=[fill_color_r, fill_color_g, fill_color_b]"

  if (!is.null(cell_color) && cell_color %in% colnames(combined_data)) {
    color_values <- combined_data[[cell_color]]
    combined_data$color_value <- color_values

    if (!color_as_factor && is.numeric(color_values)) {
      # Continuous color mapping using gradient
      # Convert numeric values to RGB colors for deck.gl rendering
      palette <- .generate_color_palette(
        color_values,
        color_as_factor = FALSE,
        cell_color_code = cell_color_code,
        cell_color_gradient = cell_color_gradient
      )
      # Ensure palette has at least 2 colors for interpolation
      if (length(palette) < 2) {
        palette <- rep(palette, 2L)
      }

      # Create color ramp function in Lab color space for perceptual uniformity
      color_ramp <- grDevices::colorRamp(palette, space = "Lab")

      # Initialize RGB channels with neutral gray (128) as default
      combined_data$fill_color_r <- rep.int(128L, nrow(combined_data))
      combined_data$fill_color_g <- rep.int(128L, nrow(combined_data))
      combined_data$fill_color_b <- rep.int(128L, nrow(combined_data))

      valid_idx <- which(!is.na(color_values) & is.finite(color_values))
      if (length(valid_idx) > 0) {
        value_range <- range(color_values[valid_idx], na.rm = TRUE)
        if (diff(value_range) == 0) {
          value_range <- value_range + c(-0.5, 0.5)
        }
        scaled_vals <- (color_values[valid_idx] - value_range[1]) /
          (value_range[2] - value_range[1])
        scaled_vals <- pmin(pmax(scaled_vals, 0), 1)

        rgb_vals <- color_ramp(scaled_vals)
        combined_data$fill_color_r[valid_idx] <- as.integer(round(rgb_vals[,
          1
        ]))
        combined_data$fill_color_g[valid_idx] <- as.integer(round(rgb_vals[,
          2
        ]))
        combined_data$fill_color_b[valid_idx] <- as.integer(round(rgb_vals[,
          3
        ]))
      }
    } else {
      # Discrete or non-numeric mapping
      colors <- .generate_color_palette(
        color_values,
        color_as_factor = TRUE,
        cell_color_code = cell_color_code,
        cell_color_gradient = cell_color_gradient
      )

      keys <- as.character(color_values)
      color_map <- colors[keys]
      color_map[is.na(color_map)] <- "#FFA07A"

      rgb_vals <- grDevices::col2rgb(color_map)
      combined_data$fill_color_r <- as.integer(rgb_vals[1, ])
      combined_data$fill_color_g <- as.integer(rgb_vals[2, ])
      combined_data$fill_color_b <- as.integer(rgb_vals[3, ])
    }
  } else {
    # Default color (salmon/light coral - matches Giotto default)
    combined_data$fill_color_r <- rep.int(255L, nrow(combined_data))
    combined_data$fill_color_g <- rep.int(160L, nrow(combined_data))
    combined_data$fill_color_b <- rep.int(122L, nrow(combined_data))
  }

  # Build deck.gl spec
  spec <- list(
    initialViewState = list(
      target = list(x_center, y_center, 0),
      zoom = 0
    ),
    views = list(
      list(
        "@@type" = "OrthographicView",
        controller = TRUE
      )
    ),
    layers = list(
      list(
        "@@type" = "ScatterplotLayer",
        id = "cells",
        data = list(
          type = "duckdb",
          query = "SELECT * FROM cells"
        ),
        getPosition = sprintf("@@=[%s, %s]", sdimx, sdimy),
        getFillColor = fill_color_accessor,
        getRadius = point_size,
        radiusUnits = "pixels",
        opacity = point_alpha,
        pickable = TRUE,
        autoHighlight = TRUE
      )
    )
  )

  if (!is.null(title)) {
    spec$title <- title
  }

  return(list(spec = spec, data = combined_data))
}


#' Generate deck.gl Polygon Spec for GiottoDB
#'
#' @keywords internal
#' @noRd
.generate_deckgl_polygon_spec <- function(
  gobject,
  combined_data,
  polygon_data,
  spat_unit = NULL,
  sdimx = "sdimx",
  sdimy = "sdimy",
  cell_color = NULL,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  cell_color_gradient = NULL,
  point_alpha = 1,
  title = NULL
) {
  # For GiottoDB, we'll use SQL to extract polygon vertices
  spec <- .generate_deckgl_giottodb_polygon_spec(
    gobject = gobject,
    combined_data = combined_data,
    spat_unit = spat_unit,
    sdimx = sdimx,
    sdimy = sdimy,
    cell_color = cell_color,
    color_as_factor = color_as_factor,
    cell_color_code = cell_color_code,
    point_alpha = point_alpha,
    title = title
  )

  if (is.null(spec)) {
    # Fallback to scatterplot if polygon rendering fails
    result <- .generate_deckgl_scatterplot_spec(
      combined_data = combined_data,
      sdimx = sdimx,
      sdimy = sdimy,
      cell_color = cell_color,
      color_as_factor = color_as_factor,
      cell_color_code = cell_color_code,
      cell_color_gradient = cell_color_gradient,
      point_size = 5,
      point_alpha = point_alpha,
      title = title
    )
    return(result)
  }

  return(spec)
}


#' Generate deck.gl Polygon Spec for GiottoDB with WKB geometries
#'
#' @keywords internal
#' @noRd
.generate_deckgl_giottodb_polygon_spec <- function(
  gobject,
  combined_data,
  spat_unit = NULL,
  sdimx = "sdimx",
  sdimy = "sdimy",
  cell_color = NULL,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  point_alpha = 1,
  title = NULL
) {
  con <- .extract_duckdb_connection(gobject)
  if (is.null(con)) {
    warning(
      "Unable to locate DuckDB connection in GiottoDB object; falling back to scatterplot."
    )
    return(NULL)
  }

  available_tables <- tryCatch(DBI::dbListTables(con), error = function(e) {
    character()
  })
  # Get DuckDB table name for polygons
  if (is.null(spat_unit)) {
    spat_unit <- GiottoClass::activeSpatUnit(gobject)
  }

  base_poly_table <- paste0("gdb_poly_", spat_unit)
  candidate_pattern <- paste0("^", base_poly_table, "(\\d+)?$")
  candidate_tables <- available_tables[grepl(
    candidate_pattern,
    available_tables
  )]

  if (length(candidate_tables) == 0) {
    warning(sprintf(
      "No polygon table matching '%s' found in GiottoDB; falling back to scatterplot.",
      base_poly_table
    ))
    return(NULL)
  }

  if (base_poly_table %in% candidate_tables) {
    poly_table <- base_poly_table
  } else {
    suffix_values <- suppressWarnings(
      as.integer(sub(base_poly_table, "", candidate_tables))
    )
    suffix_values[is.na(suffix_values)] <- -Inf
    poly_table <- candidate_tables[order(suffix_values, decreasing = TRUE)][1]
  }

  # Calculate centroids SQL query
  centroid_query <- sprintf(
    "SELECT poly_ID as cell_ID,
            ST_X(ST_Centroid(ST_GeomFromWKB(geom))) as sdimx,
            ST_Y(ST_Centroid(ST_GeomFromWKB(geom))) as sdimy
     FROM %s",
    poly_table
  )

  # Polygon vertices extraction query
  polygon_query <- sprintf(
    "SELECT poly_ID as cell_ID,
            ST_X(geom_point) as x,
            ST_Y(geom_point) as y,
            path_idx
     FROM (
       SELECT poly_ID,
              geom,
              UNNEST(ST_DumpPoints(ST_GeomFromWKB(geom))) as geom_point,
              ROW_NUMBER() OVER (PARTITION BY poly_ID ORDER BY path_idx) as path_idx
       FROM %s
     )",
    poly_table
  )

  # Calculate bounds
  x_range <- range(combined_data[[sdimx]], na.rm = TRUE)
  y_range <- range(combined_data[[sdimy]], na.rm = TRUE)
  x_center <- mean(x_range)
  y_center <- mean(y_range)

  # Generate colors if specified
  fill_color_accessor <- list(200, 200, 200)
  if (!is.null(cell_color) && cell_color %in% colnames(combined_data)) {
    colors <- .generate_color_palette(
      combined_data[[cell_color]],
      color_as_factor = color_as_factor,
      cell_color_code = cell_color_code
    )
  }

  # Build spec with both polygons and centroids
  spec <- list(
    initialViewState = list(
      target = list(x_center, y_center, 0),
      zoom = 0
    ),
    views = list(
      list(
        "@@type" = "OrthographicView",
        controller = TRUE
      )
    ),
    layers = list(
      # Polygon layer
      list(
        "@@type" = "PolygonLayer",
        id = "cell-polygons",
        data = list(
          type = "duckdb",
          query = polygon_query
        ),
        getPolygon = "@@=polygon",
        getFillColor = fill_color_accessor,
        getLineColor = list(255, 255, 255),
        getLineWidth = 1,
        lineWidthUnits = "pixels",
        opacity = point_alpha * 0.8,
        pickable = TRUE,
        autoHighlight = TRUE,
        filled = TRUE,
        stroked = TRUE
      ),
      # Centroid layer
      list(
        "@@type" = "ScatterplotLayer",
        id = "cell-centroids",
        data = list(
          type = "duckdb",
          query = centroid_query
        ),
        getPosition = "@@=[sdimx, sdimy]",
        getFillColor = list(255, 0, 0),
        getRadius = 2,
        radiusUnits = "pixels",
        opacity = 0.6,
        pickable = TRUE
      )
    )
  )

  if (!is.null(title)) {
    spec$title <- title
  }

  return(list(spec = spec, data = combined_data))
}


#' Internal: Spatial Plot using Mosaic Method for GiottoDB
#'
#' @keywords internal
#' @noRd
.spatPlot2D_mosaic <- function(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  sdimx = "sdimx",
  sdimy = "sdimy",
  cell_color = NULL,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  select_cell_groups = NULL,
  select_cells = NULL,
  point_size = 3,
  point_alpha = 1,
  title = NULL,
  ...
) {
  # Check if rMosaic is available
  if (!requireNamespace("rMosaic", quietly = TRUE)) {
    stop(
      "Package 'rMosaic' is required for plot_method = 'mosaic'. ",
      "Please install it from DBVisuals package."
    )
  }

  # Validate GiottoDB object
  if (!inherits(gobject, "GiottoDB")) {
    stop(
      "spatPlot2D for GiottoDB only accepts GiottoDB objects. ",
      "Use as_giottodb() to convert a giotto object to GiottoDB."
    )
  }

  # Prepare spatial data
  data_list <- .prepare_spatial_data_db(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    sdimx = sdimx,
    sdimy = sdimy,
    cell_color = cell_color,
    select_cells = select_cells,
    select_cell_groups = select_cell_groups
  )

  combined_data <- data_list$combined_data

  # Generate Mosaic spec using unmodified spatial coordinates
  spec_result <- .generate_mosaic_spec(
    combined_data = combined_data,
    sdimx = sdimx,
    sdimy = sdimy,
    cell_color = cell_color,
    color_as_factor = color_as_factor,
    cell_color_code = cell_color_code,
    point_size = point_size,
    point_alpha = point_alpha,
    title = title
  )

  # Create rMosaic visualization
  # Always use WASM backend for better compatibility
  # Clean data to only include columns we need

  # Determine which columns are needed
  cols_needed <- c(sdimx, sdimy)
  if (!is.null(cell_color) && cell_color %in% colnames(spec_result$data)) {
    cols_needed <- c(cols_needed, cell_color)
  }

  # Extract only needed columns and ensure simple types
  data_clean <- spec_result$data[, cols_needed, drop = FALSE]
  data_clean <- as.data.frame(data_clean)

  # Convert all columns to appropriate simple types
  for (col in colnames(data_clean)) {
    if (is.factor(data_clean[[col]])) {
      data_clean[[col]] <- as.character(data_clean[[col]])
    } else if (is.list(data_clean[[col]])) {
      # Convert list columns to character
      data_clean[[col]] <- as.character(data_clean[[col]])
    } else if (
      !is.numeric(data_clean[[col]]) && !is.character(data_clean[[col]])
    ) {
      # Convert any other complex types to character
      data_clean[[col]] <- as.character(data_clean[[col]])
    }
  }

  # Ensure numeric columns are proper numeric (not integer64 or other special types)
  for (col in colnames(data_clean)) {
    if (is.numeric(data_clean[[col]])) {
      data_clean[[col]] <- as.numeric(data_clean[[col]])
    }
  }

  rMosaic::mosaic(
    spec = spec_result$spec,
    data = list(cells = data_clean),
    backend = "wasm"
  )
}


#' Generate Mosaic Spec for Spatial Data
#'
#' @keywords internal
#' @noRd
.generate_mosaic_spec <- function(
  combined_data,
  sdimx = "sdimx",
  sdimy = "sdimy",
  cell_color = NULL,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  point_size = 3,
  point_alpha = 1,
  title = NULL
) {
  # For Mosaic, we need to reverse the Y-axis scale, not transform coordinates
  # Mosaic uses standard coordinate system where Y increases upward
  # Spatial data typically has Y increasing downward (top-left origin)

  # Build simple Mosaic spec - use simple property syntax, not nested objects
  plot_spec <- list(
    mark = "dot",
    data = list(from = "cells"),
    x = sdimx, # Simple string, not list(field=..., type=...)
    y = sdimy,
    r = point_size,
    opacity = point_alpha
  )

  # Add fill color so categorical palettes show up in Mosaic
  if (!is.null(cell_color) && cell_color %in% colnames(combined_data)) {
    plot_spec$fill <- cell_color
  } else {
    # For fixed colors, use 'fill' with value syntax
    plot_spec$fill <- "steelblue" # Direct string value, not list(value = ...)
  }

  spec <- list(
    plot = list(plot_spec),
    yReverse = FALSE
  )

  return(list(spec = spec, data = combined_data))
}


#' Generate Mosaic Spec with Histogram for Spatial Data
#'
#' Creates a linked view with spatial plot and histogram
#'
#' @keywords internal
#' @noRd
.generate_mosaic_spec_with_histogram <- function(
  combined_data,
  sdimx = "sdimx",
  sdimy = "sdimy",
  cell_color = NULL,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  point_size = 3,
  point_alpha = 1,
  title = NULL
) {
  if (is.null(cell_color) || !cell_color %in% colnames(combined_data)) {
    # Fall back to simple spec if no color variable
    return(.generate_mosaic_spec(
      combined_data,
      sdimx,
      sdimy,
      cell_color,
      color_as_factor,
      cell_color_code,
      point_size,
      point_alpha,
      title
    ))
  }

  # Determine if we should use histogram or bar chart
  is_continuous <- !color_as_factor &&
    (is.numeric(combined_data[[cell_color]]) &&
      !is.factor(combined_data[[cell_color]]))

  if (is_continuous) {
    # Histogram for continuous data
    hist_mark <- "rectY"
    hist_x <- list(field = cell_color, type = "quantitative", bin = TRUE)
    hist_y <- list(aggregate = "count")
  } else {
    # Bar chart for categorical data
    hist_mark <- "barY"
    hist_x <- list(field = cell_color, type = "nominal")
    hist_y <- list(aggregate = "count")
  }

  # Determine color encoding
  if (is_continuous) {
    color_spec <- list(
      field = cell_color,
      type = "quantitative",
      scale = list(scheme = "viridis")
    )
  } else {
    color_spec <- list(
      field = cell_color,
      type = "nominal"
    )
    if (!is.null(cell_color_code)) {
      color_spec$scale <- list(range = unname(cell_color_code))
    }
  }

  # Build linked visualization spec
  spec <- list(
    meta = list(
      title = if (!is.null(title)) title else "Spatial Plot with Distribution",
      description = "Linked spatial plot and histogram"
    ),
    data = list(
      cells = list(
        type = "table"
      )
    ),
    params = list(
      brush = list(select = "crossfilter")
    ),
    hconcat = list(
      # Spatial plot
      list(
        plot = list(
          list(
            mark = "dot",
            data = list(from = "cells", filterBy = "$brush"),
            x = list(field = sdimx, type = "quantitative"),
            y = list(field = sdimy, type = "quantitative"),
            fill = color_spec,
            size = list(value = point_size * point_size),
            opacity = list(value = point_alpha),
            select = "intervalXY",
            as = "$brush"
          ),
          list(
            mark = "frame",
            data = list(from = "cells")
          )
        ),
        width = 500,
        height = 500
      ),
      # Histogram
      list(
        plot = list(
          list(
            mark = hist_mark,
            data = list(from = "cells", filterBy = "$brush"),
            x = hist_x,
            y = hist_y,
            fill = color_spec,
            select = "toggleY",
            as = "$brush"
          )
        ),
        width = 300,
        height = 500
      )
    )
  )

  return(spec)
}
