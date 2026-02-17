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
  initial_zoom = NULL,
  zoom_padding = 0.1,
  ...
) {
  # Check if rDeckgl is available
  if (!requireNamespace("rDeckgl", quietly = TRUE)) {
    stop(
      "Package 'rDeckgl' is required for plot_method = 'deckgl'. ",
      "Please install it from dbVisuals package."
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
    initial_zoom = initial_zoom,
    zoom_padding = zoom_padding,
    title = title,
    data_name = "cells",
    layer_id = "cells"
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
  initial_zoom = NULL,
  zoom_padding = 0.1,
  title = NULL,
  data_name = "cells",
  layer_id = NULL
) {
  if (is.null(layer_id)) {
    layer_id <- data_name
  }

  sdimx <- as.character(sdimx)
  sdimy <- as.character(sdimy)
  if (!all(c(sdimx, sdimy) %in% colnames(combined_data))) {
    stop(
      "Required coordinate columns not found: ",
      paste(setdiff(c(sdimx, sdimy), colnames(combined_data)), collapse = ", ")
    )
  }

  # Drop rows with missing coordinates to avoid deck.gl range warnings
  coord_complete <- stats::complete.cases(combined_data[, c(sdimx, sdimy)])
  combined_data <- combined_data[coord_complete, , drop = FALSE]
  if (nrow(combined_data) == 0) {
    stop("No non-missing coordinates available for plotting.")
  }

  # Calculate viewport bounds and center point for initial view
  # This ensures the visualization is properly centered on the data
  x_range <- range(combined_data[[sdimx]], na.rm = TRUE)
  y_range <- range(combined_data[[sdimy]], na.rm = TRUE)

  x_center <- mean(x_range)
  y_center <- mean(y_range)

  zoom <- if (is.null(initial_zoom)) 0 else initial_zoom

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
  # Ensure simple types for JSON
  combined_data <- as.data.frame(combined_data)
  combined_data[] <- lapply(combined_data, function(col) {
    if (is.factor(col)) as.character(col) else col
  })

  table_sql <- DBI::dbQuoteIdentifier(DBI::ANSI(), data_name)

  spec <- list(
    initialViewState = list(
      target = list(x_center, y_center, 0),
      zoom = zoom
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
        id = layer_id,
        data = list(
          type = "duckdb",
          query = sprintf("SELECT * FROM %s", table_sql)
        ),
        getPosition = sprintf("@@=[%s, %s]", sdimx, sdimy),
        getFillColor = fill_color_accessor,
        getRadius = point_size,
        radiusUnits = "pixels",
        opacity = point_alpha,
        coordinateSystem = "@@#COORDINATE_SYSTEM.CARTESIAN",
        positionFormat = "XY",
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


#' Build polygon layer for in-situ deck.gl plots using DuckDB geometry tables
#'
#' @keywords internal
#' @noRd
.build_in_situ_polygon_layer_deckgl <- function(
  gobject,
  spat_unit = NULL,
  polygon_feat_type = NULL,
  polygon_fill = NULL,
  polygon_fill_gradient = NULL,
  polygon_fill_gradient_midpoint = NULL,
  polygon_fill_gradient_style = c("divergent", "sequential"),
  polygon_fill_as_factor = NULL,
  polygon_fill_code = NULL,
  polygon_color = "grey",
  polygon_bg_color = "black",
  polygon_alpha = NULL,
  polygon_line_size = 0.4
) {
  polygon_fill_gradient_style <- match.arg(polygon_fill_gradient_style)

  con <- .extract_duckdb_connection(gobject)
  if (is.null(con)) {
    warning(
      "No DuckDB connection available in GiottoDB object; skipping polygon overlay."
    )
    return(NULL)
  }

  poly_unit <- polygon_feat_type %||% spat_unit %||% GiottoClass::activeSpatUnit(gobject)
  base_poly <- paste0("gdb_poly_", poly_unit)

  available_tables <- tryCatch(DBI::dbListTables(con), error = function(e) {
    character()
  })
  candidate_tables <- available_tables[grepl(
    paste0("^", base_poly),
    available_tables
  )]

  if (length(candidate_tables) == 0) {
    warning(
      sprintf(
        "No polygon table found for unit '%s'; skipping polygon overlay.",
        poly_unit
      )
    )
    return(NULL)
  }

  preferred_tables <- c(paste0(base_poly, "_persisted"), base_poly)
  preferred_tables <- preferred_tables[preferred_tables %in% candidate_tables]
  poly_table <- if (length(preferred_tables) > 0) {
    preferred_tables[[1]]
  } else {
    candidate_tables[[1]]
  }

  fields <- tryCatch(DBI::dbListFields(con, poly_table), error = function(e) {
    character()
  })
  geom_col <- if ("geom" %in% fields) {
    "geom"
  } else if ("geometry" %in% fields) {
    "geometry"
  } else {
    warning(
      sprintf(
        "Could not find geometry column in polygon table '%s'; skipping polygon overlay.",
        poly_table
      )
    )
    return(NULL)
  }

  # Detect geometry type to decide whether we need ST_GeomFromWKB
  geom_type <- tryCatch(
    DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT data_type FROM information_schema.columns WHERE table_name = '%s' AND column_name = '%s'",
        poly_table,
        geom_col
      )
    ),
    error = function(e) NULL
  )
  geom_is_geometry <- !is.null(geom_type) &&
    nrow(geom_type) > 0 &&
    grepl("GEOMETRY", toupper(as.character(geom_type$data_type[[1]])))

  polygon_alpha <- polygon_alpha %||% 0.6
  polygon_alpha <- pmin(pmax(polygon_alpha, 0), 1)
  alpha_byte <- as.integer(round(polygon_alpha * 255))

  default_fill_rgb <- grDevices::col2rgb(polygon_bg_color)
  fill_accessor <- list(
    as.integer(default_fill_rgb[1, 1]),
    as.integer(default_fill_rgb[2, 1]),
    as.integer(default_fill_rgb[3, 1]),
    alpha_byte
  )

  polygon_color_rgb <- NULL
  polygon_stroked <- TRUE
  if (!is.na(polygon_color)) {
    line_rgb <- grDevices::col2rgb(polygon_color)
    polygon_color_rgb <- as.integer(line_rgb[, 1])
  } else {
    polygon_stroked <- FALSE
    polygon_color_rgb <- c(0L, 0L, 0L, 0L)
  }

  has_fill_colors <- FALSE
  color_join_sql <- ""
  color_cols_sql <- ""

  if (!is.null(polygon_fill) && polygon_fill %in% fields) {
    # Fetch distinct fill values to build a palette
    fill_values <- tryCatch(
      DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT DISTINCT %s AS fill_key FROM %s WHERE %s IS NOT NULL",
          DBI::dbQuoteIdentifier(con, polygon_fill),
          DBI::dbQuoteIdentifier(con, poly_table),
          DBI::dbQuoteIdentifier(con, polygon_fill)
        )
      )$fill_key,
      error = function(e) NULL
    )

    if (!is.null(fill_values) && length(fill_values) > 0) {
      # Determine factor/continuous behaviour
      fill_numeric <- is.numeric(fill_values)
      color_as_factor <- if (is.null(polygon_fill_as_factor)) {
        !fill_numeric
      } else {
        polygon_fill_as_factor
      }

      alpha_vec <- rep.int(alpha_byte, length(fill_values))

      if (!color_as_factor && fill_numeric) {
        range_vals <- tryCatch(
          DBI::dbGetQuery(
            con,
            sprintf(
              "SELECT MIN(%s) AS mn, MAX(%s) AS mx FROM %s WHERE %s IS NOT NULL",
              DBI::dbQuoteIdentifier(con, polygon_fill),
              DBI::dbQuoteIdentifier(con, polygon_fill),
              DBI::dbQuoteIdentifier(con, poly_table),
              DBI::dbQuoteIdentifier(con, polygon_fill)
            )
          ),
          error = function(e) NULL
        )

        value_range <- if (!is.null(range_vals) && nrow(range_vals) > 0) {
          c(range_vals$mn, range_vals$mx)
        } else {
          range(fill_values, na.rm = TRUE)
        }
        if (!is.finite(diff(value_range)) || diff(value_range) == 0) {
          value_range <- value_range + c(-0.5, 0.5)
        }

        palette <- .generate_color_palette(
          fill_values,
          color_as_factor = FALSE,
          cell_color_gradient = polygon_fill_gradient,
          cell_color_code = polygon_fill_code
        )
        if (length(palette) < 2) {
          palette <- rep(palette, 2L)
        }
        color_ramp <- grDevices::colorRamp(palette, space = "Lab")
        scaled_vals <- (fill_values - value_range[1]) / (value_range[2] - value_range[1])
        scaled_vals <- pmin(pmax(scaled_vals, 0), 1)
        rgb_vals <- color_ramp(scaled_vals)

        color_map <- data.frame(
          fill_key = fill_values,
          fill_color_r = as.integer(round(rgb_vals[, 1])),
          fill_color_g = as.integer(round(rgb_vals[, 2])),
          fill_color_b = as.integer(round(rgb_vals[, 3])),
          fill_color_a = alpha_vec,
          stringsAsFactors = FALSE
        )
      } else {
        colors <- .generate_color_palette(
          fill_values,
          color_as_factor = TRUE,
          cell_color_code = polygon_fill_code,
          cell_color_gradient = polygon_fill_gradient
        )
        keys <- as.character(fill_values)
        color_map_vals <- colors[keys]
        color_map_vals[is.na(color_map_vals)] <- "#FFA07A"
        rgb_vals <- grDevices::col2rgb(color_map_vals)

        color_map <- data.frame(
          fill_key = fill_values,
          fill_color_r = as.integer(rgb_vals[1, ]),
          fill_color_g = as.integer(rgb_vals[2, ]),
          fill_color_b = as.integer(rgb_vals[3, ]),
          fill_color_a = alpha_vec,
          stringsAsFactors = FALSE
        )
      }

      color_table_name <- paste0("gdb_poly_color_", sample.int(1e9, 1))
      tryCatch(
        DBI::dbWriteTable(
          con,
          name = color_table_name,
          value = color_map,
          overwrite = TRUE,
          temporary = TRUE
        ),
        error = function(e) {
          warning("Failed to register polygon color map: ", e$message)
          NULL
        }
      )

      if (color_table_name %in% DBI::dbListTables(con)) {
        color_join_sql <- sprintf(
          " LEFT JOIN %s AS c ON p.%s = c.fill_key ",
          DBI::dbQuoteIdentifier(con, color_table_name),
          DBI::dbQuoteIdentifier(con, polygon_fill)
        )
        color_cols_sql <- ", c.fill_color_r, c.fill_color_g, c.fill_color_b, c.fill_color_a"
        fill_accessor <- "@@=[fill_color_r, fill_color_g, fill_color_b, fill_color_a]"
        has_fill_colors <- TRUE
      }
    }
  }

  geom_expr_raw <- if (geom_is_geometry) {
    sprintf("p.%s", DBI::dbQuoteIdentifier(con, geom_col))
  } else {
    sprintf("ST_GeomFromWKB(p.%s)", DBI::dbQuoteIdentifier(con, geom_col))
  }

  # Flip Y axis to match point inversion (top-left origin)
  geom_expr <- geom_expr_raw
  y_bounds <- tryCatch(
    DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT MIN(ST_YMin(%s)) AS ymin, MAX(ST_YMax(%s)) AS ymax FROM %s",
        geom_expr_raw,
        geom_expr_raw,
        DBI::dbQuoteIdentifier(con, poly_table)
      )
    ),
    error = function(e) NULL
  )
  if (!is.null(y_bounds) &&
    nrow(y_bounds) > 0 &&
    is.finite(y_bounds$ymin[[1]]) &&
    is.finite(y_bounds$ymax[[1]])) {
    y_offset <- y_bounds$ymin[[1]] + y_bounds$ymax[[1]]
    geom_expr <- sprintf(
      "ST_Translate(ST_Scale(%s, 1, -1), 0, %f)",
      geom_expr_raw,
      y_offset
    )
  }

  polygon_query <- sprintf(
    "SELECT p.poly_ID, %s AS geometry%s FROM %s AS p%s",
    geom_expr,
    color_cols_sql,
    DBI::dbQuoteIdentifier(con, poly_table),
    color_join_sql
  )

  list(
    "@@type" = "GeoArrowSolidPolygonLayer",
    id = paste0("polygons-", poly_unit),
    data = list(
      type = "duckdb",
      query = polygon_query,
      format = "geoarrow"
    ),
    geometryColumn = "geometry",
    getFillColor = fill_accessor,
    getLineColor = polygon_color_rgb,
    getLineWidth = polygon_line_size,
    lineWidthUnits = "pixels",
    stroked = polygon_stroked,
    filled = TRUE,
    pickable = TRUE,
    autoHighlight = TRUE,
    parameters = list(pickingRadius = 2)
  )
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
      title = title,
      data_name = "cells",
      layer_id = "cells"
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
      "Please install it from dbVisuals package."
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

  # Generate Mosaic spec using unmodified spatial coordinates
  spec_result <- .generate_mosaic_spec(
    combined_data = combined_data,
    sdimx = sdimx,
    sdimy = sdimy,
    cell_color = cell_color,
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


#' Internal: UMAP using deck.gl for GiottoDB
#'
#' @keywords internal
#' @noRd
.plotUMAP_deckgl <- function(
  gobject,
  dim_reduction_name = NULL,
  cell_color = NULL,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  cell_color_gradient = NULL,
  dim1_to_use = 1,
  dim2_to_use = 2,
  point_size = 3,
  point_alpha = 1,
  initial_zoom = NULL,
  zoom_padding = 0.1,
  title = NULL,
  ...
) {
  if (!requireNamespace("rDeckgl", quietly = TRUE)) {
    stop(
      "Package 'rDeckgl' is required for plot_method = 'deckgl'. ",
      "Please install it from dbVisuals package."
    )
  }

  dim_info <- .fetch_dimred_data(
    gobject = gobject,
    method = "umap",
    dim_reduction_name = dim_reduction_name,
    dim1_to_use = dim1_to_use,
    dim2_to_use = dim2_to_use,
    cell_color = cell_color
  )

  combined_data <- dim_info$data
  sdimx <- dim_info$sdimx
  sdimy <- dim_info$sdimy

  result <- .generate_deckgl_scatterplot_spec(
    combined_data = combined_data,
    sdimx = sdimx,
    sdimy = sdimy,
    cell_color = cell_color,
    color_as_factor = color_as_factor,
    cell_color_code = cell_color_code,
    cell_color_gradient = cell_color_gradient,
    point_size = point_size,
    point_alpha = point_alpha,
    initial_zoom = initial_zoom,
    zoom_padding = zoom_padding,
    title = title,
    data_name = "cells",
    layer_id = "cells"
  )

  rDeckgl::deckgl(spec = result$spec, data = list(cells = result$data))
}


#' Internal: UMAP using Mosaic for GiottoDB
#'
#' @keywords internal
#' @noRd
.plotUMAP_mosaic <- function(
  gobject,
  dim_reduction_name = NULL,
  cell_color = NULL,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  cell_color_gradient = NULL,
  dim1_to_use = 1,
  dim2_to_use = 2,
  point_size = 3,
  point_alpha = 1,
  initial_zoom = NULL,
  zoom_padding = 0.1,
  title = NULL,
  ...
) {
  if (!requireNamespace("rMosaic", quietly = TRUE)) {
    stop(
      "Package 'rMosaic' is required for plot_method = 'mosaic'. ",
      "Please install it from dbVisuals package."
    )
  }

  dim_info <- .fetch_dimred_data(
    gobject = gobject,
    method = "umap",
    dim_reduction_name = dim_reduction_name,
    dim1_to_use = dim1_to_use,
    dim2_to_use = dim2_to_use,
    cell_color = cell_color
  )

  combined_data <- dim_info$data
  sdimx <- dim_info$sdimx
  sdimy <- dim_info$sdimy

  spec_result <- .generate_mosaic_spec(
    combined_data = combined_data,
    sdimx = sdimx,
    sdimy = sdimy,
    cell_color = cell_color,
    point_size = point_size,
    point_alpha = point_alpha,
    title = title
  )

  cols_needed <- c(sdimx, sdimy)
  if (!is.null(cell_color) && cell_color %in% colnames(spec_result$data)) {
    cols_needed <- c(cols_needed, cell_color)
  }
  missing_cols <- setdiff(cols_needed, colnames(spec_result$data))
  if (length(missing_cols) > 0) {
    stop(
      "UMAP data is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  data_clean <- spec_result$data[, cols_needed, drop = FALSE]
  data_clean <- as.data.frame(data_clean)

  for (col in colnames(data_clean)) {
    if (is.factor(data_clean[[col]])) data_clean[[col]] <- as.character(data_clean[[col]])
    if (is.list(data_clean[[col]])) data_clean[[col]] <- as.character(data_clean[[col]])
    if (!is.numeric(data_clean[[col]]) && !is.character(data_clean[[col]])) {
      data_clean[[col]] <- as.character(data_clean[[col]])
    }
    if (is.numeric(data_clean[[col]])) data_clean[[col]] <- as.numeric(data_clean[[col]])
  }

  rMosaic::mosaic(
    spec = spec_result$spec,
    data = list(cells = data_clean),
    backend = "wasm"
  )
}


#' Internal: PCA using deck.gl for GiottoDB
#'
#' @keywords internal
#' @noRd
.plotPCA_deckgl <- function(
  gobject,
  dim_reduction_name = NULL,
  cell_color = NULL,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  cell_color_gradient = NULL,
  dim1_to_use = 1,
  dim2_to_use = 2,
  point_size = 3,
  point_alpha = 1,
  initial_zoom = NULL,
  zoom_padding = 0.1,
  title = NULL,
  ...
) {
  if (!requireNamespace("rDeckgl", quietly = TRUE)) {
    stop(
      "Package 'rDeckgl' is required for plot_method = 'deckgl'. ",
      "Please install it from dbVisuals package."
    )
  }

  dim_info <- .fetch_dimred_data(
    gobject = gobject,
    method = "pca",
    dim_reduction_name = dim_reduction_name,
    dim1_to_use = dim1_to_use,
    dim2_to_use = dim2_to_use,
    cell_color = cell_color
  )

  combined_data <- dim_info$data
  sdimx <- dim_info$sdimx
  sdimy <- dim_info$sdimy

  result <- .generate_deckgl_scatterplot_spec(
    combined_data = combined_data,
    sdimx = sdimx,
    sdimy = sdimy,
    cell_color = cell_color,
    color_as_factor = color_as_factor,
    cell_color_code = cell_color_code,
    cell_color_gradient = cell_color_gradient,
    point_size = point_size,
    point_alpha = point_alpha,
    initial_zoom = initial_zoom,
    zoom_padding = zoom_padding,
    title = title,
    data_name = "cells",
    layer_id = "cells"
  )

  rDeckgl::deckgl(spec = result$spec, data = list(cells = result$data))
}


#' Internal: PCA using Mosaic for GiottoDB
#'
#' @keywords internal
#' @noRd
.plotPCA_mosaic <- function(
  gobject,
  dim_reduction_name = NULL,
  cell_color = NULL,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  cell_color_gradient = NULL,
  dim1_to_use = 1,
  dim2_to_use = 2,
  point_size = 3,
  point_alpha = 1,
  initial_zoom = NULL,
  zoom_padding = 0.1,
  title = NULL,
  ...
) {
  if (!requireNamespace("rMosaic", quietly = TRUE)) {
    stop(
      "Package 'rMosaic' is required for plot_method = 'mosaic'. ",
      "Please install it from dbVisuals package."
    )
  }

  dim_info <- .fetch_dimred_data(
    gobject = gobject,
    method = "pca",
    dim_reduction_name = dim_reduction_name,
    dim1_to_use = dim1_to_use,
    dim2_to_use = dim2_to_use,
    cell_color = cell_color
  )

  combined_data <- dim_info$data
  sdimx <- dim_info$sdimx
  sdimy <- dim_info$sdimy

  spec_result <- .generate_mosaic_spec(
    combined_data = combined_data,
    sdimx = sdimx,
    sdimy = sdimy,
    cell_color = cell_color,
    point_size = point_size,
    point_alpha = point_alpha,
    title = title
  )

  cols_needed <- c(sdimx, sdimy)
  if (!is.null(cell_color) && cell_color %in% colnames(spec_result$data)) {
    cols_needed <- c(cols_needed, cell_color)
  }
  missing_cols <- setdiff(cols_needed, colnames(spec_result$data))
  if (length(missing_cols) > 0) {
    stop(
      "PCA data is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  data_clean <- spec_result$data[, cols_needed, drop = FALSE]
  data_clean <- as.data.frame(data_clean)

  for (col in colnames(data_clean)) {
    if (is.factor(data_clean[[col]])) data_clean[[col]] <- as.character(data_clean[[col]])
    if (is.list(data_clean[[col]])) data_clean[[col]] <- as.character(data_clean[[col]])
    if (!is.numeric(data_clean[[col]]) && !is.character(data_clean[[col]])) {
      data_clean[[col]] <- as.character(data_clean[[col]])
    }
    if (is.numeric(data_clean[[col]])) data_clean[[col]] <- as.numeric(data_clean[[col]])
  }

  rMosaic::mosaic(
    spec = spec_result$spec,
    data = list(cells = data_clean),
    backend = "wasm"
  )
}


#' Internal: In situ points using deck.gl for GiottoDB
#'
#' @keywords internal
#' @noRd
.spatInSituPlotPoints_deckgl <- function(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  feats = NULL,
  sdimx = "x",
  sdimy = "y",
  expand_counts = FALSE,
  count_info_column = "count",
  jitter = c(0, 0),
  point_size = 1.5,
  point_alpha = 1,
  feats_color_code = NULL,
  color_as_factor = TRUE,
  show_polygon = TRUE,
  polygon_feat_type = "cell",
  polygon_color = "grey",
  polygon_bg_color = "black",
  polygon_fill = NULL,
  polygon_fill_gradient = NULL,
  polygon_fill_gradient_midpoint = NULL,
  polygon_fill_gradient_style = c("divergent", "sequential"),
  polygon_fill_as_factor = NULL,
  polygon_fill_code = NULL,
  polygon_alpha = NULL,
  polygon_line_size = 0.4,
  plot_last = c("polygons", "points"),
  initial_zoom = NULL,
  zoom_padding = 0.1,
  title = NULL,
  ...
) {
  if (!requireNamespace("rDeckgl", quietly = TRUE)) {
    stop(
      "Package 'rDeckgl' is required for plot_method = 'deckgl'. ",
      "Please install it from dbVisuals package."
    )
  }

  if (!inherits(gobject, "GiottoDB")) {
    stop(
      "spatInSituPlotPoints for GiottoDB only accepts GiottoDB objects. ",
      "Use as_giottodb() to convert a giotto object to GiottoDB."
    )
  }

  plot_last <- match.arg(plot_last)
  polygon_fill_gradient_style <- match.arg(polygon_fill_gradient_style)

  data_info <- .fetch_in_situ_data(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    feats = feats,
    sdimx = sdimx,
    sdimy = sdimy
  )

  combined_data <- .prepare_in_situ_points(
    data = data_info$data,
    feature_col = data_info$feature_col,
    sdimx = data_info$sdimx,
    sdimy = data_info$sdimy,
    expand_counts = expand_counts,
    count_info_column = count_info_column,
    jitter = jitter
  )
  feature_col <- data_info$feature_col
  sdimx <- data_info$sdimx
  sdimy <- data_info$sdimy

  if (nrow(combined_data) == 0) {
    stop("No in situ feature coordinates available for plotting.")
  }

  # Invert Y axis to match spatial orientation (top-left origin)
  y_range <- range(combined_data[[sdimy]], na.rm = TRUE)
  combined_data[[paste0(sdimy, "_inverted")]] <- y_range[1] +
    y_range[2] -
    combined_data[[sdimy]]
  sdimy_plot <- paste0(sdimy, "_inverted")

  result <- .generate_deckgl_scatterplot_spec(
    combined_data = combined_data,
    sdimx = sdimx,
    sdimy = sdimy_plot,
    cell_color = feature_col,
    color_as_factor = color_as_factor,
    cell_color_code = feats_color_code,
    point_size = point_size,
    point_alpha = point_alpha,
    initial_zoom = initial_zoom,
    zoom_padding = zoom_padding,
    title = title,
    data_name = "features",
    layer_id = "features"
  )

  polygon_layer <- NULL
  if (isTRUE(show_polygon)) {
    polygon_layer <- .build_in_situ_polygon_layer_deckgl(
      gobject = gobject,
      spat_unit = spat_unit,
      polygon_feat_type = polygon_feat_type,
      polygon_fill = polygon_fill,
      polygon_fill_gradient = polygon_fill_gradient,
      polygon_fill_gradient_midpoint = polygon_fill_gradient_midpoint,
      polygon_fill_gradient_style = polygon_fill_gradient_style,
      polygon_fill_as_factor = polygon_fill_as_factor,
      polygon_fill_code = polygon_fill_code,
      polygon_color = polygon_color,
      polygon_bg_color = polygon_bg_color,
      polygon_alpha = polygon_alpha,
      polygon_line_size = polygon_line_size
    )
  }

  if (!is.null(polygon_layer)) {
    if (identical(plot_last, "points")) {
      result$spec$layers <- c(list(polygon_layer), result$spec$layers)
    } else {
      result$spec$layers <- c(result$spec$layers, list(polygon_layer))
    }
  }

  rDeckgl::deckgl(
    spec = result$spec,
    data = list(features = result$data),
    con = .extract_duckdb_connection(gobject)
  )
}


#' Internal: In situ points using Mosaic for GiottoDB
#'
#' @keywords internal
#' @noRd
.spatInSituPlotPoints_mosaic <- function(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  feats = NULL,
  sdimx = "x",
  sdimy = "y",
  expand_counts = FALSE,
  count_info_column = "count",
  jitter = c(0, 0),
  point_size = 1.5,
  point_alpha = 1,
  feats_color_code = NULL,
  color_as_factor = TRUE,
  show_polygon = TRUE,
  polygon_feat_type = "cell",
  polygon_color = "grey",
  polygon_bg_color = "black",
  polygon_fill = NULL,
  polygon_fill_gradient = NULL,
  polygon_fill_gradient_midpoint = NULL,
  polygon_fill_gradient_style = c("divergent", "sequential"),
  polygon_fill_as_factor = NULL,
  polygon_fill_code = NULL,
  polygon_alpha = NULL,
  polygon_line_size = 0.4,
  plot_last = c("polygons", "points"),
  title = NULL,
  ...
) {
  if (!requireNamespace("rMosaic", quietly = TRUE)) {
    stop(
      "Package 'rMosaic' is required for plot_method = 'mosaic'. ",
      "Please install it from dbVisuals package."
    )
  }

  if (!inherits(gobject, "GiottoDB")) {
    stop(
      "spatInSituPlotPoints for GiottoDB only accepts GiottoDB objects. ",
      "Use as_giottodb() to convert a giotto object to GiottoDB."
    )
  }

  polygon_fill_gradient_style <- match.arg(polygon_fill_gradient_style)
  plot_last <- match.arg(plot_last)

  data_info <- .fetch_in_situ_data(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    feats = feats,
    sdimx = sdimx,
    sdimy = sdimy
  )

  combined_data <- .prepare_in_situ_points(
    data = data_info$data,
    feature_col = data_info$feature_col,
    sdimx = data_info$sdimx,
    sdimy = data_info$sdimy,
    expand_counts = expand_counts,
    count_info_column = count_info_column,
    jitter = jitter
  )
  feature_col <- data_info$feature_col
  sdimx <- data_info$sdimx
  sdimy <- data_info$sdimy

  if (nrow(combined_data) == 0) {
    stop("No in situ feature coordinates available for plotting.")
  }

  if (isTRUE(show_polygon)) {
    warning(
      "Polygon overlay for Mosaic backend is not yet implemented; rendering points only."
    )
  }

  spec_result <- .generate_mosaic_spec(
    combined_data = combined_data,
    sdimx = sdimx,
    sdimy = sdimy,
    cell_color = feature_col,
    point_size = point_size,
    point_alpha = point_alpha,
    title = title,
    data_name = "features"
  )

  # Clean data for Mosaic backend (simple columns only)
  cols_needed <- c(sdimx, sdimy, feature_col)
  cols_needed <- cols_needed[cols_needed %in% colnames(spec_result$data)]
  data_clean <- spec_result$data[, cols_needed, drop = FALSE]
  data_clean <- as.data.frame(data_clean)

  for (col in colnames(data_clean)) {
    if (is.factor(data_clean[[col]])) {
      data_clean[[col]] <- as.character(data_clean[[col]])
    } else if (is.list(data_clean[[col]])) {
      data_clean[[col]] <- as.character(data_clean[[col]])
    } else if (!is.numeric(data_clean[[col]]) && !is.character(data_clean[[col]])) {
      data_clean[[col]] <- as.character(data_clean[[col]])
    }
    if (is.numeric(data_clean[[col]])) {
      data_clean[[col]] <- as.numeric(data_clean[[col]])
    }
  }

  rMosaic::mosaic(
    spec = spec_result$spec,
    data = list(features = data_clean),
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
  point_size = 3,
  point_alpha = 1,
  title = NULL,
  data_name = "cells"
) {
  sdimx <- as.character(sdimx)
  sdimy <- as.character(sdimy)
  if (!all(c(sdimx, sdimy) %in% colnames(combined_data))) {
    stop(
      "Required coordinate columns not found: ",
      paste(setdiff(c(sdimx, sdimy), colnames(combined_data)), collapse = ", ")
    )
  }

  coord_complete <- stats::complete.cases(combined_data[, c(sdimx, sdimy)])
  combined_data <- combined_data[coord_complete, , drop = FALSE]
  if (nrow(combined_data) == 0) {
    stop("No non-missing coordinates available for plotting.")
  }

  # For Mosaic, we need to reverse the Y-axis scale, not transform coordinates
  # Mosaic uses standard coordinate system where Y increases upward
  # Spatial data typically has Y increasing downward (top-left origin)

  plot_spec <- list(
    mark = "dot",
    data = list(from = data_name),
    x = sdimx, # Simple string, not list(field=..., type=...)
    y = sdimy,
    r = point_size,
    opacity = point_alpha
  )

  plot_spec$fill <- if (!is.null(cell_color) && cell_color %in% colnames(combined_data)) {
    cell_color
  } else {
    "steelblue"
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
      combined_data = combined_data,
      sdimx = sdimx,
      sdimy = sdimy,
      cell_color = cell_color,
      point_size = point_size,
      point_alpha = point_alpha,
      title = title
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
