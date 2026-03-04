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

`%||%` <- function(x, y) if (is.null(x)) y else x


#' Fetch in situ feature coordinates for visualization
#'
#' @param gobject GiottoDB object
#' @param spat_unit Spatial unit (defaults to active)
#' @param feat_type Feature type (defaults to active)
#' @param feats Optional vector of feature IDs to keep
#' @param sdimx Name of x column (default "x" for in situ data)
#' @param sdimy Name of y column (default "y" for in situ data)
#' @keywords internal
#' @return list with `data` (data.frame) and `feature_col` (column used for coloring)
.fetch_in_situ_data <- function(
  gobject,
  spat_unit = NULL,
  feat_type = NULL,
  feats = NULL,
  sdimx = "x",
  sdimy = "y"
) {
  if (is.null(spat_unit)) {
    spat_unit <- GiottoClass::activeSpatUnit(gobject)
  }
  if (is.null(feat_type)) {
    feat_type <- GiottoClass::activeFeatType(gobject)
  }

  feat_info <- GiottoClass::getFeatureInfo(
    gobject = gobject,
    feat_type = feat_type,
    return_giottoPoints = TRUE,
    set_defaults = TRUE,
    simplify = FALSE
  )

  # get the giottoPoints object (can be a single object or nested list)
  points_obj <- NULL
  if (methods::is(feat_info, "giottoPoints")) {
    points_obj <- feat_info
  } else if (is.list(feat_info)) {
    if (!is.null(spat_unit) && spat_unit %in% names(feat_info)) {
      points_obj <- feat_info[[spat_unit]]
    } else if (length(feat_info) > 0) {
      points_obj <- feat_info[[1]]
    }
  }

  if (is.null(points_obj)) {
    stop("No in situ feature coordinates found for the provided GiottoDB object.")
  }

  # Extract coordinates from giottoPoints
  if (!is.null(points_obj@spatVector)) {
    data <- as.data.frame(points_obj@spatVector)

    # terra::crds returns coordinate columns for SpatVector points
    coord_df <- tryCatch(
      terra::crds(points_obj@spatVector, df = TRUE),
      error = function(e) NULL
    )

    if (is.null(coord_df)) {
      coord_df <- tryCatch(
        {
          gm <- terra::geom(points_obj@spatVector)
          if (!is.null(gm) && ncol(gm) >= 2) gm[, c("x", "y"), drop = FALSE] else NULL
        },
        error = function(e) NULL
      )
    }

    if (!is.null(coord_df)) {
      coord_names <- tolower(colnames(coord_df))
      # Rename first two columns to x/y if needed
      if (!"x" %in% coord_names && ncol(coord_df) >= 1) {
        colnames(coord_df)[1] <- "x"
      }
      if (!"y" %in% coord_names && ncol(coord_df) >= 2) {
        colnames(coord_df)[2] <- "y"
      }
      # Only add if not already present
      for (nm in c("x", "y", "sdimx", "sdimy")) {
        if (!nm %in% colnames(data) && nm %in% colnames(coord_df)) {
          data[[nm]] <- coord_df[[nm]]
        }
      }
    }
  } else if (!is.null(points_obj@coordinates)) {
    data <- as.data.frame(points_obj@coordinates)
  } else {
    stop("Unable to extract coordinates from giottoPoints object.")
  }

  # Best-effort detection of feature column
  feature_col <- intersect(
    c("feat_ID", "gene_ID", "feature", "feat", "name"),
    colnames(data)
  )
  feature_col <- if (length(feature_col) > 0) feature_col[[1]] else NULL

  if (is.null(feature_col)) {
    stop("Could not determine feature column in in situ data.")
  }

  # Filter by feats if provided
  if (!is.null(feats)) {
    feats_vec <- feats
    if (is.list(feats_vec)) {
      feats_vec <- unlist(feats_vec, use.names = FALSE)
    }
    data <- data[data[[feature_col]] %in% feats_vec, , drop = FALSE]
  }

  # Resolve coordinate column names
  if (!sdimx %in% colnames(data)) {
    if ("sdimx" %in% colnames(data)) {
      sdimx <- "sdimx"
    } else if ("x" %in% colnames(data)) {
      sdimx <- "x"
    }
  }

  if (!sdimy %in% colnames(data)) {
    if ("sdimy" %in% colnames(data)) {
      sdimy <- "sdimy"
    } else if ("y" %in% colnames(data)) {
      sdimy <- "y"
    }
  }

  if (!sdimx %in% colnames(data) || !sdimy %in% colnames(data)) {
    stop("Spatial coordinate columns could not be identified in in situ data.")
  }

  return(list(
    data = data,
    feature_col = feature_col,
    sdimx = sdimx,
    sdimy = sdimy
  ))
}


#' Expand and jitter in situ feature coordinates
#'
#' @param data data.frame returned by `.fetch_in_situ_data()`
#' @param feature_col column name holding feature labels
#' @param sdimx x coordinate column
#' @param sdimy y coordinate column
#' @param expand_counts logical; replicate rows using `count_info_column`
#' @param count_info_column column that stores counts
#' @param jitter numeric of length 1 or 2; maximum uniform jitter to apply
#' @keywords internal
.prepare_in_situ_points <- function(
  data,
  feature_col,
  sdimx,
  sdimy,
  expand_counts = FALSE,
  count_info_column = "count",
  jitter = c(0, 0)
) {
  out <- data

  if (isTRUE(expand_counts)) {
    has_count_col <- !is.null(count_info_column) &&
      count_info_column %in% colnames(out)

    if (!has_count_col) {
      warning(
        "expand_counts=TRUE but count column '",
        count_info_column,
        "' is not present; skipping expansion."
      )
    } else {
      counts <- out[[count_info_column]]
      counts[is.na(counts)] <- 0
      counts[counts < 0] <- 0
      counts_int <- suppressWarnings(as.integer(round(counts)))

      idx <- rep(seq_len(nrow(out)), times = pmax(counts_int, 1L))
      out <- out[idx, , drop = FALSE]
    }
  }

  # Apply jitter if requested
  jitter <- as.numeric(jitter %||% c(0, 0))
  if (length(jitter) == 1) jitter <- rep(jitter, 2)
  if (any(jitter != 0)) {
    out[[sdimx]] <- out[[sdimx]] + stats::runif(nrow(out), -jitter[[1]], jitter[[1]])
    out[[sdimy]] <- out[[sdimy]] + stats::runif(nrow(out), -jitter[[2]], jitter[[2]])
  }

  # Ensure coordinates are numeric
  out[[sdimx]] <- as.numeric(out[[sdimx]])
  out[[sdimy]] <- as.numeric(out[[sdimy]])

  # Standardize feature column to character for plotting and palette generation
  if (!is.null(feature_col) && feature_col %in% colnames(out)) {
    if (is.factor(out[[feature_col]])) {
      out[[feature_col]] <- as.character(out[[feature_col]])
    }
  }

  out
}


#' Fetch dimension reduction coordinates for visualization
#'
#' @param gobject GiottoDB or giotto object
#' @param method Name of the dimred method (e.g. "umap", "pca")
#' @param dim_reduction_name Optional name override
#' @param dim1_to_use Dimension to use on x-axis
#' @param dim2_to_use Dimension to use on y-axis
#' @param spat_unit Spatial unit
#' @param feat_type Feature type
#' @param cell_color Metadata column to join for coloring
#' @keywords internal
.fetch_dimred_data <- function(
  gobject,
  method = c("umap", "pca"),
  dim_reduction_name = NULL,
  dim1_to_use = 1,
  dim2_to_use = 2,
  spat_unit = NULL,
  feat_type = NULL,
  cell_color = NULL
) {
  method <- match.arg(method)

  if (is.null(spat_unit)) {
    spat_unit <- GiottoClass::activeSpatUnit(gobject)
  }
  if (is.null(feat_type)) {
    feat_type <- GiottoClass::activeFeatType(gobject)
  }

  dim_name <- dim_reduction_name %||% method

  args <- list(
    gobject = gobject,
    spat_unit = spat_unit,
    feat_type = feat_type,
    reduction = "cells",
    reduction_method = method,
    name = dim_name,
    output = "data.table",
    set_defaults = FALSE
  )

  get_fun_if_exists <- function(pkg, fun) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      return(NULL)
    }
    ns <- asNamespace(pkg)
    if (exists(fun, envir = ns, inherits = FALSE)) {
      get(fun, envir = ns)
    } else {
      NULL
    }
  }

  candidate_funs <- list(
    get_fun_if_exists("GiottoClass", "getDimReduction"),
    get_fun_if_exists("GiottoClass", "get_dimReduction"),
    get_fun_if_exists("Giotto", "getDimReduction"),
    get_fun_if_exists("Giotto", "get_dimReduction")
  )
  candidate_funs <- Filter(Negate(is.null), candidate_funs)

  dim_dt <- NULL
  last_error <- NULL
  for (fn in candidate_funs) {
    dim_dt <- tryCatch(
      {
        do.call(fn, args)
      },
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    if (!is.null(dim_dt)) {
      break
    }
  }

  if (is.null(dim_dt)) {
    stop(
      "Could not retrieve dimension reduction '",
      dim_name,
      "' for method '",
      method,
      "'. ",
      if (!is.null(last_error)) last_error$message,
      " Use Giotto::showGiottoDimRed(gobject) to inspect available reductions."
    )
  }

  dim_df <- as.data.frame(dim_dt)
  if (nrow(dim_df) == 0) {
    stop(
      "Dimension reduction '", dim_name, "' for method '", method,
      "' returned no coordinates."
    )
  }

  # Resolve coordinate columns with flexible pattern matching
  detect_dim_cols <- function(df_cols) {
    # ordered patterns to try
    patterns <- c(
      "^Dim[._]?(\\d+)$",   # Dim.1 / Dim1
      "^dim[._]?(\\d+)$",   # dim1
      "^UMAP[._]?(\\d+)$",  # UMAP_1 / UMAP1
      "^umap[._]?(\\d+)$",
      "^PC[._]?(\\d+)$",    # PC1 / PC_1
      "^PCA[._]?(\\d+)$",
      "^(\\d+)$"            # bare numbers
    )

    matches <- list()
    for (pat in patterns) {
      hit <- grep(pat, df_cols, value = TRUE, ignore.case = FALSE)
      if (length(hit) > 0) {
        idx <- suppressWarnings(as.integer(gsub(pat, "\\1", hit)))
        ord <- order(idx, na.last = TRUE)
        matches <- c(matches, hit[ord])
      }
    }
    unique(matches)
  }

  dim_cols <- detect_dim_cols(colnames(dim_df))

  # Fallback: any numeric columns (after removing obvious non-dim columns)
  if (length(dim_cols) == 0) {
    numeric_cols <- names(dim_df)[vapply(dim_df, is.numeric, logical(1))]
    dim_cols <- setdiff(numeric_cols, c("x", "y", "sdimx", "sdimy"))
  }

  needed_dims <- max(dim1_to_use, dim2_to_use)
  if (length(dim_cols) < needed_dims) {
    stop(
      "Not enough dimension columns found in reduction '",
      dim_name,
      "'. Available columns: ",
      paste(colnames(dim_df), collapse = ", ")
    )
  }

  dimx <- dim_cols[[dim1_to_use]]
  dimy <- dim_cols[[dim2_to_use]]

  # Ensure selected columns exist and create sanitized aliases for plotting
  if (!dimx %in% colnames(dim_df) || !dimy %in% colnames(dim_df)) {
    stop(
      "Dimension reduction '", dim_name, "' is missing requested dimensions. ",
      "Columns found: ", paste(colnames(dim_df), collapse = ", ")
    )
  }

  # Pull cell metadata for coloring if requested
  if (!is.null(cell_color)) {
    cell_meta <- GiottoClass::getCellMetadata(
      gobject,
      spat_unit = spat_unit,
      feat_type = feat_type,
      output = "data.table",
      copy_obj = TRUE
    )
    cell_meta <- as.data.frame(cell_meta)
    if ("cell_ID" %in% colnames(dim_df) && "cell_ID" %in% colnames(cell_meta)) {
      dim_df <- merge(dim_df, cell_meta, by = "cell_ID", all.x = TRUE)
    } else {
      dim_df <- cbind(dim_df, cell_meta)
    }
  }

  sanitize_dim_name <- function(name, suffix) {
    if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", name)) {
      new_name <- paste0("dim_", suffix)
      counter <- 1
      candidate <- new_name
      while (candidate %in% colnames(dim_df)) {
        counter <- counter + 1
        candidate <- paste0(new_name, "_", counter)
      }
      dim_df[[candidate]] <- dim_df[[name]]
      candidate
    } else {
      name
    }
  }

  sdimx_plot <- sanitize_dim_name(dimx, dim1_to_use)
  sdimy_plot <- sanitize_dim_name(dimy, dim2_to_use)

  # Always ensure the sanitized columns exist for plotting
  if (!sdimx_plot %in% colnames(dim_df)) {
    dim_df[[sdimx_plot]] <- dim_df[[dimx]]
  }
  if (!sdimy_plot %in% colnames(dim_df)) {
    dim_df[[sdimy_plot]] <- dim_df[[dimy]]
  }
  # Ensure coordinates are numeric for plotting backends
  dim_df[[sdimx_plot]] <- as.numeric(dim_df[[sdimx_plot]])
  dim_df[[sdimy_plot]] <- as.numeric(dim_df[[sdimy_plot]])

  list(
    data = dim_df,
    sdimx = sdimx_plot,
    sdimy = sdimy_plot
  )
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

#' Convert values to CSS color strings for Mosaic identity color scales
#'
#' @param values Vector of values to color
#' @param color_as_factor Treat values as discrete categories
#' @param cell_color_code Optional named or unnamed color vector
#' @param cell_color_gradient Optional gradient vector for continuous values
#' @param default_color Fallback CSS color for missing values
#' @keywords internal
#' @return Character vector of CSS color strings
.mosaic_color_values <- function(
  values,
  color_as_factor = TRUE,
  cell_color_code = NULL,
  cell_color_gradient = NULL,
  default_color = "#808080"
) {
  if (length(values) == 0) {
    return(character())
  }

  if (color_as_factor || is.character(values) || is.factor(values)) {
    palette_map <- .generate_color_palette(
      values,
      color_as_factor = TRUE,
      cell_color_code = cell_color_code,
      cell_color_gradient = cell_color_gradient
    )
    keys <- as.character(values)
    colors <- unname(palette_map[keys])
    colors[is.na(colors) | !nzchar(colors)] <- default_color
    return(colors)
  }

  palette <- .generate_color_palette(
    values,
    color_as_factor = FALSE,
    cell_color_code = cell_color_code,
    cell_color_gradient = cell_color_gradient
  )
  if (length(palette) < 2) {
    palette <- rep(palette, 2L)
  }

  value_range <- range(values, na.rm = TRUE)
  if (any(!is.finite(value_range)) || diff(value_range) == 0) {
    value_range <- value_range + c(-0.5, 0.5)
  }

  color_ramp <- grDevices::colorRamp(palette, space = "Lab")
  rgb_vals <- matrix(
    rep.int(128L, length(values) * 3L),
    ncol = 3L
  )

  valid_idx <- which(!is.na(values) & is.finite(values))
  if (length(valid_idx) > 0) {
    scaled_vals <- (values[valid_idx] - value_range[1]) /
      (value_range[2] - value_range[1])
    scaled_vals <- pmin(pmax(scaled_vals, 0), 1)
    rgb_vals[valid_idx, ] <- color_ramp(scaled_vals)
  }

  grDevices::rgb(
    red = as.integer(round(rgb_vals[, 1])),
    green = as.integer(round(rgb_vals[, 2])),
    blue = as.integer(round(rgb_vals[, 3])),
    maxColorValue = 255
  )
}


#' Darken CSS color strings
#'
#' @param colors Character vector of CSS colors
#' @param factor Multiplicative darkening factor in [0, 1]
#' @keywords internal
#' @return Character vector of darkened CSS colors
.darken_mosaic_colors <- function(colors, factor = 0.7) {
  if (length(colors) == 0) {
    return(character())
  }

  factor <- max(0, min(1, factor))
  rgb_mat <- grDevices::col2rgb(colors)
  grDevices::rgb(
    red = as.integer(round(rgb_mat[1, ] * factor)),
    green = as.integer(round(rgb_mat[2, ] * factor)),
    blue = as.integer(round(rgb_mat[3, ] * factor)),
    maxColorValue = 255
  )
}


#' Prepare rows for Mosaic inline JSON data
#'
#' @param data A data.frame to serialize into row objects
#' @keywords internal
#' @return List of row-wise named lists
.as_mosaic_rows <- function(data) {
  data <- as.data.frame(data)

  for (col in colnames(data)) {
    if (is.factor(data[[col]])) {
      data[[col]] <- as.character(data[[col]])
    } else if (is.list(data[[col]])) {
      data[[col]] <- vapply(data[[col]], as.character, character(1))
    } else if (!is.numeric(data[[col]]) && !is.character(data[[col]]) &&
      !is.logical(data[[col]])) {
      data[[col]] <- as.character(data[[col]])
    }

    if (is.numeric(data[[col]])) {
      data[[col]] <- as.numeric(data[[col]])
    }
  }

  lapply(seq_len(nrow(data)), function(i) {
    as.list(data[i, , drop = FALSE])
  })
}


#' Convert WKB raw payloads to uppercase hex strings
#'
#' @param value A raw vector, blob-like object, or scalar value
#' @keywords internal
#' @return A scalar character hex string or `NA_character_`
.raw_to_hex_string <- function(value) {
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }

  if (is.list(value) && length(value) == 1L) {
    value <- value[[1]]
  }

  if (inherits(value, "blob")) {
    value <- unclass(value)
    if (is.list(value) && length(value) == 1L) {
      value <- value[[1]]
    }
  }

  if (is.raw(value)) {
    return(paste0(toupper(format(value)), collapse = ""))
  }

  if (is.numeric(value)) {
    ints <- suppressWarnings(as.integer(value))
    if (all(!is.na(ints)) && all(ints >= 0L & ints <= 255L)) {
      return(paste0(sprintf("%02X", ints), collapse = ""))
    }
  }

  if (is.character(value) && length(value) == 1L) {
    candidate <- trimws(value)
    candidate <- sub("^\\\\x", "", candidate, perl = TRUE)
    if (nzchar(candidate) && grepl("^[0-9A-Fa-f]+$", candidate)) {
      return(toupper(candidate))
    }
  }

  NA_character_
}


#' Build a lightweight GeoJSON bbox polygon for Mosaic projection fitting
#'
#' @param xmin Minimum x
#' @param ymin Minimum y
#' @param xmax Maximum x
#' @param ymax Maximum y
#' @keywords internal
#' @return A GeoJSON-like polygon list or `NULL`
.mosaic_bbox_domain <- function(xmin, ymin, xmax, ymax) {
  vals <- c(xmin, ymin, xmax, ymax)
  if (!all(is.finite(vals)) || xmin >= xmax || ymin >= ymax) {
    return(NULL)
  }

  list(
    type = "Polygon",
    coordinates = list(list(
      c(xmin, ymin),
      c(xmax, ymin),
      c(xmax, ymax),
      c(xmin, ymax),
      c(xmin, ymin)
    ))
  )
}
