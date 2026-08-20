##----------------------------------------------------------------------------##
## Portable Linked views configuration contract.
##
## This file is sourced by the generated Viewer at runtime. Keep it independent
## of the CerebroNexus package namespace: base R, tools, and jsonlite only.
##----------------------------------------------------------------------------##

CV_CONFIG_SCHEMA <- "cerebronexus-linked-view"
CV_CONFIG_VERSION <- 1L
CV_CONFIG_MAX_BYTES <- 5L * 1024L * 1024L
CV_CONFIG_MAX_DEPTH <- 10L
CV_CONFIG_MAX_NODES <- 300000L

cv_config_abort <- function(code, message) {
  stop(structure(
    list(message = message, call = NULL, code = code),
    class = c("cv_config_error", "error", "condition")
  ))
}

cv_config_path <- function(path, field) {
  if (identical(path, "$")) paste0("$.", field) else paste0(path, ".", field)
}

cv_config_record <- function(value, allowed, required = allowed, path = "$") {
  if (!is.list(value) || (length(value) && is.null(names(value)))) {
    cv_config_abort("invalid_type", paste0(path, " must be an object."))
  }
  keys <- names(value)
  if (is.null(keys)) {
    keys <- character()
  }
  if (any(!nzchar(keys)) || anyDuplicated(keys)) {
    cv_config_abort("invalid_object", paste0(path, " has invalid field names."))
  }
  unknown <- setdiff(keys, allowed)
  if (length(unknown)) {
    cv_config_abort(
      "unknown_field",
      paste0(cv_config_path(path, unknown[[1L]]), " is not supported.")
    )
  }
  missing <- setdiff(required, keys)
  if (length(missing)) {
    cv_config_abort(
      "missing_field",
      paste0(cv_config_path(path, missing[[1L]]), " is required.")
    )
  }
  value
}

cv_config_string <- function(
  value,
  path,
  max_bytes = 256L,
  allow_empty = FALSE
) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    cv_config_abort("invalid_type", paste0(path, " must be a string."))
  }
  value <- enc2utf8(value)
  if (!allow_empty && !nzchar(value)) {
    cv_config_abort("invalid_value", paste0(path, " must not be empty."))
  }
  if (nchar(value, type = "bytes") > max_bytes) {
    cv_config_abort("string_too_long", paste0(path, " is too long."))
  }
  value
}

cv_config_nullable_string <- function(value, path, max_bytes = 256L) {
  if (is.null(value)) {
    return(NULL)
  }
  cv_config_string(value, path, max_bytes = max_bytes)
}

cv_config_number <- function(value, path, min = -Inf, max = Inf) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value)
  ) {
    cv_config_abort("invalid_type", paste0(path, " must be a finite number."))
  }
  value <- as.numeric(value)
  if (value < min || value > max) {
    cv_config_abort(
      "out_of_range",
      paste0(path, " is outside the supported range.")
    )
  }
  value
}

cv_config_integer <- function(
  value,
  path,
  min = 0L,
  max = .Machine$integer.max
) {
  number <- cv_config_number(value, path, min = min, max = max)
  if (number != floor(number)) {
    cv_config_abort("invalid_type", paste0(path, " must be an integer."))
  }
  as.integer(number)
}

cv_config_logical <- function(value, path) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    cv_config_abort("invalid_type", paste0(path, " must be true or false."))
  }
  isTRUE(value)
}

cv_config_array_values <- function(value, path) {
  if (is.null(value)) {
    return(list())
  }
  if (is.atomic(value)) {
    return(as.list(value))
  }
  if (!is.list(value) || (!is.null(names(value)) && length(value))) {
    cv_config_abort("invalid_type", paste0(path, " must be an array."))
  }
  value
}

cv_config_check_node_limit <- function(value) {
  nodes <- 0
  walk <- function(item, depth = 1L) {
    if (depth > CV_CONFIG_MAX_DEPTH) {
      cv_config_abort("too_deep", "The configuration is nested too deeply.")
    }
    nodes <<- nodes + if (is.list(item)) 1L else max(1L, length(item))
    if (nodes > CV_CONFIG_MAX_NODES) {
      cv_config_abort("too_complex", "The configuration has too many values.")
    }
    if (is.list(item)) {
      for (child in item) {
        walk(child, depth + 1L)
      }
    }
    invisible(NULL)
  }
  walk(value)
  invisible(value)
}

cv_config_string_array <- function(
  value,
  path,
  max_items = Inf,
  max_bytes = 256L
) {
  items <- cv_config_array_values(value, path)
  if (length(items) > max_items) {
    cv_config_abort("too_many_items", paste0(path, " has too many items."))
  }
  out <- vapply(
    seq_along(items),
    function(index) {
      cv_config_string(
        items[[index]],
        paste0(path, "[", index, "]"),
        max_bytes = max_bytes
      )
    },
    character(1)
  )
  if (anyDuplicated(out)) {
    cv_config_abort(
      "duplicate_item",
      paste0(path, " must contain unique values.")
    )
  }
  unname(out)
}

cv_config_choice <- function(value, path, choices) {
  value <- cv_config_string(value, path)
  if (!value %in% choices) {
    cv_config_abort("invalid_value", paste0(path, " is not supported."))
  }
  value
}

cv_config_normalize_polygon <- function(value, path) {
  points <- cv_config_array_values(value, path)
  if (length(points) < 3L) {
    cv_config_abort(
      "too_few_items",
      paste0(path, " must have at least 3 points.")
    )
  }
  if (length(points) > 10000L) {
    cv_config_abort("too_many_items", paste0(path, " has too many points."))
  }
  lapply(seq_along(points), function(index) {
    point_path <- paste0(path, "[", index, "]")
    coordinates <- cv_config_array_values(points[[index]], point_path)
    if (length(coordinates) != 2L) {
      cv_config_abort(
        "invalid_type",
        paste0(point_path, " must be an [x, y] pair.")
      )
    }
    c(
      cv_config_number(coordinates[[1L]], paste0(point_path, "[1]"), -1e9, 1e9),
      cv_config_number(coordinates[[2L]], paste0(point_path, "[2]"), -1e9, 1e9)
    )
  })
}

cv_config_require_json_array <- function(value, path) {
  if (!is.list(value) || !is.null(names(value))) {
    cv_config_abort("invalid_type", paste0(path, " must be an array."))
  }
  invisible(value)
}

cv_config_check_json_array_shapes <- function(config) {
  if (!is.list(config) || is.null(names(config))) {
    return(invisible(config))
  }
  selection <- config$selection
  view <- config$view
  if (is.list(selection) && !is.null(names(selection))) {
    cv_config_require_json_array(selection$cells, "$.selection.cells")
    geometry <- selection$geometry
    if (is.list(geometry) && !is.null(names(geometry))) {
      cv_config_require_json_array(
        geometry$polygon,
        "$.selection.geometry.polygon"
      )
      if (is.list(geometry$polygon) && is.null(names(geometry$polygon))) {
        for (index in seq_along(geometry$polygon)) {
          cv_config_require_json_array(
            geometry$polygon[[index]],
            paste0("$.selection.geometry.polygon[", index, "]")
          )
        }
      }
    }
  }
  if (!is.list(view) || is.null(names(view))) {
    return(invisible(config))
  }
  colour <- view$colour
  if (is.list(colour) && !is.null(names(colour))) {
    cv_config_require_json_array(
      colour$rgb_genes,
      "$.view.colour.rgb_genes"
    )
  }
  cv_config_require_json_array(view$projections, "$.view.projections")
  cv_config_require_json_array(
    view$spatial_sections,
    "$.view.spatial_sections"
  )
  cv_config_require_json_array(view$hidden_levels, "$.view.hidden_levels")
  cv_config_require_json_array(view$lenses, "$.view.lenses")
  cv_config_require_json_array(
    view$spatial_backgrounds,
    "$.view.spatial_backgrounds"
  )
  if (is.list(view$filters) && !is.null(names(view$filters))) {
    for (name in names(view$filters)) {
      cv_config_require_json_array(
        view$filters[[name]],
        cv_config_path("$.view.filters", name)
      )
    }
  }
  if (is.list(view$hidden_levels) && is.null(names(view$hidden_levels))) {
    for (index in seq_along(view$hidden_levels)) {
      record <- view$hidden_levels[[index]]
      if (is.list(record) && !is.null(names(record))) {
        cv_config_require_json_array(
          record$levels,
          paste0("$.view.hidden_levels[", index, "].levels")
        )
      }
    }
  }
  invisible(config)
}

cv_config_check_json_size <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    cv_config_abort("invalid_json", "The configuration must be JSON text.")
  }
  if (nchar(text, type = "bytes") > CV_CONFIG_MAX_BYTES) {
    cv_config_abort("too_large", "The configuration is larger than 5 MiB.")
  }
  invisible(TRUE)
}

cv_config_cell_fingerprint <- function(cells) {
  if (
    !is.character(cells) ||
      anyNA(cells) ||
      any(!nzchar(cells)) ||
      anyDuplicated(cells)
  ) {
    cv_config_abort(
      "invalid_dataset",
      "The current data set must have unique, non-empty cell barcodes."
    )
  }
  cells <- sort(enc2utf8(cells), method = "radix")
  stream <- paste0(nchar(cells, type = "bytes"), ":", cells, collapse = "")
  path <- tempfile("coordviews-fingerprint-")
  on.exit(unlink(path), add = TRUE)
  writeBin(charToRaw(stream), path)
  paste0("md5-cell-set-v1:", unname(tools::md5sum(path)))
}

cv_config_normalize_viewport <- function(value, path) {
  value <- cv_config_record(value, c("cx", "cy", "span"), path = path)
  list(
    cx = cv_config_number(value$cx, cv_config_path(path, "cx"), -1e9, 1e9),
    cy = cv_config_number(value$cy, cv_config_path(path, "cy"), -1e9, 1e9),
    span = cv_config_number(value$span, cv_config_path(path, "span"), 1e-6, 1e6)
  )
}

cv_config_normalize_rotation <- function(value, path) {
  if (is.null(value)) {
    return(NULL)
  }
  value <- cv_config_record(value, c("rx", "ry"), path = path)
  list(
    rx = cv_config_number(value$rx, cv_config_path(path, "rx"), -1e6, 1e6),
    ry = cv_config_number(value$ry, cv_config_path(path, "ry"), -1e6, 1e6)
  )
}

cv_config_normalize_lenses <- function(value, path) {
  items <- cv_config_array_values(value, path)
  if (length(items) > 512L) {
    cv_config_abort("too_many_items", paste0(path, " has too many items."))
  }
  out <- lapply(seq_along(items), function(index) {
    item_path <- paste0(path, "[", index, "]")
    item <- cv_config_record(
      items[[index]],
      c("space", "viewport", "rotation"),
      path = item_path
    )
    list(
      space = cv_config_string(item$space, cv_config_path(item_path, "space")),
      viewport = cv_config_normalize_viewport(
        item$viewport,
        cv_config_path(item_path, "viewport")
      ),
      rotation = cv_config_normalize_rotation(
        item$rotation,
        cv_config_path(item_path, "rotation")
      )
    )
  })
  spaces <- vapply(out, `[[`, character(1), "space")
  if (anyDuplicated(spaces)) {
    cv_config_abort(
      "duplicate_item",
      paste0(path, " has duplicate lens identities.")
    )
  }
  out
}

cv_config_normalize_filters <- function(value, path) {
  if (!is.list(value) || is.null(names(value))) {
    cv_config_abort("invalid_type", paste0(path, " must be an object."))
  }
  if (!length(value)) {
    return(structure(list(), names = character()))
  }
  keys <- names(value)
  if (length(keys) > 256L) {
    cv_config_abort("too_many_items", paste0(path, " has too many fields."))
  }
  if (any(!nzchar(keys)) || anyDuplicated(keys)) {
    cv_config_abort("invalid_object", paste0(path, " has invalid field names."))
  }
  lapply_keys <- lapply(seq_along(keys), function(index) {
    key <- cv_config_string(keys[[index]], paste0(path, " field name"))
    cv_config_string_array(
      value[[index]],
      cv_config_path(path, key),
      max_items = 65536L
    )
  })
  names(lapply_keys) <- keys
  lapply_keys
}

cv_config_normalize_hidden_levels <- function(value, path) {
  items <- cv_config_array_values(value, path)
  if (length(items) > 256L) {
    cv_config_abort("too_many_items", paste0(path, " has too many items."))
  }
  out <- lapply(seq_along(items), function(index) {
    item_path <- paste0(path, "[", index, "]")
    item <- cv_config_record(
      items[[index]],
      c("group", "levels"),
      path = item_path
    )
    list(
      group = cv_config_string(item$group, cv_config_path(item_path, "group")),
      levels = cv_config_string_array(
        item$levels,
        cv_config_path(item_path, "levels"),
        max_items = 65536L
      )
    )
  })
  groups <- vapply(out, `[[`, character(1), "group")
  if (anyDuplicated(groups)) {
    cv_config_abort("duplicate_item", paste0(path, " has duplicate groups."))
  }
  out
}

cv_config_normalize_alignment <- function(value, path) {
  value <- cv_config_record(
    value,
    c(
      "offset_x",
      "offset_y",
      "scale_x",
      "scale_y",
      "rotation",
      "lock_aspect",
      "flip_x",
      "flip_y",
      "show"
    ),
    path = path
  )
  list(
    offset_x = cv_config_number(
      value$offset_x,
      cv_config_path(path, "offset_x"),
      -1e12,
      1e12
    ),
    offset_y = cv_config_number(
      value$offset_y,
      cv_config_path(path, "offset_y"),
      -1e12,
      1e12
    ),
    scale_x = cv_config_number(
      value$scale_x,
      cv_config_path(path, "scale_x"),
      1e-6,
      1e6
    ),
    scale_y = cv_config_number(
      value$scale_y,
      cv_config_path(path, "scale_y"),
      1e-6,
      1e6
    ),
    rotation = cv_config_number(
      value$rotation,
      cv_config_path(path, "rotation"),
      -1e6,
      1e6
    ),
    lock_aspect = cv_config_logical(
      value$lock_aspect,
      cv_config_path(path, "lock_aspect")
    ),
    flip_x = cv_config_logical(value$flip_x, cv_config_path(path, "flip_x")),
    flip_y = cv_config_logical(value$flip_y, cv_config_path(path, "flip_y")),
    show = cv_config_logical(value$show, cv_config_path(path, "show"))
  )
}

cv_config_normalize_backgrounds <- function(value, path) {
  items <- cv_config_array_values(value, path)
  if (length(items) > 256L) {
    cv_config_abort("too_many_items", paste0(path, " has too many items."))
  }
  out <- lapply(seq_along(items), function(index) {
    item_path <- paste0(path, "[", index, "]")
    item <- cv_config_record(
      items[[index]],
      c("section", "mode", "image_id", "opacity", "alignment"),
      path = item_path
    )
    list(
      section = cv_config_string(
        item$section,
        cv_config_path(item_path, "section")
      ),
      mode = cv_config_choice(
        item$mode,
        cv_config_path(item_path, "mode"),
        c("auto", "none", "image")
      ),
      image_id = cv_config_nullable_string(
        item$image_id,
        cv_config_path(item_path, "image_id")
      ),
      opacity = cv_config_number(
        item$opacity,
        cv_config_path(item_path, "opacity"),
        0,
        1
      ),
      alignment = cv_config_normalize_alignment(
        item$alignment,
        cv_config_path(item_path, "alignment")
      )
    )
  })
  sections <- vapply(out, `[[`, character(1), "section")
  if (anyDuplicated(sections)) {
    cv_config_abort("duplicate_item", paste0(path, " has duplicate sections."))
  }
  out
}

cv_config_normalize <- function(config, cells) {
  cv_config_check_node_limit(config)
  cells <- enc2utf8(as.character(cells))
  expected_fingerprint <- cv_config_cell_fingerprint(cells)
  config <- cv_config_record(
    config,
    c("schema", "version", "created_at", "dataset", "selection", "view"),
    path = "$"
  )

  schema <- cv_config_string(config$schema, "$.schema")
  if (!identical(schema, CV_CONFIG_SCHEMA)) {
    cv_config_abort(
      "unsupported_schema",
      "This is not a Linked views configuration."
    )
  }
  version <- cv_config_integer(config$version, "$.version", 1L)
  if (!identical(version, CV_CONFIG_VERSION)) {
    cv_config_abort(
      "unsupported_version",
      "This configuration version is not supported."
    )
  }
  created_at <- cv_config_string(config$created_at, "$.created_at", 64L)
  parsed_at <- suppressWarnings(as.POSIXct(
    strptime(created_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ))
  if (
    is.na(parsed_at) ||
      !identical(
        format(parsed_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        created_at
      )
  ) {
    cv_config_abort(
      "invalid_timestamp",
      "$.created_at must be a UTC timestamp."
    )
  }

  dataset <- cv_config_record(
    config$dataset,
    c("cell_count", "cell_fingerprint"),
    path = "$.dataset"
  )
  cell_count <- cv_config_integer(dataset$cell_count, "$.dataset.cell_count")
  cell_fingerprint <- cv_config_string(
    dataset$cell_fingerprint,
    "$.dataset.cell_fingerprint",
    80L
  )
  if (
    cell_count != length(cells) ||
      !identical(cell_fingerprint, expected_fingerprint)
  ) {
    cv_config_abort(
      "dataset_mismatch",
      "This configuration belongs to a different cell population."
    )
  }

  selection <- cv_config_record(
    config$selection,
    c("cells", "source", "geometry"),
    path = "$.selection"
  )
  geometry <- cv_config_record(
    selection$geometry,
    c("space", "mode", "polygon"),
    path = "$.selection.geometry"
  )
  selected_cells <- cv_config_string_array(
    selection$cells,
    "$.selection.cells",
    max_items = length(cells),
    max_bytes = 1024L
  )
  missing_cells <- setdiff(selected_cells, cells)
  if (length(missing_cells)) {
    cv_config_abort(
      "missing_cell",
      "The configuration selects cells that are not in this data set."
    )
  }

  view <- cv_config_record(
    config$view,
    c(
      "colour",
      "projections",
      "spatial_sections",
      "active_spatial",
      "filters",
      "hidden_levels",
      "display",
      "focus_space",
      "lenses",
      "spatial_backgrounds",
      "trekker"
    ),
    path = "$.view"
  )
  colour <- cv_config_record(
    view$colour,
    c("mode", "gene", "rgb_genes", "clip"),
    path = "$.view.colour"
  )
  display <- cv_config_record(
    view$display,
    c(
      "percentage_cells",
      "point_size",
      "point_opacity",
      "group_labels",
      "selection_mode",
      "clone_layout"
    ),
    path = "$.view.display"
  )
  trekker <- cv_config_record(
    view$trekker,
    c("dissolve_percentage", "evidence", "niche_radius"),
    path = "$.view.trekker"
  )

  normalized <- list(
    schema = schema,
    version = version,
    created_at = created_at,
    dataset = list(
      cell_count = cell_count,
      cell_fingerprint = cell_fingerprint
    ),
    selection = list(
      cells = selected_cells,
      source = cv_config_nullable_string(
        selection$source,
        "$.selection.source"
      ),
      geometry = list(
        space = cv_config_string(
          geometry$space,
          "$.selection.geometry.space"
        ),
        mode = cv_config_choice(
          geometry$mode,
          "$.selection.geometry.mode",
          c("lasso", "box")
        ),
        polygon = cv_config_normalize_polygon(
          geometry$polygon,
          "$.selection.geometry.polygon"
        )
      )
    ),
    view = list(
      colour = list(
        mode = cv_config_string(colour$mode, "$.view.colour.mode"),
        gene = cv_config_nullable_string(colour$gene, "$.view.colour.gene"),
        rgb_genes = cv_config_string_array(
          colour$rgb_genes,
          "$.view.colour.rgb_genes",
          max_items = 3L
        ),
        clip = cv_config_number(colour$clip, "$.view.colour.clip", 0, 0.5)
      ),
      projections = cv_config_string_array(
        view$projections,
        "$.view.projections",
        max_items = 64L
      ),
      spatial_sections = cv_config_string_array(
        view$spatial_sections,
        "$.view.spatial_sections",
        max_items = 256L
      ),
      active_spatial = cv_config_nullable_string(
        view$active_spatial,
        "$.view.active_spatial"
      ),
      filters = cv_config_normalize_filters(view$filters, "$.view.filters"),
      hidden_levels = cv_config_normalize_hidden_levels(
        view$hidden_levels,
        "$.view.hidden_levels"
      ),
      display = list(
        percentage_cells = cv_config_number(
          display$percentage_cells,
          "$.view.display.percentage_cells",
          5,
          100
        ),
        point_size = cv_config_number(
          display$point_size,
          "$.view.display.point_size",
          0,
          20
        ),
        point_opacity = cv_config_number(
          display$point_opacity,
          "$.view.display.point_opacity",
          0,
          1
        ),
        group_labels = cv_config_logical(
          display$group_labels,
          "$.view.display.group_labels"
        ),
        selection_mode = cv_config_choice(
          display$selection_mode,
          "$.view.display.selection_mode",
          c("lasso", "box")
        ),
        clone_layout = cv_config_choice(
          display$clone_layout,
          "$.view.display.clone_layout",
          c("stack", "bands")
        )
      ),
      focus_space = cv_config_nullable_string(
        view$focus_space,
        "$.view.focus_space"
      ),
      lenses = cv_config_normalize_lenses(view$lenses, "$.view.lenses"),
      spatial_backgrounds = cv_config_normalize_backgrounds(
        view$spatial_backgrounds,
        "$.view.spatial_backgrounds"
      ),
      trekker = list(
        dissolve_percentage = cv_config_number(
          trekker$dissolve_percentage,
          "$.view.trekker.dissolve_percentage",
          0,
          95
        ),
        evidence = cv_config_logical(
          trekker$evidence,
          "$.view.trekker.evidence"
        ),
        niche_radius = cv_config_number(
          trekker$niche_radius,
          "$.view.trekker.niche_radius",
          50,
          500
        )
      )
    )
  )

  normalized_view <- normalized$view
  if (
    !is.null(normalized_view$active_spatial) &&
      !normalized_view$active_spatial %in% normalized_view$spatial_sections
  ) {
    cv_config_abort(
      "invalid_reference",
      "The active Spatial section is not part of this workspace."
    )
  }
  lens_spaces <- vapply(
    normalized_view$lenses,
    `[[`,
    character(1),
    "space"
  )
  if (
    !is.null(normalized_view$focus_space) &&
      !normalized_view$focus_space %in% lens_spaces
  ) {
    cv_config_abort(
      "invalid_reference",
      "The focused lens is not part of this workspace."
    )
  }
  if (length(normalized_view$spatial_backgrounds)) {
    for (background in normalized_view$spatial_backgrounds) {
      if (!background$section %in% normalized_view$spatial_sections) {
        cv_config_abort(
          "invalid_reference",
          "A Spatial background refers to a section outside this workspace."
        )
      }
      if (identical(background$mode, "image") && is.null(background$image_id)) {
        cv_config_abort(
          "invalid_reference",
          "An image background must identify its image."
        )
      }
    }
  }
  if (
    identical(normalized_view$colour$mode, "__gene__") &&
      is.null(normalized_view$colour$gene)
  ) {
    cv_config_abort(
      "invalid_reference",
      "Gene colour mode must identify a gene."
    )
  }
  if (
    identical(normalized_view$colour$mode, "__rgb__") &&
      length(normalized_view$colour$rgb_genes) != 3L
  ) {
    cv_config_abort(
      "invalid_reference",
      "RGB colour mode requires exactly three genes."
    )
  }
  if (
    !identical(normalized_view$colour$mode, "__gene__") &&
      !is.null(normalized_view$colour$gene)
  ) {
    cv_config_abort(
      "invalid_reference",
      "Only gene colour mode may carry a selected gene."
    )
  }
  if (
    !identical(normalized_view$colour$mode, "__rgb__") &&
      length(normalized_view$colour$rgb_genes)
  ) {
    cv_config_abort(
      "invalid_reference",
      "Only RGB colour mode may carry RGB genes."
    )
  }

  normalized
}

cv_config_json_document <- function(config) {
  # `auto_unbox = TRUE` is necessary for scalar contract fields, but it would
  # otherwise collapse one-item vectors. Mark every array-valued field so the
  # wire format stays structurally stable for non-R consumers.
  document <- config
  document$selection$cells <- I(document$selection$cells)
  document$view$colour$rgb_genes <- I(document$view$colour$rgb_genes)
  document$view$projections <- I(document$view$projections)
  document$view$spatial_sections <- I(document$view$spatial_sections)
  document$view$filters <- lapply(document$view$filters, I)
  document$view$hidden_levels <- lapply(
    document$view$hidden_levels,
    function(item) {
      item$levels <- I(item$levels)
      item
    }
  )
  document
}

cv_config_encode <- function(config) {
  document <- cv_config_json_document(config)
  text <- jsonlite::toJSON(
    document,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = TRUE
  )
  text <- paste0(as.character(text), "\n")
  cv_config_check_json_size(text)
  text
}

cv_config_decode <- function(text, cells) {
  cv_config_check_json_size(text)
  value <- tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(error) {
      cv_config_abort("invalid_json", "The file is not valid JSON.")
    }
  )
  cv_config_check_json_array_shapes(value)
  cv_config_normalize(value, cells = cells)
}

cv_config_prepare <- function(config, cells, now = Sys.time()) {
  if (!is.list(config)) {
    cv_config_abort("invalid_type", "The configuration must be an object.")
  }
  config$created_at <- format(
    as.POSIXct(now, tz = "UTC"),
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  normalized <- cv_config_normalize(config, cells = cells)
  list(config = normalized, json = cv_config_encode(normalized))
}

cv_config_safe_message <- function(error) {
  code <- if (inherits(error, "cv_config_error")) error$code else NULL
  if (is.null(code)) {
    code <- "unknown"
  }
  switch(
    code,
    too_large = "The configuration is larger than 5 MiB.",
    too_complex = "The configuration contains too many values.",
    too_many_items = "The configuration contains too many items.",
    too_deep = "The configuration is nested too deeply.",
    invalid_json = "The file is not valid JSON.",
    invalid_file = "Choose one JSON file.",
    unsupported_schema = "This is not a Linked views configuration.",
    unsupported_version = "This configuration version is not supported.",
    dataset_mismatch = "This configuration belongs to a different cell population.",
    missing_cell = "The configuration selects cells that are not in this data set.",
    missing_gene = "The configuration uses a gene that is unavailable here.",
    invalid_reference = "The configuration uses a view that is unavailable here.",
    unknown_field = "The configuration contains an unsupported setting.",
    missing_field = "The configuration is missing a required setting.",
    invalid_type = "The configuration contains a value of the wrong type.",
    invalid_value = "The configuration contains an invalid value.",
    invalid_object = "The configuration contains an invalid object.",
    invalid_timestamp = "The configuration contains an invalid timestamp.",
    duplicate_item = "The configuration contains duplicate items.",
    string_too_long = "The configuration contains text that is too long.",
    out_of_range = "The configuration contains a value outside its supported range.",
    "The configuration could not be opened."
  )
}
