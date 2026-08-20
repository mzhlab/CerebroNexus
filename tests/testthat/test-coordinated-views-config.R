# test-coordinated-views-config.R — portable Linked views configuration.

inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE),
  normalizePath(
    system.file(package = "CerebroNexus"),
    mustWork = FALSE
  )
)
config_inst <- inst_candidates[file.exists(file.path(
  inst_candidates,
  "viewer"
))][1]
config_file <- if (is.na(config_inst)) {
  ""
} else {
  file.path(config_inst, "viewer/coordinated_views/config.R")
}
config_env <- new.env(parent = baseenv())
if (nzchar(config_file) && file.exists(config_file)) {
  sys.source(config_file, envir = config_env)
}

config_cells <- c("cell-a", "cell-b", "cell-c")

valid_linked_view_config <- function() {
  list(
    schema = "cerebronexus-linked-view",
    version = 1L,
    created_at = "2026-08-20T12:00:00Z",
    dataset = list(
      cell_count = length(config_cells),
      cell_fingerprint = config_env$cv_config_cell_fingerprint(config_cells)
    ),
    selection = list(
      cells = c("cell-a", "cell-c"),
      source = "UMAP"
    ),
    view = list(
      colour = list(
        mode = "cell_type",
        gene = NULL,
        rgb_genes = character(),
        clip = 0.01
      ),
      projections = c("umap", "pca"),
      spatial_sections = "donor-a",
      active_spatial = "donor-a",
      filters = list(sample = c("donor-a", "donor-b")),
      hidden_levels = list(
        list(group = "cell_type", levels = "Doublet")
      ),
      display = list(
        percentage_cells = 75,
        point_size = 3,
        point_opacity = 0.8,
        group_labels = TRUE,
        selection_mode = "lasso",
        clone_layout = "stack"
      ),
      focus_space = "projection::umap",
      lenses = list(
        list(
          space = "projection::umap",
          viewport = list(cx = 0.5, cy = 0.4, span = 1.2),
          rotation = NULL
        ),
        list(
          space = "projection::pca",
          viewport = list(cx = 0.6, cy = 0.7, span = 0.9),
          rotation = list(rx = 0.2, ry = -0.1)
        )
      ),
      spatial_backgrounds = list(
        list(
          section = "donor-a",
          mode = "image",
          image_id = "he-main",
          opacity = 0.65,
          alignment = list(
            offset_x = 3,
            offset_y = -2,
            scale_x = 1.1,
            scale_y = 0.9,
            rotation = 0.05,
            lock_aspect = FALSE,
            flip_x = TRUE,
            flip_y = FALSE,
            show = TRUE
          )
        )
      ),
      trekker = list(
        dissolve_percentage = 25,
        evidence = TRUE,
        niche_radius = 250
      )
    )
  )
}

test_that("the linked-view configuration module exists and parses", {
  expect_true(nzchar(config_file) && file.exists(config_file))
  expect_no_error(parse(file = config_file))
})

test_that("linked-view fingerprints ignore cell order", {
  fingerprint <- config_env$cv_config_cell_fingerprint
  expect_true(is.function(fingerprint))
  expect_identical(
    fingerprint(c("cell-b", "cell-a", "cell-c")),
    fingerprint(c("cell-c", "cell-b", "cell-a"))
  )
  expect_match(
    fingerprint(config_cells),
    "^md5-cell-set-v1:[0-9a-f]{32}$"
  )
})

test_that("a version-one configuration round-trips canonically", {
  normalized <- config_env$cv_config_normalize(
    valid_linked_view_config(),
    cells = config_cells
  )
  encoded <- config_env$cv_config_encode(normalized)
  decoded <- config_env$cv_config_decode(encoded, cells = config_cells)

  expect_identical(decoded$schema, "cerebronexus-linked-view")
  expect_identical(decoded$version, 1L)
  expect_identical(decoded$selection$cells, c("cell-a", "cell-c"))
  expect_identical(decoded$view$display$selection_mode, "lasso")
  expect_identical(decoded$view$lenses[[2L]]$rotation$ry, -0.1)
  expect_identical(decoded$view$spatial_backgrounds[[1L]]$image_id, "he-main")
  expect_identical(
    decoded$view$spatial_backgrounds[[1L]]$alignment$scale_y,
    0.9
  )
  expect_true(decoded$view$spatial_backgrounds[[1L]]$alignment$flip_x)
  expect_true(endsWith(encoded, "\n"))
})

test_that("canonical JSON preserves array fields with one item", {
  config <- valid_linked_view_config()
  config$selection$cells <- "cell-a"
  config$view$projections <- "umap"
  config$view$filters <- list(sample = "donor-a")
  config$view$lenses <- config$view$lenses[1L]

  encoded <- config_env$cv_config_encode(
    config_env$cv_config_normalize(config, cells = config_cells)
  )
  decoded <- jsonlite::fromJSON(encoded, simplifyVector = FALSE)

  expect_type(decoded$dataset$cell_fingerprint, "character")
  expect_type(decoded$selection$cells, "list")
  expect_type(decoded$view$projections, "list")
  expect_type(decoded$view$spatial_sections, "list")
  expect_type(decoded$view$filters$sample, "list")
  expect_type(decoded$view$hidden_levels[[1L]]$levels, "list")
  expect_length(decoded$selection$cells, 1L)
  expect_length(decoded$view$lenses, 1L)
})

test_that("JSON scalar values cannot impersonate arrays", {
  normalized <- config_env$cv_config_normalize(
    valid_linked_view_config(),
    cells = config_cells
  )
  document <- jsonlite::fromJSON(
    config_env$cv_config_encode(normalized),
    simplifyVector = FALSE
  )
  mutations <- list(
    selection_cells = function(value) {
      value$selection$cells <- "cell-a"
      value
    },
    rgb_genes = function(value) {
      value$view$colour$rgb_genes <- "CD3D"
      value
    },
    projections = function(value) {
      value$view$projections <- "umap"
      value
    },
    spatial_sections = function(value) {
      value$view$spatial_sections <- "donor-a"
      value
    },
    filter_levels = function(value) {
      value$view$filters$sample <- "donor-a"
      value
    },
    hidden_levels = function(value) {
      value$view$hidden_levels <- value$view$hidden_levels[[1L]]
      value
    },
    hidden_level_names = function(value) {
      value$view$hidden_levels[[1L]]$levels <- "Doublet"
      value
    },
    lenses = function(value) {
      value$view$lenses <- value$view$lenses[[1L]]
      value
    },
    backgrounds = function(value) {
      value$view$spatial_backgrounds <- value$view$spatial_backgrounds[[1L]]
      value
    }
  )

  for (name in names(mutations)) {
    text <- jsonlite::toJSON(
      mutations[[name]](document),
      auto_unbox = TRUE,
      null = "null"
    )
    error <- tryCatch(
      config_env$cv_config_decode(text, cells = config_cells),
      cv_config_error = identity
    )
    expect_s3_class(error, "cv_config_error")
    expect_identical(error$code, "invalid_type", label = name)
  }
})

expect_config_error <- function(config, code) {
  error <- tryCatch(
    {
      config_env$cv_config_normalize(config, cells = config_cells)
      NULL
    },
    cv_config_error = identity
  )
  expect_s3_class(error, "cv_config_error")
  expect_identical(error$code, code)
}

test_that("unsupported schema versions and unknown fields are rejected", {
  config <- valid_linked_view_config()
  config$schema <- "another-schema"
  expect_config_error(config, "unsupported_schema")

  config <- valid_linked_view_config()
  config$version <- 2L
  expect_config_error(config, "unsupported_version")

  config <- valid_linked_view_config()
  config$token <- "must-not-be-accepted"
  expect_config_error(config, "unknown_field")

  config <- valid_linked_view_config()
  config$view$display$future_option <- TRUE
  expect_config_error(config, "unknown_field")
})

test_that("dataset mismatch and incomplete cohorts are rejected", {
  config <- valid_linked_view_config()
  config$dataset$cell_count <- 4L
  expect_config_error(config, "dataset_mismatch")

  config <- valid_linked_view_config()
  config$dataset$cell_fingerprint <- paste0(
    "md5-cell-set-v1:",
    paste(rep("0", 32L), collapse = "")
  )
  expect_config_error(config, "dataset_mismatch")

  config <- valid_linked_view_config()
  config$selection$cells <- c("cell-a", "missing-cell")
  expect_config_error(config, "missing_cell")

  config <- valid_linked_view_config()
  config$selection$cells <- c("cell-a", "cell-a")
  expect_config_error(config, "duplicate_item")
})

test_that("configuration bounds reject hostile or ambiguous state", {
  config <- valid_linked_view_config()
  config$view$colour$clip <- Inf
  expect_config_error(config, "invalid_type")

  config <- valid_linked_view_config()
  config$view$display$point_opacity <- 1.1
  expect_config_error(config, "out_of_range")

  config <- valid_linked_view_config()
  config$view$lenses[[2L]]$space <- config$view$lenses[[1L]]$space
  expect_config_error(config, "duplicate_item")

  config <- valid_linked_view_config()
  config$view$trekker$niche_radius <- 501
  expect_config_error(config, "out_of_range")

  config <- valid_linked_view_config()
  config$view$filters <- Reduce(
    function(value, unused) list(value),
    seq_len(12L),
    init = "cell-a"
  )
  expect_config_error(config, "too_deep")

  config <- valid_linked_view_config()
  config$selection$cells <- rep(
    "cell-a",
    config_env$CV_CONFIG_MAX_NODES + 1L
  )
  expect_config_error(config, "too_complex")
})

test_that("timestamps must identify a real UTC instant", {
  config <- valid_linked_view_config()
  config$created_at <- "2026-99-99T99:99:99Z"
  expect_config_error(config, "invalid_timestamp")
})

test_that("display bounds match the controls users can actually choose", {
  config <- valid_linked_view_config()
  config$view$display$percentage_cells <- 5
  config$view$display$point_size <- 0
  config$view$display$point_opacity <- 0
  expect_no_error(config_env$cv_config_normalize(config, cells = config_cells))

  config$view$display$point_size <- 20.1
  expect_config_error(config, "out_of_range")
})

test_that("cross-field references must describe one coherent workspace", {
  config <- valid_linked_view_config()
  config$view$active_spatial <- "not-selected"
  expect_config_error(config, "invalid_reference")

  config <- valid_linked_view_config()
  config$view$focus_space <- "projection::missing"
  expect_config_error(config, "invalid_reference")

  config <- valid_linked_view_config()
  config$view$spatial_backgrounds[[1L]]$section <- "not-selected"
  expect_config_error(config, "invalid_reference")

  config <- valid_linked_view_config()
  config$view$spatial_backgrounds[[1L]]["image_id"] <- list(NULL)
  expect_config_error(config, "invalid_reference")

  config <- valid_linked_view_config()
  config$view$colour$mode <- "__rgb__"
  config$view$colour$rgb_genes <- c("CD3D", "MS4A1")
  expect_config_error(config, "invalid_reference")

  config <- valid_linked_view_config()
  config$view$colour$gene <- "CD3D"
  expect_config_error(config, "invalid_reference")

  config <- valid_linked_view_config()
  config$view$colour$rgb_genes <- c("CD3D", "MS4A1", "LYZ")
  expect_config_error(config, "invalid_reference")
})

test_that("JSON decoding is bounded and reports safe error codes", {
  malformed <- tryCatch(
    config_env$cv_config_decode("{not-json", cells = config_cells),
    cv_config_error = identity
  )
  expect_s3_class(malformed, "cv_config_error")
  expect_identical(malformed$code, "invalid_json")
  expect_identical(conditionMessage(malformed), "The file is not valid JSON.")

  oversized <- paste(rep("x", 5L * 1024L * 1024L + 1L), collapse = "")
  too_large <- tryCatch(
    config_env$cv_config_decode(oversized, cells = config_cells),
    cv_config_error = identity
  )
  expect_s3_class(too_large, "cv_config_error")
  expect_identical(too_large$code, "too_large")
})

test_that("canonical output contains only the portable allowlist", {
  config <- valid_linked_view_config()
  config$view$filters <- structure(list(), names = character())
  encoded <- config_env$cv_config_encode(config_env$cv_config_normalize(
    config,
    cells = config_cells
  ))
  expect_no_match(encoded, "dataset_id", fixed = TRUE)
  expect_no_match(encoded, "data:image", fixed = TRUE)
  expect_no_match(encoded, "expression", fixed = TRUE)
  expect_no_match(encoded, "metadata", fixed = TRUE)
  expect_no_match(encoded, "credentials", fixed = TRUE)
  expect_no_match(encoded, "token", fixed = TRUE)
  expect_no_match(encoded, "/Users/", fixed = TRUE)
  expect_match(encoded, '"filters": \\{\\}', perl = TRUE)

  invalid <- valid_linked_view_config()
  invalid$view$filters <- unname(invalid$view$filters)
  expect_config_error(invalid, "invalid_type")
})

test_that("the server prepares a canonical snapshot with its own timestamp", {
  config <- valid_linked_view_config()
  config$created_at <- "1999-01-01T00:00:00Z"
  prepared <- config_env$cv_config_prepare(
    config,
    cells = config_cells,
    now = as.POSIXct("2026-08-20 14:35:42", tz = "UTC")
  )

  expect_identical(prepared$config$created_at, "2026-08-20T14:35:42Z")
  expect_identical(
    config_env$cv_config_decode(prepared$json, cells = config_cells),
    prepared$config
  )
})

test_that("configuration failures map to a bounded public vocabulary", {
  mismatch <- structure(
    list(
      message = "/private/path must never be shown",
      call = NULL,
      code = "dataset_mismatch"
    ),
    class = c("cv_config_error", "error", "condition")
  )
  malformed <- simpleError("parser leaked /private/path")

  expect_identical(
    config_env$cv_config_safe_message(mismatch),
    "This configuration belongs to a different cell population."
  )
  expect_identical(
    config_env$cv_config_safe_message(malformed),
    "The configuration could not be opened."
  )
  expect_identical(
    config_env$cv_config_safe_message(structure(
      list(message = "raw parser details", call = NULL, code = "invalid_json"),
      class = c("cv_config_error", "error", "condition")
    )),
    "The file is not valid JSON."
  )
  expect_identical(
    config_env$cv_config_safe_message(structure(
      list(message = "bad value", call = NULL, code = "out_of_range"),
      class = c("cv_config_error", "error", "condition")
    )),
    "The configuration contains a value outside its supported range."
  )
})

test_that("the coordinated server exposes validated configuration transport", {
  server_file <- file.path(config_inst, "viewer/coordinated_views/server.R")
  server <- paste(readLines(server_file, warn = FALSE), collapse = "\n")

  expect_match(server, "/viewer/coordinated_views/config.R", fixed = TRUE)
  expect_match(server, "cv_config_cell_fingerprint(b$cells)", fixed = TRUE)
  expect_match(server, "coordviews_config_request", fixed = TRUE)
  expect_match(
    server,
    'c("nonce", "action", "revision", "config", "config_json")',
    fixed = TRUE
  )
  expect_match(
    server,
    'c("prepare", "apply")',
    fixed = TRUE
  )
  expect_match(server, "normalized <- cv_config_decode(", fixed = TRUE)
  expect_match(server, "request$config_json", fixed = TRUE)
  expect_match(server, "coordviews_config_upload", fixed = TRUE)
  expect_match(server, "coordviews_config_upload_nonce", fixed = TRUE)
  expect_match(server, '"coordviews_config_result"', fixed = TRUE)
  expect_false(grepl("downloadHandler", server, fixed = TRUE))
  expect_false(grepl("coordviews_config_json", server, fixed = TRUE))
  expect_false(grepl('"copy", "download", "save"', server, fixed = TRUE))
  expect_match(server, "CV_CONFIG_MAX_BYTES", fixed = TRUE)
  capture_start <- regexpr(
    'observeEvent(\n  input[["coordviews_config_request"]]',
    server,
    fixed = TRUE
  )
  capture_end <- regexpr(
    "coordviews_share_response_cache",
    server,
    fixed = TRUE
  )
  capture_observer <- substr(
    server,
    capture_start,
    capture_end - 1L
  )
  capture_export <- strsplit(
    capture_observer,
    "        } else {",
    fixed = TRUE
  )[[1L]][[1L]]
  expect_false(grepl(
    "cv_config_validate_genes",
    capture_export,
    fixed = TRUE
  ))
})

test_that("Save and share markup is accessible and bundled in every Viewer", {
  ui_file <- file.path(config_inst, "viewer/coordinated_views/UI.R")
  shell_file <- file.path(config_inst, "viewer/shiny_UI.R")
  controller_file <- file.path(config_inst, "viewer/www/coordviews-config.js")
  cache_file <- file.path(
    config_inst,
    "viewer/www/coordviews-config-cache.js"
  )
  css_file <- file.path(config_inst, "viewer/www/coordviews.css")
  server_file <- file.path(config_inst, "viewer/coordinated_views/server.R")
  ui <- paste(readLines(ui_file, warn = FALSE), collapse = "\n")
  shell <- paste(readLines(shell_file, warn = FALSE), collapse = "\n")
  controller <- paste(readLines(controller_file, warn = FALSE), collapse = "\n")
  css <- paste(readLines(css_file, warn = FALSE), collapse = "\n")
  server <- paste(readLines(server_file, warn = FALSE), collapse = "\n")

  expect_match(ui, 'id = "cv-config-open"', fixed = TRUE)
  expect_match(ui, 'icon("share-alt")', fixed = TRUE)
  expect_match(ui, 'tags$span("Share views")', fixed = TRUE)
  expect_match(
    ui,
    'title = "Save, open, import, export, or share a linked view"',
    fixed = TRUE
  )
  expect_false(grepl("Import / export view…", ui, fixed = TRUE))
  expect_match(
    css,
    "color: var(--cv-amber700, #c85a0e); font: 750 12px/1 inherit;",
    fixed = TRUE
  )
  expect_match(ui, 'id = "cv-config-dialog"', fixed = TRUE)
  expect_match(ui, 'id = "cv-config-status"', fixed = TRUE)
  expect_match(ui, 'id = "cv-snapshot-save"', fixed = TRUE)
  expect_match(ui, 'id = "cv-snapshot-list"', fixed = TRUE)
  expect_match(ui, 'id = "cv-snapshot-name-dialog"', fixed = TRUE)
  expect_match(ui, 'id = "cv-snapshot-name-input"', fixed = TRUE)
  expect_match(ui, 'id = "cv-config-share"', fixed = TRUE)
  expect_match(ui, 'id = "cv-share-create"', fixed = TRUE)
  expect_match(ui, 'id = "cv-share-list"', fixed = TRUE)
  expect_match(
    ui,
    'class = "cv-config-region cv-config-transfer"',
    fixed = TRUE
  )
  expect_match(ui, '"Import or export a view"', fixed = TRUE)
  expect_match(
    ui,
    'class = "cv-config-region cv-config-save-local"',
    fixed = TRUE
  )
  expect_match(ui, '"Saved on this device"', fixed = TRUE)
  expect_match(ui, 'class = "cv-snapshot-library"', fixed = TRUE)
  expect_false(grepl(
    'class = "cv-config-region cv-snapshots"',
    ui,
    fixed = TRUE
  ))
  expect_match(ui, '`aria-live` = "polite"', fixed = TRUE)
  expect_match(ui, '"coordviews_config_upload"', fixed = TRUE)
  expect_false(grepl('"coordviews_config_download"', ui, fixed = TRUE))
  expect_match(ui, 'icon("folder-open")', fixed = TRUE)
  expect_match(ui, "cell barcodes", fixed = TRUE)
  expect_true(file.exists(controller_file))
  expect_true(file.exists(cache_file))
  expect_true(file.exists(file.path(
    config_inst,
    "viewer/www/viewer-clipboard.js"
  )))
  expect_match(controller, "new window.Blob", fixed = TRUE)
  expect_match(controller, "URL.createObjectURL", fixed = TRUE)
  expect_match(controller, "setUploadLoading", fixed = TRUE)
  expect_match(controller, "cerebro.linked-views.snapshots.v1", fixed = TRUE)
  expect_false(grepl(
    "cerebro.linked-views.share-receipts.v1",
    controller,
    fixed = TRUE
  ))
  expect_match(controller, "share_create", fixed = TRUE)
  expect_match(controller, "share_open", fixed = TRUE)
  expect_false(grepl("share_revoke", controller, fixed = TRUE))
  expect_match(controller, "linked_view", fixed = TRUE)
  share_section <- substr(
    ui,
    regexpr('id = "cv-config-share"', ui, fixed = TRUE),
    regexpr('id = "cv-config-status"', ui, fixed = TRUE)
  )
  expect_false(grepl('hidden = "hidden"', share_section, fixed = TRUE))
  expect_false(grepl("shareAdminAllowed", controller, fixed = TRUE))
  expect_false(grepl("viewer_admin_capability", controller, fixed = TRUE))
  expect_match(controller, "pendingShare.retried", fixed = TRUE)
  expect_match(controller, "crypto.getRandomValues", fixed = TRUE)
  expect_match(controller, "copy.textContent = ok ? 'Copied ✓'", fixed = TRUE)
  expect_false(grepl("copyText(shareUrl(token));", controller, fixed = TRUE))
  expect_match(controller, "window.cerebroClipboard.copyText", fixed = TRUE)
  expect_match(controller, "copy.textContent = 'Copying…'", fixed = TRUE)
  expect_gt(
    regexpr("copy.textContent = ok ? 'Copied ✓'", controller, fixed = TRUE),
    regexpr("window.cerebroClipboard.copyText", controller, fixed = TRUE)
  )
  expect_match(
    controller,
    "document.contains(copy)) copy.focus()",
    fixed = TRUE
  )
  expect_false(grepl("copy.disabled = true", controller, fixed = TRUE))
  expect_match(controller, "cerebro:share-created", fixed = TRUE)
  expect_match(
    controller,
    "Share link ready. Saving in the background…",
    fixed = TRUE
  )
  expect_lt(
    regexpr("latestShare = {", controller, fixed = TRUE),
    regexpr("preparedCache.get()", controller, fixed = TRUE)
  )
  expect_false(grepl("Preparing view…", controller, fixed = TRUE))
  expect_false(grepl("Creating share link…", controller, fixed = TRUE))
  expect_match(controller, "Share link ready.", fixed = TRUE)
  expect_match(controller, "result.code === 'prepare_expired'", fixed = TRUE)
  expect_match(controller, "preparedCache.clear()", fixed = TRUE)
  expect_match(
    server,
    'c("nonce", "action", "prepared_id", "token")',
    fixed = TRUE
  )
  expect_match(
    server,
    'creator = viewer_auth_context(session)$user %||% ""',
    fixed = TRUE
  )
  expect_match(
    server,
    "dataset_label = cv_selected_dataset_name()",
    fixed = TRUE
  )
  create_branch <- substr(
    server,
    regexpr('if (identical(action, "share_create"))', server, fixed = TRUE),
    regexpr('} else if (identical(action, "share_open"))', server, fixed = TRUE)
  )
  expect_false(grepl("viewer_is_admin(session)", create_branch, fixed = TRUE))
  expect_false(grepl(
    "Administrator access is required",
    create_branch,
    fixed = TRUE
  ))
  expect_false(grepl(
    "cv_config_prepare(request$config",
    create_branch,
    fixed = TRUE
  ))
  expect_false(grepl("cv_config_decode", create_branch, fixed = TRUE))
  expect_match(create_branch, "cv_prepared_share_fetch", fixed = TRUE)
  expect_match(
    controller,
    "Shiny.setInputValue('coordviews_share_request', pendingShare.payload",
    fixed = TRUE
  )
  expect_match(controller, "saveSnapshotLocally", fixed = TRUE)
  expect_match(css, ".cv-config-share[hidden]", fixed = TRUE)
  expect_false(grepl("background: #f7fbff", css, fixed = TRUE))
  expect_false(grepl("color: #245b8f", css, fixed = TRUE))
  expect_false(grepl(
    "JSON.stringify(state.capture())",
    controller,
    fixed = TRUE
  ))
  expect_match(controller, "withPreparedConfig", fixed = TRUE)
  expect_match(controller, "}, 120)", fixed = TRUE)
  expect_match(controller, "preparedCache.invalidate", fixed = TRUE)
  expect_match(
    controller,
    "if (!preparedCache && !setupPreparedCache())",
    fixed = TRUE
  )
  expect_match(
    controller,
    "The view preparation service is not ready. Reload this page and try again.",
    fixed = TRUE
  )
  expect_match(
    controller,
    "payload.prepared_id = prepared.prepared_id",
    fixed = TRUE
  )
  expect_false(grepl(
    "payload.config_json = prepared.json",
    controller,
    fixed = TRUE
  ))
  expect_match(controller, "250", fixed = TRUE)
  expect_false(grepl("request('save')", controller, fixed = TRUE))
  expect_false(grepl("function finishSave", controller, fixed = TRUE))
  expect_match(controller, "Restore did not finish", fixed = TRUE)
  expect_match(controller, "snapshotNeedsColourData", fixed = TRUE)
  expect_match(controller, "cv-snapshot-mark", fixed = TRUE)
  expect_match(controller, "cv-snapshot-primary", fixed = TRUE)
  expect_match(controller, "openSnapshotNameDialog", fixed = TRUE)
  expect_no_match(controller, "window.prompt", fixed = TRUE)
  expect_match(css, "cv-config-status.is-working", fixed = TRUE)
  expect_match(controller, "cerebro:linkedviews-selection", fixed = TRUE)
  expect_match(controller, "function connectShiny", fixed = TRUE)
  expect_match(controller, "shiny:connected", fixed = TRUE)
  expect_match(css, "background: var(--cv-amber", fixed = TRUE)
  expect_match(css, ".cv-config-upload .btn-file > span", fixed = TRUE)
  expect_match(css, ".cv-config-upload .input-group-prepend", fixed = TRUE)
  expect_match(css, ".shiny-file-input-progress { display: none", fixed = TRUE)
  expect_match(css, "display: grid; width: 100%; min-width: 0;", fixed = TRUE)
  expect_match(
    shell,
    'cerebro_js("viewer-clipboard.js", defer = TRUE)',
    fixed = TRUE
  )
  expect_match(
    shell,
    'cerebro_js("coordviews-config-cache.js", defer = TRUE)',
    fixed = TRUE
  )
  expect_match(
    shell,
    'cerebro_js("coordviews-config.js", defer = TRUE)',
    fixed = TRUE
  )
})

test_that("prepared configuration cache rejects stale responses and reuses JSON", {
  cache_file <- file.path(config_inst, "viewer/www/coordviews-config-cache.js")
  expect_true(file.exists(cache_file))
  if (!nzchar(Sys.which("node")) || !file.exists(cache_file)) {
    skip("Node.js and the prepared-cache module are required")
  }
  script <- paste0(
    "const api=require(",
    encodeString(cache_file, quote = '"'),
    ");",
    "const watchdog=setTimeout(()=>{console.error('cache timeout missing');process.exit(2);},80);",
    "let captured=0,sent=[];",
    "let state={created_at:'2026-08-20T00:00:00Z',selection:{cells:['a']}};",
    "const cache=api.create({debounceMs:1,capture:()=>{captured++;return {...state};},",
    "send:x=>sent.push(x),ready:()=>true});",
    "cache.invalidate();",
    "setTimeout(()=>{",
    "if(sent.length!==1)throw Error('one warm request expected');",
    "const stale=sent[0];cache.invalidate();",
    "setTimeout(()=>{if(sent.length!==2)throw Error('new revision expected');",
    "cache.receive({nonce:stale.nonce,action:'prepare',ok:true,json:'stale'});",
    "const fresh=sent[1];cache.receive({nonce:fresh.nonce,action:'prepare',ok:true,",
    "json:'canonical',prepared_id:'prepared-1',filename:'view.json',selected_cells:1});",
    "Promise.all([cache.get(),cache.get()]).then(v=>{",
    "if(v[0].json!=='canonical'||v[1].json!=='canonical')throw Error('cache miss');",
    "if(v[0].prepared_id!=='prepared-1')throw Error('prepared id missing');",
    "if(sent.length!==2)throw Error('ready cache was not reused');",
    "const slow=api.create({debounceMs:1,requestTimeoutMs:3,capture:()=>state,",
    "send:()=>{},ready:()=>true});",
    "slow.get().then(()=>{throw Error('timeout expected');},error=>{",
    "if(!/timed out/.test(error.message))throw error;",
    "clearTimeout(watchdog);console.log('prepared-cache-ok');});});},5);},5);"
  )
  script_file <- tempfile(fileext = ".js")
  writeLines(script, script_file, useBytes = TRUE)
  output <- system2("node", script_file, stdout = TRUE, stderr = TRUE)
  expect_identical(
    attr(output, "status") %||% 0L,
    0L,
    info = paste(output, collapse = "\n")
  )
  expect_true(any(grepl("prepared-cache-ok", output, fixed = TRUE)))
})

test_that("share responses can be replayed without creating duplicate links", {
  server_file <- file.path(config_inst, "viewer/coordinated_views/server.R")
  server <- paste(readLines(server_file, warn = FALSE), collapse = "\n")

  expect_match(server, "coordviews_share_response_cache", fixed = TRUE)
  expect_match(server, "cv_share_replay(nonce, action)", fixed = TRUE)
  expect_match(server, "cv_share_cache(nonce, payload)", fixed = TRUE)
  expect_match(server, "CV_SHARE_REPLAY_LIMIT <- 64L", fixed = TRUE)
  expect_match(server, "rm(list = expired", fixed = TRUE)
})

test_that("prepared share records are bounded and expire inside one session", {
  server_file <- file.path(config_inst, "viewer/coordinated_views/server.R")
  server <- paste(readLines(server_file, warn = FALSE), collapse = "\n")

  expect_match(server, "CV_PREPARED_SHARE_LIMIT <- 8L", fixed = TRUE)
  expect_match(server, "CV_PREPARED_SHARE_TTL_SECONDS <- 300", fixed = TRUE)
  expect_match(server, "cv_prepared_share_store", fixed = TRUE)
  expect_match(server, "cv_prepared_share_fetch", fixed = TRUE)
  expect_match(server, "dataset_fingerprint", fixed = TRUE)
  expect_match(server, "prepared_id = prepared_id", fixed = TRUE)
})
