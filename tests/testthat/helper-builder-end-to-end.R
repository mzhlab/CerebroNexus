builder_e2e_source_runtime <- function(local = parent.frame()) {
  builder_profile_source_runtime(local)
  builder_dir <- builder_profile_inst_path("builder")
  for (file in c(
    "io.R",
    "recommend.R",
    "inspect.R",
    "adapters.R",
    "preview.R",
    "extras.R",
    "analysis.R",
    "marker_import.R",
    "app_bundle.R",
    "build.R",
    "prerequisite.R",
    "state.R",
    "plan.R",
    "report.R",
    "publish.R",
    "coordinator.R"
  )) {
    sys.source(file.path(builder_dir, file), envir = local)
  }
  invisible(local)
}

builder_e2e_without_source <- function(value) {
  if (!is.list(value)) {
    return(value)
  }
  value[c("source", "fingerprint")] <- NULL
  lapply(value, builder_e2e_without_source)
}

builder_e2e_review_metadata_policy <- function(recommendation, groups) {
  policy <- unserialize(serialize(recommendation, NULL, version = 3L))
  for (id in names(policy$columns)) {
    record <- policy$columns[[id]]
    if (identical(record$disposition, "blocking")) {
      next
    }
    include <- isTRUE(record$retain_in_crb) ||
      isTRUE(record$required) ||
      id %in% groups
    disposition <- if (include) {
      "included"
    } else {
      "excluded"
    }
    record$value <- disposition
    record$disposition <- disposition
    record$effective_included <- identical(disposition, "included")
    record$retain_in_crb <- include
    record$group_enabled <- id %in% groups
    record$forced <- isTRUE(record$forced %||% record$required)
    record$requires_confirmation <- FALSE
    record$group_eligible <- id %in% groups
    record$preview_allowed <- id %in% groups
    policy$columns[[id]] <- record
  }
  dispositions <- vapply(policy$columns, `[[`, character(1), "disposition")
  retained <- vapply(
    policy$columns,
    `[[`,
    logical(1),
    "retain_in_crb"
  )
  ids <- names(policy$columns)
  policy$retained <- ids[retained]
  policy$groups <- intersect(ids, groups)
  policy$forced <- ids[vapply(policy$columns, `[[`, logical(1), "forced")]
  policy$included <- ids[retained]
  policy$attention <- ids[dispositions == "attention"]
  policy$excluded <- ids[!retained]
  policy$blocking <- ids[dispositions == "blocking"]
  policy$value <- policy$retained
  policy$requires_confirmation <- length(policy$attention) > 0L ||
    length(policy$blocking) > 0L
  policy
}

builder_e2e_entry <- function(record, caller = parent.frame()) {
  inspected <- get("builder_adapter_inspect", caller, inherits = TRUE)(
    get("builder_example_adapter", caller, inherits = TRUE)(
      record$id,
      record$make()$object
    )
  )
  metadata <- get(
    "builder_recommend_metadata",
    caller,
    inherits = TRUE
  )(
    inspected$profile,
    required = c(
      inspected$legacy_profile$nUMI,
      inspected$legacy_profile$nGene
    ),
    dependency_ids = stats::setNames(
      list("core.qc.nUMI", "core.qc.nGene"),
      c(
        inspected$legacy_profile$nUMI,
        inspected$legacy_profile$nGene
      )
    )
  )
  groups <- as.character(inspected$legacy_profile$group_preselect)
  projections <- as.character(inspected$legacy_profile$reduction_preselect)
  recommendations <- list(
    metadata = metadata,
    groups = list(value = groups[[1L]], included = groups),
    projections = list(
      value = projections[[1L]],
      included = projections
    ),
    organism = list(value = inspected$legacy_profile$organism_guess),
    nomenclature = list(value = "name"),
    backend = list(value = "embedded")
  )
  settings <- get("builder_default_settings", caller, inherits = TRUE)(
    inspected$legacy_profile,
    record$label,
    recommendations = recommendations
  )
  review_groups <- as.character(inspected$legacy_profile$group_candidates)
  settings$metadata_policy <- builder_e2e_review_metadata_policy(
    recommendations$metadata,
    review_groups
  )
  settings$groups <- review_groups
  settings$included_groups <- review_groups
  settings$default_group <- inspected$legacy_profile$group_preselect[[1L]]
  list(
    id = record$id,
    profile = inspected$legacy_profile,
    dataset_profile = inspected$profile,
    levels = inspected$levels,
    revision = 0L,
    settings = settings
  )
}

builder_e2e_invalid_content_entry <- function() {
  caller <- parent.frame()
  object <- get(
    ".builder_fixture_immune",
    caller,
    inherits = TRUE
  )("tcr_hla")
  legacy <- object@misc$immune_repertoire
  legacy[[1L]]$CTaa[[1L]] <- "CASSDIVERGENTF"
  legacy[[1L]]$CTstrict[[1L]] <- "TRB_divergent_clone"
  object@misc$tcr_data <- legacy
  record <- list(
    id = "invalid-content",
    label = "Invalid content",
    make = function() list(object = object, format = "Built-in example")
  )
  inspected <- get("builder_adapter_inspect", caller, inherits = TRUE)(
    get("builder_example_adapter", caller, inherits = TRUE)(
      record$id,
      object
    )
  )
  list(
    object = object,
    inspected = inspected,
    entry = builder_e2e_entry(record, caller = caller)
  )
}

builder_e2e_validate_all_content <- function(
  record,
  source,
  settings,
  crb,
  caller = parent.frame()
) {
  check <- function(value, label) {
    if (!isTRUE(value)) {
      stop("all_content readback mismatch: ", label, call. = FALSE)
    }
  }
  same <- function(left, right) {
    isTRUE(all.equal(left, right, check.attributes = TRUE))
  }
  field_reader <- get(".builder_build_field", caller, inherits = TRUE)
  field <- function(name) field_reader(crb, name)

  promised <- names(record$expected_dispositions)[
    record$expected_dispositions == "preserved"
  ]
  check(
    identical(
      intersect(promised, c("spatial", "trekker")),
      c("spatial", "trekker")
    ),
    "catalog preserved families"
  )
  forbidden <- c(
    "marker_genes",
    "most_expressed_genes",
    "mean_expression",
    "enriched_pathways",
    "trajectories",
    "extra_material",
    "immune_repertoire",
    "hla_typing"
  )
  check(
    all(vapply(
      forbidden,
      function(name) length(field(name)) == 0L,
      logical(1)
    )),
    "Enhance content is not precomputed"
  )

  source_trekker <- source@misc$trekker
  output_trekker <- field("trekker")
  builder_fields <- c(
    "builder_group",
    "builder_colors",
    "builder_group_values"
  )
  check(
    identical(names(output_trekker), c(names(source_trekker), builder_fields)),
    "trekker source and Builder field order"
  )
  check(
    same(source_trekker, output_trekker[names(source_trekker)]),
    "trekker source fields round trip"
  )
  check(
    identical(output_trekker$builder_group, settings$default_group),
    "trekker Builder group"
  )
  expected_group_values <- as.character(
    source@meta.data[
      source_trekker$barcodes,
      settings$default_group,
      drop = TRUE
    ]
  )
  check(
    identical(output_trekker$builder_group_values, expected_group_values),
    "trekker Builder group values"
  )

  spatial <- field("spatial")
  sections <- SeuratObject::Images(source)
  check(identical(names(spatial), sections), "spatial section order")
  image_fovs <- vapply(
    record$histology_images,
    function(image) image$fov_ids[[1L]],
    character(1)
  )
  expected_images <- unique(image_fovs)
  check(
    identical(names(settings$images), expected_images),
    "default image FOV assignments"
  )
  for (section in sections) {
    source_coordinates <- SeuratObject::GetTissueCoordinates(source[[section]])
    source_coordinates <- source_coordinates[, c("x", "y"), drop = FALSE]
    output <- spatial[[section]]
    check(
      same(
        unname(as.matrix(output$coordinates)),
        unname(as.matrix(source_coordinates))
      ),
      paste(section, "coordinates")
    )
    check(
      identical(
        rownames(output$coordinates),
        SeuratObject::Cells(source[[section]])
      ),
      paste(section, "coordinate barcodes")
    )
    check(
      identical(output$coordinate_source, "object.GetTissueCoordinates"),
      paste(section, "coordinate source")
    )
    if (section %in% expected_images) {
      configured <- settings$images[[section]]
      bound_names <- c("xmin", "xmax", "ymin", "ymax")
      payload <- list(
        histology_image = configured$uri,
        histology_image_bounds = stats::setNames(
          as.numeric(unlist(
            configured$bounds[bound_names],
            use.names = FALSE
          )),
          bound_names
        )
      )
      matches <- vapply(
        output$histology_images %||% list(),
        function(observed) {
          identical(
            observed[c("histology_image", "histology_image_bounds")],
            payload
          )
        },
        logical(1)
      )
      check(
        any(matches),
        paste(section, "histology image")
      )
    } else {
      check(!length(output$histology_images), "patient C has no image")
    }
  }
  invisible(TRUE)
}
builder_e2e_browser_available <- function(
  info = tryCatch(chromote::chromote_info(), error = function(error) NULL)
) {
  is.list(info) &&
    is.character(info$path) &&
    length(info$path) == 1L &&
    nzchar(info$path) &&
    identical(info$error %||% "", "") &&
    is.list(info$.check) &&
    identical(info$.check$status, 0L)
}

builder_e2e_run_generated_app <- function(
  app_dir,
  hermetic_library,
  root,
  backend,
  content,
  expected_images,
  label
) {
  started_at <- proc.time()[["elapsed"]]
  runtime_root <- file.path(
    root,
    paste0("runtime-", gsub("[^a-z0-9]+", "-", tolower(label)))
  )
  dir.create(runtime_root, recursive = TRUE, showWarnings = FALSE)
  check <- function(value, detail) {
    if (!isTRUE(value)) {
      stop("generated app runtime [", label, "]: ", detail, call. = FALSE)
    }
  }

  tryCatch(
    {
      port <- httpuv::randomPort(host = "127.0.0.1")
      app <- privacy_start_app(
        app_dir,
        port,
        runtime_root,
        libpath = hermetic_library,
        exclude_package = TRUE,
        test_mode = TRUE
      )
      on.exit(privacy_stop_app(app), add = TRUE)
      privacy_wait_for_app(app)

      if (!builder_e2e_browser_available()) {
        return(list(
          started = TRUE,
          browser_checked = FALSE,
          backend = backend,
          content = content,
          elapsed = unname(proc.time()[["elapsed"]] - started_at)
        ))
      }

      driver <- shinytest2::AppDriver$new(
        app$base_url,
        name = paste0("builder_matrix_", gsub("[^a-z0-9]+", "_", label)),
        load_timeout = 60000
      )
      on.exit(try(driver$stop(), silent = TRUE), add = TRUE)
      driver$wait_for_idle(timeout = 60000)
      driver$wait_for_js(
        paste0(
          "document.querySelector('a[href=\"#shiny-tab-geneExpression\"]') ",
          "!== null && window.Shiny && Shiny.shinyapp.$socket.readyState === 1"
        ),
        timeout = 60000
      )

      driver$click(
        selector = "a[href='#shiny-tab-geneExpression']"
      )
      driver$wait_for_js(
        paste0(
          "document.getElementById('expression_genes_input') !== null && ",
          "document.getElementById('expression_projection_to_display') !== null"
        ),
        timeout = 60000
      )
      driver$set_inputs(expression_genes_input = "Gene1", wait_ = FALSE)
      driver$wait_for_js(
        paste0(
          "(function() {",
          "var plot = document.getElementById('expression_projection');",
          "var canvas = plot && plot.querySelector('canvas.cerebro-projection-canvas');",
          "return !!(canvas && canvas.width > 0 && canvas.height > 0 &&",
          "Number(plot.dataset.pointCount) > 0);",
          "})()"
        ),
        timeout = 60000
      )

      logs <- privacy_app_logs(app)
      backend_evidence <- switch(
        backend,
        embedded = TRUE,
        h5 = grepl("Attaching h5 backend", logs, fixed = TRUE),
        bpcells = grepl("Attaching bpcells backend", logs, fixed = TRUE),
        FALSE
      )
      check(backend_evidence, paste(backend, "backend was not attached"))

      has_tab <- function(id) {
        isTRUE(driver$get_js(sprintf(
          "document.querySelector('a[href=\"#shiny-tab-%s\"]') !== null;",
          id
        )))
      }
      check(has_tab("coordinated_views"), "Linked views was not exposed")
      check(!has_tab("spatial"), "the removed Spatial page was exposed")
      check(!has_tab("trekker"), "the removed Trekker page was exposed")
      driver$click(selector = "a[href='#shiny-tab-coordinated_views']")
      driver$wait_for_js(
        paste0(
          "document.querySelector(",
          "'a[href=\"#shiny-tab-coordinated_views\"]'",
          ").parentElement.classList.contains('active')"
        ),
        timeout = 30000
      )

      if (identical(content, "plain")) {
        driver$wait_for_js(
          paste0(
            "(function() {",
            "var meta = document.getElementById('cv-meta');",
            "return !!(meta && meta.textContent.indexOf('expression') >= 0);",
            "})()"
          ),
          timeout = 60000
        )
      } else if (identical(content, "histology")) {
        expected <- expected_images[[1L]]
        source <- expected$source %||%
          list(name = "Embedded tissue image")
        expected_label <- basename(source$name)
        expected_json <- jsonlite::toJSON(expected_label, auto_unbox = TRUE)
        driver$wait_for_js(
          paste0(
            "(function() {",
            "var titles = Array.from(document.querySelectorAll('.cv-ptitle'))",
            ".map(function(x) { return x.textContent; });",
            "var picker = document.getElementById('cv-img-pick');",
            "var labels = picker ? Array.from(picker.options)",
            ".map(function(x) { return x.textContent; }) : [];",
            "var canvas = document.querySelector(",
            "'.cv-pane:not(.cv-hidden) canvas[id^=\"cv-cv-\"]');",
            "return titles.some(function(x) { return x.indexOf('(spatial)') >= 0; }) && ",
            "labels.indexOf(",
            expected_json,
            ") >= 0 && ",
            "canvas && canvas.width > 0 && canvas.height > 0;",
            "})()"
          ),
          timeout = 60000
        )
      } else if (identical(content, "trekker")) {
        driver$wait_for_js(
          paste0(
            "(function() {",
            "var titles = Array.from(document.querySelectorAll('.cv-ptitle'))",
            ".map(function(x) { return x.textContent; });",
            "var controls = document.getElementById('cv-trekker-ctl');",
            "var canvas = document.querySelector(",
            "'.cv-pane:not(.cv-hidden) canvas[id^=\"cv-cv-\"]');",
            "return titles.some(function(x) { return x.indexOf('Trekker') >= 0; }) && ",
            "controls && controls.style.display !== 'none' && ",
            "canvas && canvas.width > 0 && canvas.height > 0;",
            "})()"
          ),
          timeout = 60000
        )
      }

      list(
        started = TRUE,
        browser_checked = TRUE,
        backend = backend,
        content = content,
        elapsed = unname(proc.time()[["elapsed"]] - started_at)
      )
    },
    error = function(error) {
      stop(
        "generated app runtime [",
        label,
        "]: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
}
