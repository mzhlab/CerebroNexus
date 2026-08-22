builder_profile_source_runtime(globalenv())

builder_rail_source <- function(file) {
  path <- builder_profile_inst_path("builder", file)
  if (nzchar(path) && file.exists(path)) {
    had_dir <- exists("dir", envir = globalenv(), inherits = FALSE)
    old_dir <- if (had_dir) get("dir", envir = globalenv()) else NULL
    assign("dir", builder_profile_inst_path("builder"), envir = globalenv())
    on.exit(
      {
        if (had_dir) {
          assign("dir", old_dir, envir = globalenv())
        } else {
          rm("dir", envir = globalenv())
        }
      },
      add = TRUE
    )
    sys.source(path, envir = globalenv())
  }
}

test_that("the dataset rail presents a button and single-file transport", {
  app <- readLines(
    builder_profile_inst_path("builder", "app.R"),
    warn = FALSE
  )
  rail <- paste(
    app[seq_len(grep("server <- function", app)[1L] - 1L)],
    collapse = "\n"
  )

  expect_match(rail, "tags$input(", fixed = TRUE)
  expect_match(rail, 'id = "dataset_files"', fixed = TRUE)
  expect_match(rail, 'type = "file"', fixed = TRUE)
  expect_false(grepl('multiple = "multiple"', rail, fixed = TRUE))
  expect_match(rail, 'hidden = "hidden"', fixed = TRUE)
  expect_match(rail, "tags$button(", fixed = TRUE)
  expect_match(rail, 'id = "choose_local_datasets"', fixed = TRUE)
  expect_match(rail, 'id = "builder_upload_datasets"', fixed = TRUE)
  expect_match(
    rail,
    'class = "dataset-file-control builder-file-picker builder-file-picker--sidebar"',
    fixed = TRUE
  )
  expect_match(rail, "builder-upload-transport", fixed = TRUE)
  expect_match(rail, "builder-file-trigger", fixed = TRUE)
  expect_match(rail, '"Choose local datasets…"', fixed = TRUE)
  expect_match(rail, '"Upload through browser…"', fixed = TRUE)
  expect_false(grepl("fileInput(", rail, fixed = TRUE))
  expect_match(rail, "builder_example_buttons_ui(", fixed = TRUE)
  expect_false(grepl('uiOutput("example_buttons")', rail, fixed = TRUE))
  expect_false(grepl('uiOutput("browser_panel")', rail, fixed = TRUE))
})

test_that("example directory is static and does not load objects for first paint", {
  app <- readLines(
    builder_profile_inst_path("builder", "app.R"),
    warn = FALSE
  )
  pre_server <- paste(
    app[seq_len(grep("server <- function", app)[1L] - 1L)],
    collapse = "\n"
  )

  expect_match(pre_server, "builder_example_directory()", fixed = TRUE)
  expect_match(pre_server, "builder_example_buttons_ui", fixed = TRUE)
  expect_false(grepl("renderUI\\(.*example", pre_server, perl = TRUE))
  expect_false(grepl("readRDS|read\\.rds|builder_adapter_inspect", pre_server))
  io <- paste(
    readLines(builder_profile_inst_path("builder", "io.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(io, "builder_example_directory <- local", fixed = TRUE)
})

test_that("static example cards carry stable IDs and loading metadata", {
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  html <- htmltools::renderTags(app_env$builder_example_buttons_ui())$html

  expect_match(html, 'data-ex="all_content"', fixed = TRUE)
  expect_match(html, 'data-label="All content"', fixed = TRUE)
  expect_match(html, 'class="btn example-btn"', fixed = TRUE)
  expect_match(html, "builder-example-directory", fixed = TRUE)
})

builder_rail_source("state.R")
builder_rail_source("extras.R")
builder_rail_source(file.path("ui", "dataset_rail.R"))

test_that("the empty workspace presents the first guided step", {
  html <- htmltools::renderTags(builder_empty_workbench_ui())$html

  expect_match(html, "Step 1 of 4", fixed = TRUE)
  expect_match(html, "Add your data", fixed = TRUE)
  expect_match(html, "Drop a dataset here", fixed = TRUE)
  expect_match(html, "Large files load in the background", fixed = TRUE)
  expect_match(html, "Choose files", fixed = TRUE)
})


test_that("the Builder launcher bounds uploads without leaking process options", {
  launcher_path <- testthat::test_path(
    "..",
    "..",
    "R",
    "launchCerebroBuilder.R"
  )
  skip_if_not(file.exists(launcher_path), "R source tree not present")
  launcher <- paste(
    readLines(launcher_path, warn = FALSE),
    collapse = "\n"
  )

  expect_match(launcher, "max_file_size = 8000", fixed = TRUE)
  expect_match(launcher, 'getOption("shiny.maxRequestSize")', fixed = TRUE)
  expect_match(launcher, "on.exit(", fixed = TRUE)
  expect_match(launcher, "max_file_size * 1024^2", fixed = TRUE)
})

builder_rail_api <- c(
  "builder_state",
  "builder_reduce_state",
  "builder_app_options_for_plan",
  "builder_dataset_rail_ui",
  "builder_dataset_rail_server",
  "builder_validate_next_plan"
)
builder_rail_api_available <- all(vapply(
  builder_rail_api,
  exists,
  logical(1),
  mode = "function",
  inherits = TRUE
))

builder_rail_entry <- function(id, label = toupper(id), source_id = id) {
  list(
    id = id,
    source_id = source_id,
    output_id = id,
    selector_value = id,
    snapshot = list(
      path = paste0("/private/", source_id),
      owner_token = paste0("owner-", source_id),
      object_md5 = strrep("a", 32L)
    ),
    format = "RDS",
    profile = list(
      n_cells = 12L,
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA"
    ),
    settings = list(
      name = label,
      groups = "cluster",
      reductions = "umap",
      layer = "data",
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA",
      palette = list(cluster = c(one = "#111111")),
      analyses = character()
    )
  )
}

test_that("the persistent dataset rail API is available", {
  expect_true(builder_rail_api_available)
})

if (builder_rail_api_available) {
  test_that("rail actions use copy-on-write instead of serializing the store", {
    state <- builder_state(lapply(c("a", "b"), builder_rail_entry))
    reducer_environment <- environment(builder_reduce_state)
    expect_false(exists(
      ".builder_store_copy",
      envir = reducer_environment,
      inherits = FALSE
    ))

    selected <- builder_reduce_state(state, list(type = "select", id = "b"))
    planned <- builder_datasets_for_plan(state)
    selected$datasets[[1L]]$settings$name <- "Changed"
    planned[[2L]]$settings$name <- "Changed too"

    expect_identical(state$current_dataset, "a")
    expect_identical(state$datasets[[1L]]$settings$name, "A")
    expect_identical(state$datasets[[2L]]$settings$name, "B")
  })

  test_that("derived dataset state is shared for one entry revision", {
    entry <- builder_rail_entry("a", "Dataset A")
    entry$revision <- 1L
    cache <- new.env(parent = emptyenv())
    runtime <- environment(builder_dataset_state)
    original_dataset_state <- get("builder_dataset_state", envir = runtime)
    calls <- 0L
    assign(
      "builder_dataset_state",
      function(entry) {
        calls <<- calls + 1L
        original_dataset_state(entry)
      },
      envir = runtime
    )
    on.exit(
      assign("builder_dataset_state", original_dataset_state, envir = runtime),
      add = TRUE
    )

    first <- builder_cached_dataset_state(entry, cache)
    second <- builder_cached_dataset_state(entry, cache)
    entry$revision <- 2L
    third <- builder_cached_dataset_state(entry, cache)

    expect_identical(first, second)
    expect_s3_class(third, "builder_dataset_state")
    expect_identical(calls, 2L)
    expect_length(ls(cache, all.names = TRUE), 1L)
  })

  test_that("trusted rail actions validate only the action boundary", {
    state <- builder_state(lapply(c("a", "b"), builder_rail_entry))
    reducer_environment <- environment(builder_reduce_state)
    original_dataset_state <- get(
      "builder_dataset_state",
      envir = reducer_environment
    )
    assign(
      "builder_dataset_state",
      function(...) stop("an unchanged entry was revalidated"),
      envir = reducer_environment
    )
    on.exit(
      assign(
        "builder_dataset_state",
        original_dataset_state,
        envir = reducer_environment
      ),
      add = TRUE
    )

    selected <- builder_reduce_state(
      state,
      list(type = "select", id = "b"),
      trusted = TRUE
    )

    expect_identical(selected$current_dataset, "b")
  })

  test_that("initial-App selection is not stored in dataset state", {
    state <- builder_state(lapply(c("a", "b", "c"), builder_rail_entry))
    reordered <- builder_reduce_state(
      state,
      list(type = "reorder", order = c("c", "a", "b"))
    )

    expect_identical(builder_app_options_for_plan(reordered), list())
    expect_error(
      builder_state(
        list(builder_rail_entry("a")),
        initial_dataset_override = "a"
      )
    )
    expect_error(
      builder_reduce_state(reordered, list(type = "select_initial", id = "a")),
      class = "builder_state_error"
    )
    expect_error(
      builder_reduce_state(
        reordered,
        list(type = "duplicate", id = "a", new_id = "a-copy")
      ),
      class = "builder_state_error"
    )

    html <- as.character(builder_dataset_rail_ui(reordered))
    expect_false(grepl("builder-select-initial", html, fixed = TRUE))
    expect_false(grepl("builder-duplicate", html, fixed = TRUE))
  })

  test_that("typed rail state rejects hostile entries and forged undo records", {
    base <- builder_state(list(builder_rail_entry("a")))

    numeric_groups <- builder_rail_entry("b")
    numeric_groups$settings$groups <- 1
    expect_error(
      builder_reduce_state(base, list(type = "add", entry = numeric_groups)),
      class = "builder_state_error"
    )

    language_reductions <- builder_rail_entry("a")
    language_reductions$settings$reductions <- quote(umap)
    expect_error(
      builder_reduce_state(
        base,
        list(
          type = "replace",
          id = "a",
          entry = language_reductions
        )
      ),
      class = "builder_state_error"
    )

    referenced <- builder_rail_entry("b")
    referenced$settings$palette$hostile <- new.env(parent = emptyenv())
    expect_error(
      builder_reduce_state(base, list(type = "add", entry = referenced)),
      class = "builder_state_error"
    )

    forged <- base
    forged$can_undo_remove <- TRUE
    forged$last_removed <- list(
      id = "ghost",
      entry = list(id = "ghost", settings = list()),
      index = 1L
    )
    expect_error(
      builder_reduce_state(forged, list(type = "undo_remove")),
      class = "builder_state_error"
    )

    top_level_reference <- base
    top_level_reference$intruder <- new.env(parent = emptyenv())
    expect_error(
      builder_reduce_state(
        top_level_reference,
        list(type = "select", id = "a")
      ),
      class = "builder_state_error"
    )

    forged_anchor <- builder_reduce_state(base, list(type = "remove", id = "a"))
    forged_anchor$last_removed$before_id <- "ghost-anchor"
    expect_error(
      builder_datasets_for_plan(forged_anchor),
      class = "builder_state_error"
    )
    forged_tombstone_field <- builder_reduce_state(
      base,
      list(type = "remove", id = "a")
    )
    forged_tombstone_field$last_removed$intruder <- "unexpected"
    expect_error(
      builder_datasets_for_plan(forged_tombstone_field),
      class = "builder_state_error"
    )
  })

  test_that("builder_state validates the current dataset as plain scalar text", {
    entries <- list(builder_rail_entry("a"))
    invalid <- list(
      character(),
      c("a", "a"),
      NA_character_,
      " ",
      structure("a", marker = TRUE),
      factor("a"),
      1
    )
    for (value in invalid) {
      expect_error(
        builder_state(entries, current_dataset = value),
        class = "builder_state_error"
      )
    }
    state <- builder_state(entries, current_dataset = "a")
    expect_identical(.builder_store_assert(state), "a")
  })

  test_that("stable dataset and action ids are plain scalar text", {
    attributed <- structure("b", marker = TRUE)
    hostile <- builder_rail_entry("b")
    hostile$id <- attributed
    expect_error(builder_state(list(hostile)), class = "builder_state_error")

    base <- builder_state(list(builder_rail_entry("a")))
    expect_error(
      builder_reduce_state(base, list(type = "add", entry = hostile)),
      class = "builder_state_error"
    )
    expect_error(
      builder_reduce_state(
        base,
        list(type = "replace_all", datasets = list(hostile))
      ),
      class = "builder_state_error"
    )

    for (type in c("select", "remove", "move")) {
      action <- list(type = type, id = structure("a", marker = TRUE))
      if (identical(type, "move")) {
        action$direction <- "up"
      }
      expect_error(
        builder_reduce_state(base, action),
        class = "builder_state_error"
      )
    }

    hostile_source <- builder_rail_entry("b")
    hostile_source$source_id <- structure("source-b", marker = TRUE)
    expect_error(
      builder_reduce_state(base, list(type = "add", entry = hostile_source)),
      class = "builder_state_error"
    )
    hostile_output <- builder_rail_entry("b")
    hostile_output$output_id <- structure("output-b", marker = TRUE)
    expect_error(
      builder_reduce_state(base, list(type = "add", entry = hostile_output)),
      class = "builder_state_error"
    )
  })

  test_that("move accepts only exact scalar plain directions", {
    state <- builder_state(lapply(c("a", "b"), builder_rail_entry))
    invalid <- list(
      "u",
      c("up", "down"),
      NA_character_,
      structure("up", marker = TRUE),
      list("up")
    )
    for (direction in invalid) {
      expect_error(
        builder_reduce_state(
          state,
          list(
            type = "move",
            id = "b",
            direction = direction
          )
        ),
        class = "builder_state_error"
      )
    }
  })

  test_that("pending sources reject repeats and release only after failure", {
    pending <- character()
    first <- builder_source_reserve(list(), pending, "example", "pbmc_small")
    expect_true(first$ok)
    repeated <- builder_source_reserve(
      list(),
      first$pending,
      "example",
      "pbmc_small"
    )
    expect_false(repeated$ok)

    path <- file.path(tempdir(), "source", "..", "dataset.rds")
    file_first <- builder_source_reserve(list(), pending, "file", path)
    file_repeat <- builder_source_reserve(
      list(),
      file_first$pending,
      "file",
      file.path(tempdir(), "dataset.rds")
    )
    expect_false(file_repeat$ok)

    retry_pending <- builder_source_release(first$pending, first$key)
    retry <- builder_source_reserve(
      list(),
      retry_pending,
      "example",
      "pbmc_small"
    )
    expect_true(retry$ok)

    successful <- builder_rail_entry("loaded")
    successful$example <- "pbmc_small"
    handed_off <- builder_source_reserve(
      list(successful),
      builder_source_release(retry$pending, retry$key),
      "example",
      "pbmc_small"
    )
    expect_false(handed_off$ok)
    expect_identical(handed_off$code, "source_in_store")
  })

  test_that("snapshot lifecycle releases aliases and identities exactly once", {
    released <- character()
    unregistered <- character()
    release <- function(worker, id, expected_identity) {
      released <<- c(released, expected_identity)
      worker$registry[[id]] <- NULL
      worker
    }
    unregister <- function(worker, id) {
      unregistered <<- c(unregistered, id)
      worker$registry[[id]] <- NULL
      worker
    }
    worker <- list(registry = list(a = "shared", copy = "shared"))
    retained_copy <- builder_rail_entry("copy")
    retained_copy$snapshot$identity <- "shared"
    alias <- builder_snapshot_release_transition(
      worker,
      id = "a",
      identity = "shared",
      retained = list(retained_copy),
      pending = list(a = "shared"),
      release = release,
      unregister = unregister
    )
    expect_identical(unregistered, "a")
    expect_length(released, 0L)
    expect_null(alias$pending$a)

    last <- builder_snapshot_release_transition(
      alias$worker,
      id = "copy",
      identity = "shared",
      retained = list(),
      pending = list(copy = "shared"),
      release = release,
      unregister = unregister
    )
    expect_identical(released, "shared")
    expect_length(last$worker$registry, 0L)

    worker <- list(registry = list(x = "identity-x", y = "identity-y"))
    x <- builder_snapshot_release_transition(
      worker,
      "x",
      "identity-x",
      list(),
      list(x = "identity-x"),
      release,
      unregister
    )
    y <- builder_snapshot_release_transition(
      x$worker,
      "y",
      "identity-y",
      list(),
      list(y = "identity-y"),
      release,
      unregister
    )
    expect_identical(released, c("shared", "identity-x", "identity-y"))
    expect_length(y$worker$registry, 0L)

    failed_worker <- list(registry = list(z = "identity-z"))
    failed_pending <- list(z = "identity-z")
    expect_error(
      builder_snapshot_release_transition(
        failed_worker,
        "z",
        "identity-z",
        list(),
        failed_pending,
        function(...) stop("drop failed"),
        unregister
      ),
      "drop failed",
      fixed = TRUE
    )
    expect_identical(failed_worker$registry$z, "identity-z")
    expect_identical(failed_pending$z, "identity-z")
    retried <- builder_snapshot_release_transition(
      failed_worker,
      "z",
      "identity-z",
      list(),
      failed_pending,
      release,
      unregister
    )
    expect_null(retried$pending$z)
    expect_identical(tail(released, 1L), "identity-z")
  })

  test_that("remove offers undo and restores the original order", {
    state <- builder_state(lapply(c("a", "b", "c"), builder_rail_entry))
    removed <- builder_reduce_state(
      state,
      list(type = "remove", id = "b")
    )

    expect_identical(
      vapply(removed$datasets, `[[`, character(1), "id"),
      c("a", "c")
    )
    expect_true(removed$can_undo_remove)
    expect_identical(removed$last_removed$id, "b")
    expect_identical(removed$last_removed$index, 2L)
    expect_identical(
      vapply(builder_datasets_for_plan(removed), `[[`, character(1), "id"),
      c("a", "c")
    )

    restored <- builder_reduce_state(removed, list(type = "undo_remove"))
    expect_identical(
      vapply(restored$datasets, `[[`, character(1), "id"),
      c("a", "b", "c")
    )
    expect_false(restored$can_undo_remove)
  })

  test_that("typed rail readiness includes required output setup", {
    entry <- builder_rail_entry("a")
    entry$settings$groups <- character()
    state <- builder_state(list(entry))
    html <- as.character(builder_dataset_rail_ui(state))

    expect_match(html, "Blocked", fixed = TRUE)
  })

  test_that("typed store rejects unrecognized dataset profiles", {
    invalid <- list(
      id = "invalid",
      profile = list(marker = "not-a-profile"),
      settings = list(name = "Invalid")
    )

    expect_error(builder_state(list(invalid)), class = "builder_state_error")
  })

  test_that("rail UI reports canonical state and exposes semantic controls", {
    state <- builder_state(list(builder_rail_entry("a", "Dataset A")))
    html <- as.character(builder_dataset_rail_ui(state, current = "a"))

    expect_match(html, "Dataset A", fixed = TRUE)
    expect_match(html, "ds ds--ready is-active", fixed = TRUE)
    expect_match(html, 'data-load-state="ready"', fixed = TRUE)
    expect_match(html, 'class="ds-ready-stamp needs-review"', fixed = TRUE)
    expect_match(html, ">NEEDS<", fixed = TRUE)
    expect_match(html, ">CHECK<", fixed = TRUE)
    expect_false(grepl("ds-ready-dot", html, fixed = TRUE))
    expect_match(html, 'aria-current="true"', fixed = TRUE)
    expect_match(html, "12 cells", fixed = TRUE)
    expect_false(grepl(
      'rail-readiness-status">Ready',
      html,
      fixed = TRUE
    ))
    expect_match(html, "data-direction=\"up\"", fixed = TRUE)
    expect_match(html, "data-direction=\"down\"", fixed = TRUE)
    expect_match(html, ">Remove<", fixed = TRUE)
    expect_false(grepl("data-icon=\"remove\"", html, fixed = TRUE))
    expect_false(grepl("builder-select-initial", html, fixed = TRUE))
    expect_false(grepl("builder-duplicate", html, fixed = TRUE))
    expect_match(html, "data-confirm=\"true\"", fixed = TRUE)
  })

  test_that("ready rail fingerprints cover every visible row state", {
    entry <- builder_rail_entry("a", "Dataset A")
    base <- builder_dataset_rail_row_model(entry, 1L, 2L, "a")

    expect_identical(
      builder_dataset_rail_row_fingerprint(base),
      builder_dataset_rail_row_fingerprint(base)
    )
    for (field in c(
      "index",
      "id",
      "label",
      "cells",
      "format",
      "readiness_label",
      "selected",
      "confirm",
      "can_up",
      "can_down"
    )) {
      changed <- base
      changed[[field]] <- if (is.logical(base[[field]])) {
        !base[[field]]
      } else if (is.numeric(base[[field]])) {
        base[[field]] + 1L
      } else {
        paste0(base[[field]], "-changed")
      }
      expect_false(
        identical(
          builder_dataset_rail_row_fingerprint(base),
          builder_dataset_rail_row_fingerprint(changed)
        ),
        info = field
      )
    }

    html <- as.character(builder_dataset_rail_row_ui(base))
    expect_match(html, 'data-rail-fingerprint="', fixed = TRUE)
  })

  test_that("ready rail patch is an ordered authoritative snapshot", {
    state <- builder_state(list(
      builder_rail_entry("a", "Dataset A"),
      builder_rail_entry("b", "Dataset B")
    ))
    patch <- builder_dataset_rail_patch(state, current = "b")

    expect_identical(vapply(patch$rows, `[[`, character(1), "id"), c("a", "b"))
    expect_true(all(vapply(
      patch$rows,
      function(row) {
        nzchar(row$fingerprint) && grepl('data-ds="', row$html, fixed = TRUE)
      },
      logical(1)
    )))
    expect_match(patch$empty_html, "No datasets yet", fixed = TRUE)
    expect_identical(patch, builder_dataset_rail_patch(state, current = "b"))

    invisible_change <- state
    invisible_change$datasets[[1L]]$path <- "/different/source/path"
    expect_identical(
      patch,
      builder_dataset_rail_patch(invisible_change, current = "b")
    )
  })

  test_that("ready rail patch caches unchanged rows by entry revision", {
    state <- builder_state(list(
      builder_rail_entry("a", "Dataset A"),
      builder_rail_entry("b", "Dataset B")
    ))
    cache <- new.env(parent = emptyenv())
    first <- builder_dataset_rail_patch(
      state,
      current = "a",
      row_cache = cache
    )
    expect_true(all(vapply(
      first$rows,
      function(row) is.character(row$html),
      logical(1)
    )))

    original_dataset_state <- builder_dataset_state
    calls <- 0L
    assign(
      "builder_dataset_state",
      function(entry) {
        calls <<- calls + 1L
        original_dataset_state(entry)
      },
      envir = environment(builder_dataset_rail_patch)
    )
    on.exit(
      assign(
        "builder_dataset_state",
        original_dataset_state,
        envir = environment(builder_dataset_rail_patch)
      ),
      add = TRUE
    )

    unchanged <- builder_dataset_rail_patch(
      state,
      current = "a",
      row_cache = cache
    )
    expect_identical(calls, 0L)
    expect_true(all(vapply(
      unchanged$rows,
      function(row) is.null(row$html),
      logical(1)
    )))

    state$datasets[[2L]]$settings$name <- "Renamed"
    state$datasets[[2L]]$revision <- 1L
    changed <- builder_dataset_rail_patch(
      state,
      current = "a",
      row_cache = cache
    )
    expect_identical(calls, 1L)
    expect_null(changed$rows[[1L]]$html)
    expect_match(changed$rows[[2L]]$html, "Renamed", fixed = TRUE)

    synced <- builder_dataset_rail_patch(
      state,
      current = "a",
      row_cache = cache,
      force_html = TRUE
    )
    expect_true(all(vapply(
      synced$rows,
      function(row) is.character(row$html),
      logical(1)
    )))
  })

  test_that("ready rail server publishes snapshots instead of renderUI", {
    server <- paste(
      readLines(
        builder_profile_inst_path("builder", "server", "datasets.R"),
        warn = FALSE
      ),
      collapse = "\n"
    )

    expect_false(grepl(
      "output$ds_ready_list <- renderUI",
      server,
      fixed = TRUE
    ))
    expect_match(
      server,
      "builder_dataset_rail_patch(",
      fixed = TRUE
    )
    expect_match(
      server,
      'sendCustomMessage("builder_dataset_rail_patch"',
      fixed = TRUE
    )
  })

  test_that("rail status maps only typed dataset readiness", {
    entry <- builder_rail_entry("a", "Dataset A")
    readiness <- builder_dataset_state(entry)$readiness
    expect_identical(
      .builder_rail_readiness(list(readiness = readiness))$label,
      "Ready"
    )
    labels <- vapply(
      c("ready", "needs_attention", "loading", "blocked", "reload_required"),
      function(value) .builder_rail_readiness(list(readiness = value))$label,
      character(1)
    )
    expect_identical(
      unname(labels),
      c("Ready", "Needs attention", "Loading", "Blocked", "Reload required")
    )
  })

  test_that("rail provides dataset navigation without review progress", {
    first <- builder_rail_entry("a", "Dataset A")
    second <- builder_rail_entry("b", "Dataset B")
    state <- builder_state(list(first, second), current_dataset = "b")

    rail <- as.character(builder_dataset_rail_ui(state, current = "b"))

    expect_length(gregexpr("NEEDS", rail, fixed = TRUE)[[1L]], 2L)
    expect_match(rail, "Dataset B", fixed = TRUE)
    expect_match(rail, 'aria-current="true"', fixed = TRUE)
    expect_false(grepl("datasets reviewed", rail, fixed = TRUE))
    expect_false(grepl("progressbar", rail, fixed = TRUE))
  })

  test_that("client events preserve confirmed removal and native-picker semantics", {
    js <- paste(
      readLines(
        builder_profile_inst_path("builder", "www", "builder.js"),
        warn = FALSE
      ),
      collapse = "\n"
    )

    expect_false(grepl("window.confirm", js, fixed = TRUE))
    expect_match(js, "showRemoveConfirmation", fixed = TRUE)
    expect_match(js, 'setAttribute("aria-modal", "true")', fixed = TRUE)
    expect_match(js, 'getElementById("dataset_files")', fixed = TRUE)
    expect_false(grepl("choose_files", js, fixed = TRUE))
    expect_false(grepl("closeFileDialog", js, fixed = TRUE))
    expect_match(js, "event.altKey", fixed = TRUE)
    expect_match(js, "reorder_ds", fixed = TRUE)
    expect_match(js, '"builder_dataset_rail_patch"', fixed = TRUE)
    expect_match(js, "reconcileDatasetRail", fixed = TRUE)
    expect_match(js, "dataset.railFingerprint", fixed = TRUE)
    expect_match(js, "row.replaceWith(item.parsed)", fixed = TRUE)
    expect_match(js, "rail.appendChild(row)", fixed = TRUE)
    expect_match(js, "isSelfManagedRailMutation", fixed = TRUE)
    expect_match(js, "window.BuilderIcons.decorate(rail)", fixed = TRUE)
    expect_false(grepl("syncWorkflowStickyState", js, fixed = TRUE))
    expect_match(js, "datasetRailFocusTarget", fixed = TRUE)
    expect_match(js, "updateRailSummary()", fixed = TRUE)
    expect_false(grepl(
      'example.classList.add("is-taken")',
      js,
      fixed = TRUE
    ))

    app <- builder_app_source_text()
    expect_match(app, "pending_snapshot_drops", fixed = TRUE)
    expect_match(app, "other_drop_ids", fixed = TRUE)
  })

  test_that("one hidden transport sits beside queued multi-file entry", {
    app <- readLines(
      builder_profile_inst_path("builder", "app.R"),
      warn = FALSE
    )
    text <- paste(app, collapse = "\n")
    rail <- sub(
      "server <- function.*",
      "",
      text
    )

    expect_match(rail, "tags$input(", fixed = TRUE)
    expect_match(rail, 'type = "file"', fixed = TRUE)
    expect_false(grepl('multiple = "multiple"', rail, fixed = TRUE))
    expect_match(rail, 'id = "choose_local_datasets"', fixed = TRUE)
    expect_match(rail, 'id = "builder_upload_datasets"', fixed = TRUE)
    expect_match(rail, 'id = "ds_client_import_queue"', fixed = TRUE)
    expect_match(rail, "builder_example_buttons_ui()", fixed = TRUE)
    expect_false(grepl("browse_open", text, fixed = TRUE))
    expect_false(grepl("browse_dir", text, fixed = TRUE))
    expect_false(grepl("browser_panel", text, fixed = TRUE))
    expect_false(grepl("browse_list", text, fixed = TRUE))
    expect_false(grepl("ICON_FOLDER", text, fixed = TRUE))
    io <- paste(
      readLines(builder_profile_inst_path("builder", "io.R"), warn = FALSE),
      collapse = "\n"
    )
    expect_false(grepl("builder_browse <-", io, fixed = TRUE))
    expect_false(grepl("builder_browse_roots <-", io, fixed = TRUE))
    expect_false(grepl(
      'example.classList.add("is-taken")',
      paste(
        readLines(
          builder_profile_inst_path("builder", "www", "builder.js"),
          warn = FALSE
        ),
        collapse = "\n"
      ),
      fixed = TRUE
    ))
  })

  test_that("removal validation freezes exact next-plan membership", {
    state <- builder_state(lapply(c("a", "b", "c"), builder_rail_entry))
    next_state <- builder_reduce_state(state, list(type = "remove", id = "b"))
    calls <- list()
    freeze <- function(entries, out_dir, make_app, overwrite, app_options) {
      calls <<- list(
        ids = vapply(entries, `[[`, character(1), "id"),
        out_dir = out_dir,
        app_options = app_options
      )
      list(
        error = NULL,
        dataset_order = calls$ids,
        output_release = list(
          targets = file.path(out_dir, paste0(calls$ids, ".crb"))
        )
      )
    }

    validation <- builder_validate_next_plan(
      next_state,
      out_dir = file.path(tempdir(), "rail-output"),
      make_app = TRUE,
      overwrite = FALSE,
      freeze_plan = freeze
    )

    expect_s3_class(validation, "builder_rail_validation")
    expect_true(validation$ok)
    expect_identical(calls$ids, c("a", "c"))
    expect_identical(validation$dataset_ids, c("a", "c"))
    expect_identical(
      basename(validation$expected_members),
      c("a.crb", "c.crb")
    )

    missing <- builder_validate_next_plan(
      next_state,
      out_dir = "",
      freeze_plan = freeze
    )
    expect_false(missing$ok)
    expect_identical(missing$code, "missing_output_target")
    expect_null(missing$plan)

    malformed <- builder_validate_next_plan(
      next_state,
      out_dir = tempdir(),
      freeze_plan = function(...) "not-a-plan"
    )
    expect_false(malformed$ok)
    expect_identical(malformed$code, "invalid_next_plan")

    expect_error(
      builder_validate_next_plan(
        next_state,
        out_dir = tempdir(),
        freeze_plan = function(...) stop("unexpected freeze failure")
      ),
      "unexpected freeze failure",
      fixed = TRUE
    )
  })

  test_that("real Shiny rail wiring dispatches selection, ordering, and removal", {
    skip_if_not_installed("shiny")
    initial <- builder_state(lapply(c("a", "b", "c"), builder_rail_entry))

    shiny::testServer(
      function(input, output, session) {
        rail_store <- shiny::reactiveVal(initial)
        validation_calls <- shiny::reactiveVal(list())
        validate_remove <- function(next_state, id) {
          validation_calls(c(
            validation_calls(),
            list(list(
              id = id,
              dataset_ids = vapply(
                next_state$datasets,
                `[[`,
                character(1),
                "id"
              ),
              app_options = builder_app_options_for_plan(next_state)
            ))
          ))
          structure(
            list(
              ok = TRUE,
              code = NULL,
              dataset_ids = validation_calls()[[length(validation_calls())]]$dataset_ids,
              expected_members = paste0(
                validation_calls()[[length(validation_calls())]]$dataset_ids,
                ".crb"
              ),
              plan = list(error = NULL)
            ),
            class = c("builder_rail_validation", "list")
          )
        }
        rail <- builder_dataset_rail_server(
          input = input,
          session = session,
          store = rail_store,
          validate_remove = validate_remove
        )
      },
      {
        untouched <- rail$state()
        session$setInputs(pick = c("a", "b"))
        session$setInputs(pick = structure("a", marker = TRUE))
        session$setInputs(pick = " ")
        session$setInputs(pick = NA_character_)
        session$setInputs(pick = "stale")
        session$setInputs(
          drop_ds = list(
            id = c("a", "b"),
            confirmed = TRUE
          )
        )
        session$setInputs(
          drop_ds = list(
            id = structure("a", marker = TRUE),
            confirmed = TRUE
          )
        )
        session$setInputs(
          drop_ds = structure(
            list(id = "a", confirmed = TRUE),
            class = "forged_drop"
          )
        )
        session$setInputs(drop_ds = list(id = "stale", confirmed = TRUE))
        expect_identical(rail$state(), untouched)

        session$setInputs(pick = "b")
        expect_identical(rail$state()$current_dataset, "b")

        session$setInputs(reorder_ds = list(id = "c", direction = "up"))
        expect_identical(
          vapply(rail$state()$datasets, `[[`, character(1), "id"),
          c("a", "c", "b")
        )

        stable <- rail$state()
        session$setInputs(reorder_ds = list(id = "c", direction = "u"))
        session$setInputs(reorder_ds = list(id = "stale", direction = "up"))
        session$setInputs(
          reorder_ds = list(
            id = "c",
            direction = c("up", "down")
          )
        )
        expect_identical(rail$state(), stable)

        expect_identical(builder_app_options_for_plan(rail$state()), list())

        session$setInputs(drop_ds = list(id = "b", confirmed = FALSE))
        expect_identical(rail$validation()$code, "confirmation_required")
        expect_true(
          "b" %in%
            vapply(
              rail$state()$datasets,
              `[[`,
              character(1),
              "id"
            )
        )

        session$setInputs(drop_ds = list(id = "b", confirmed = TRUE))
        expect_false(
          "b" %in%
            vapply(
              rail$state()$datasets,
              `[[`,
              character(1),
              "id"
            )
        )
        expect_identical(
          validation_calls()[[1L]]$dataset_ids,
          c("a", "c")
        )

        session$setInputs(undo_remove = 1L)
        expect_identical(
          vapply(rail$state()$datasets, `[[`, character(1), "id"),
          c("a", "c", "b")
        )
      }
    )
  })

  test_that("dataset selection waits for its injected commit gate", {
    skip_if_not_installed("shiny")
    initial <- builder_state(lapply(c("a", "b"), builder_rail_entry))
    pending_commit <- NULL
    requested <- NULL

    shiny::testServer(
      function(input, output, session) {
        rail_store <- shiny::reactiveVal(initial)
        rail <- builder_dataset_rail_server(
          input = input,
          session = session,
          store = rail_store,
          validate_remove = function(...) {
            stop("removal validation should not run")
          },
          select_dataset = function(id, commit) {
            requested <<- id
            pending_commit <<- commit
          }
        )
      },
      {
        session$setInputs(pick = "b")
        expect_identical(requested, "b")
        expect_true(is.function(pending_commit))
        expect_identical(rail$state()$current_dataset, "a")

        pending_commit()
        session$flushReact()
        expect_identical(rail$state()$current_dataset, "b")
      }
    )
  })

  test_that("used examples stay disabled in the persistent rail picker", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly")
    app_env <- new.env(parent = globalenv())
    withr::local_dir(builder_profile_inst_path("builder"))
    sys.source("app.R", envir = app_env)
    app_env$builder_session_start <- function(...) {
      list(error = "Worker startup is disabled in this picker-state test.")
    }

    shiny::testServer(app_env$server, {
      expect_false(start_builder_worker())
      used <- builder_rail_entry("used-example")
      used$example <- "all_content"
      sets(list(used))

      session$setInputs(use_example = "all_content")
      expect_identical(
        add_error(),
        "Worker startup is disabled in this picker-state test."
      )

      session$setInputs(use_example = "not-an-example")
      expect_identical(
        add_error(),
        "Worker startup is disabled in this picker-state test."
      )

      expect_length(sets(), 1L)
    })
  })

  test_that("production store replacement never downgrades typed errors", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly")
    app_env <- new.env(parent = globalenv())
    withr::local_dir(builder_profile_inst_path("builder"))
    sys.source("app.R", envir = app_env)
    app_env$builder_session_start <- function(...) {
      list(error = "Worker startup is disabled in this store-seam test.")
    }

    shiny::testServer(app_env$server, {
      before <- store()
      hostile <- builder_rail_entry("hostile")
      hostile$settings$groups <- 1
      expect_error(sets(list(hostile)), class = "builder_state_error")
      expect_identical(store(), before)
      expect_null(store()$.state_only_fixture)
    })
  })

  test_that("the live picker reserves queued sources and permits failure retry", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly")
    app_env <- new.env(parent = globalenv())
    withr::local_dir(builder_profile_inst_path("builder"))
    sys.source("app.R", envir = app_env)
    app_env$builder_session_start <- function(...) {
      list(error = "Worker startup is disabled in this reservation test.")
    }

    shiny::testServer(app_env$server, {
      worker(list(epoch = "worker-reservation"))
      worker_available(TRUE)
      protocol(app_env$builder_request_protocol("worker-reservation"))

      expect_true(start_load("example", "all_content", "PBMC"))
      expect_false(start_load("example", "all_content", "PBMC"))
      expect_length(pending_sources(), 1L)
      pending_sources(builder_source_key("example", "all_content"))
      worker(NULL)
      worker_available(FALSE)
      release_pending_source(list(
        kind = "load",
        source = "example",
        example = "all_content"
      ))
      worker(list(epoch = "worker-reservation-retry"))
      worker_available(TRUE)
      protocol(app_env$builder_request_protocol("worker-reservation-retry"))
      expect_true(start_load("example", "all_content", "PBMC"))

      release_pending_source(list(
        kind = "load",
        source = "example",
        example = "all_content"
      ))
      loaded <- builder_rail_entry("loaded")
      loaded$example <- "all_content"
      sets(list(loaded))
      expect_false(start_load("example", "all_content", "PBMC"))
    })
  })

  test_that("single-file transport requires matching client metadata", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly")
    app_env <- new.env(parent = globalenv())
    withr::local_dir(builder_profile_inst_path("builder"))
    sys.source("app.R", envir = app_env)
    app_env$builder_session_start <- function(...) {
      list(error = "Worker startup is disabled in this native-picker test.")
    }
    app_env$builder_session_load <- function(worker, ...) worker
    app_env$builder_session_poll <- function(worker, ...) {
      list(worker = worker, event = NULL, result = NULL)
    }
    upload_root <- withr::local_tempdir()
    upload_path <- file.path(upload_root, "upload-a")
    writeBin(as.raw(seq_len(10L)), upload_path)
    second_upload_path <- file.path(upload_root, "upload-b")
    writeBin(as.raw(seq_len(20L)), second_upload_path)

    shiny::testServer(app_env$server, {
      worker(list(
        epoch = "worker-native-picker",
        snapshot_root = upload_root
      ))
      worker_available(TRUE)
      protocol(app_env$builder_request_protocol("worker-native-picker"))

      session$setInputs(
        builder_client_import_dispatch = list(
          client_id = "client-import-1",
          name = "alpha.rds",
          size = 10,
          nonce = 1
        )
      )
      session$setInputs(
        dataset_files = data.frame(
          name = "alpha.rds",
          size = 10,
          type = "application/octet-stream",
          datapath = upload_path,
          stringsAsFactors = FALSE
        )
      )

      expect_identical(
        pending_sources(),
        builder_source_key("file", upload_path)
      )
      requests <- c(list(protocol()$pending), protocol()$queue)
      expect_identical(
        vapply(requests, function(request) request$payload$label, character(1)),
        "alpha"
      )
      expect_identical(names(pending_uploads()), "ds1")
      expect_identical(client_import_id_for("ds1"), "client-import-1")
      expect_identical(
        unname(vapply(pending_uploads(), `[[`, character(1), "filename")),
        "alpha.rds"
      )
      expect_identical(
        unname(vapply(pending_uploads(), `[[`, character(1), "type")),
        "RDS"
      )
      expect_false(any(vapply(
        pending_uploads(),
        function(file) any(grepl("/tmp/upload", unlist(file), fixed = TRUE)),
        logical(1)
      )))
      session$setInputs(cancel_pending_upload = list(id = "ds1", nonce = 2))
      expect_false(isTRUE(pending_uploads()[["ds1"]]$visible))
      expect_identical(cancelled_loads(), "ds1")

      session$setInputs(
        dataset_files = data.frame(
          name = "beta.rds",
          size = 20,
          type = "application/octet-stream",
          datapath = second_upload_path,
          stringsAsFactors = FALSE
        )
      )
      expect_false(is.null(pending_client_upload()))
      session$setInputs(
        builder_client_import_dispatch = list(
          client_id = "client-import-2",
          name = "beta.rds",
          size = 20,
          nonce = 3
        )
      )
      expect_identical(client_import_id_for("ds2"), "client-import-2")

      session$setInputs(
        builder_client_import_dispatch = list(
          client_id = "client-import-3",
          name = "gamma.rds",
          size = 30,
          nonce = 4
        )
      )
      session$setInputs(
        dataset_files = data.frame(
          name = "wrong.rds",
          size = 30,
          type = "application/octet-stream",
          datapath = "/tmp/upload-c",
          stringsAsFactors = FALSE
        )
      )
      expect_identical(
        released_client_import_records()[["client-import-3"]]$state,
        "rejected"
      )

      session$setInputs(
        dataset_files = data.frame(
          name = "orphan.rds",
          size = 40,
          type = "application/octet-stream",
          datapath = "/tmp/upload-orphan",
          stringsAsFactors = FALSE
        )
      )
      orphan_token <- pending_client_upload()$token
      expect_true(expire_pending_client_upload(orphan_token))
      expect_null(pending_client_upload())
    })
  })

  test_that("client queue cancellation uses the established server action", {
    client <- paste(
      readLines(
        builder_profile_inst_path("builder", "www", "builder.js"),
        warn = FALSE
      ),
      collapse = "\n"
    )

    expect_false(grepl(".pending-upload-remove", client, fixed = TRUE))
    expect_match(client, "builder-cancel-client-import", fixed = TRUE)
    expect_match(client, 'send("cancel_pending_upload"', fixed = TRUE)
  })

  test_that("protocol recovery retains retried and releases failed load reservations", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly")
    app_env <- new.env(parent = globalenv())
    withr::local_dir(builder_profile_inst_path("builder"))
    sys.source("app.R", envir = app_env)
    app_env$builder_session_start <- function(...) {
      list(error = "Worker startup is disabled in this recovery test.")
    }
    app_env$builder_session_example <- function(...) invisible(TRUE)
    app_env$builder_session_poll <- function(worker, ...) {
      list(worker = worker, event = NULL, result = NULL)
    }

    shiny::testServer(app_env$server, {
      worker(list(epoch = "worker-before-recovery"))
      worker_available(TRUE)
      protocol(app_env$builder_request_protocol("worker-before-recovery"))
      expect_true(start_load("example", "all_content", "PBMC"))
      reserved <- pending_sources()

      expect_true(apply_protocol_recovery(
        protocol(),
        list(epoch = "worker-after-retry"),
        "retryable worker error",
        retry_persistent = TRUE
      ))
      expect_identical(pending_sources(), reserved)

      expect_false(apply_protocol_recovery(
        protocol(),
        list(epoch = "worker-terminal"),
        "terminal worker error",
        retry_persistent = FALSE,
        error = "worker could not restart"
      ))
      expect_length(pending_sources(), 0L)
      failed <- app_env$builder_import_find(imports(), "ds1")
      expect_s3_class(failed, "builder_import_entry")
      expect_identical(failed$load_state, "error")
      expect_match(failed$error, "terminal worker error", fixed = TRUE)

      worker(list(epoch = "worker-retry"))
      worker_available(TRUE)
      protocol(app_env$builder_request_protocol("worker-retry"))
      session$setInputs(retry_import = list(id = "ds1", nonce = 1))
      expect_identical(
        app_env$builder_import_find(imports(), "ds1")$generation,
        2L
      )
      expect_identical(protocol()$pending$dataset, "ds1")
    })
  })

  test_that("an explicitly marked state-only fixture has limited compatibility", {
    skip_if_not_installed("shiny")
    skip_if_not_installed("plotly")
    app_env <- new.env(parent = globalenv())
    withr::local_dir(builder_profile_inst_path("builder"))
    sys.source("app.R", envir = app_env)
    app_env$builder_session_start <- function(...) {
      list(error = "Worker startup is disabled in this fixture test.")
    }

    shiny::testServer(app_env$server, {
      minimal <- list(
        id = "fixture",
        profile = list(marker = "fixture-only"),
        settings = list(name = "Fixture")
      )
      use_state_only_fixture(list(minimal))
      minimal$settings$name <- "Fixture renamed"
      sets(list(minimal))
      expect_true(store()$.state_only_fixture)
      expect_identical(store()$datasets[[1L]]$settings$name, "Fixture renamed")
    })
  })
}
