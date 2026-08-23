test_that("the live app exposes imports before their worker result", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this import queue test.")
  }
  app_env$builder_session_example <- function(...) invisible(TRUE)
  app_env$builder_session_poll <- function(worker, ...) {
    list(worker = worker, event = NULL, result = NULL)
  }
  progress_root <- tempfile("builder-import-queue-")
  dir.create(progress_root, mode = "0700")
  withr::defer(unlink(progress_root, recursive = TRUE, force = TRUE))

  shiny::testServer(app_env$server, {
    worker(list(epoch = "worker-import-queue", snapshot_root = progress_root))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-import-queue"))

    expect_true(start_load("example", "all_content", "All content"))
    session$flushReact()
    entry <- app_env$builder_import_find(imports(), "ds1")
    expect_s3_class(entry, "builder_import_entry")
    expect_true(entry$load_state %in% c("queued", "reading"))
    expect_identical(active_import_id(), "ds1")

    request <- Filter(
      Negate(is.null),
      c(list(protocol()$pending), protocol()$queue)
    )[[1L]]
    expect_identical(request$payload$import_generation, 1L)
    expect_true(is.character(request$payload$progress_path))
    expect_true(startsWith(
      normalizePath(dirname(request$payload$progress_path)),
      normalizePath(progress_root)
    ))

    rail_html <- paste(
      vapply(last_import_rail_patch()$rows, `[[`, character(1), "html"),
      collapse = " "
    )
    workbench_html <- paste(unlist(output$workbench), collapse = " ")
    expect_match(rail_html, "All content", fixed = TRUE)
    expect_match(rail_html, "builder-import-status", fixed = TRUE)
    expect_identical(output$ds_count, "1")
    expect_match(workbench_html, "Add your data", fixed = TRUE)
    expect_match(workbench_html, "Choose files", fixed = TRUE)
    expect_false(grepl("Loading dataset", workbench_html, fixed = TRUE))
    expect_false(grepl('id="build"', workbench_html, fixed = TRUE))
    expect_false(grepl('id="make_app"', workbench_html, fixed = TRUE))
    expect_false(grepl('id="continue_to_review"', workbench_html, fixed = TRUE))
    expect_identical(workflow()$stage, "upload")
  })
})

test_that("client imports bind identities and release exactly once", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this protocol test.")
  }
  app_env$builder_session_example <- function(...) invisible(TRUE)
  app_env$builder_session_poll <- function(worker, ...) {
    list(worker = worker, event = NULL, result = NULL)
  }

  shiny::testServer(app_env$server, {
    worker(list(epoch = "worker-client-protocol"))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-client-protocol"))

    expect_true(start_load(
      "example",
      "all_content",
      "All content",
      client_id = "client-import-1"
    ))
    expect_identical(client_import_id_for("ds1"), "client-import-1")
    expect_true(release_client_import(
      "client-import-1",
      server_id = "ds1",
      outcome = "ready"
    ))
    expect_false(release_client_import(
      "client-import-1",
      server_id = "ds1",
      outcome = "error",
      message = "late failure"
    ))
    expect_identical(released_client_imports(), "client-import-1")
  })
})

test_that("ten queued sources stay lightweight and single-flight", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this backpressure test.")
  }
  app_env$builder_session_example <- function(...) invisible(TRUE)
  app_env$builder_session_poll <- function(worker, ...) {
    list(worker = worker, event = NULL, result = NULL)
  }
  progress_root <- tempfile("builder-import-backpressure-")
  dir.create(progress_root, mode = "0700")
  withr::defer(unlink(progress_root, recursive = TRUE, force = TRUE))

  shiny::testServer(app_env$server, {
    worker(list(epoch = "worker-backpressure", snapshot_root = progress_root))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-backpressure"))

    for (index in seq_len(10L)) {
      expect_true(start_load(
        "example",
        paste0("fixture_", index),
        paste("Dataset", index)
      ))
    }

    queue <- imports()
    expect_length(queue$entries, 10L)
    expect_lte(length(app_env$builder_import_active_ids(queue)), 1L)
    expect_identical(length(protocol()$awaiting_ack), 0L)
    expect_lte(
      length(Filter(
        Negate(is.null),
        list(protocol()$pending)
      )),
      1L
    )
    expect_true(all(vapply(
      queue$entries,
      function(entry) {
        is.null(entry$profile) && is.null(entry$settings)
      },
      logical(1)
    )))

    session$setInputs(remove_import = list(id = "ds10", nonce = 1))
    expect_null(app_env$builder_import_find(imports(), "ds10"))
    expect_false(any(vapply(
      c(list(protocol()$pending), protocol()$queue),
      function(request) {
        !is.null(request) && identical(request$dataset, "ds10")
      },
      logical(1)
    )))
  })
})

test_that("loading datasets block Configure with a user-facing reason", {
  app <- builder_app_source_text()
  workflow_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "workflow.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    app,
    "Wait for all datasets to finish loading.",
    fixed = TRUE
  )
  expect_match(
    app,
    "Retry or remove datasets that could not load.",
    fixed = TRUE
  )
  expect_match(app, "active_import_id", fixed = TRUE)
  expect_false(grepl(
    "builder_loading_workbench_ui",
    workflow_server,
    fixed = TRUE
  ))
})
