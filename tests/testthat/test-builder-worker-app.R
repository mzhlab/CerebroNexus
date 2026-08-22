## -------------------------------------------------------------------------
## Builder worker integration contracts owned by the Shiny main process.
## -------------------------------------------------------------------------

builder_app_source_runtime_prerequisites(globalenv())
builder_profile_source_runtime(globalenv())
builder_stage_contract_source_runtime(globalenv())
builder_repo_source("io.R", local = globalenv())
builder_repo_source("adapters.R", local = globalenv())
builder_repo_source("state.R", local = globalenv())

builder_app_worker_path <- builder_profile_inst_path("builder", "worker.R")
if (nzchar(builder_app_worker_path) && file.exists(builder_app_worker_path)) {
  sys.source(builder_app_worker_path, envir = globalenv())
}
builder_repo_source("session.R", local = globalenv())

builder_app_lines <- function() {
  builder_app_source_lines()
}

builder_session_lines <- function() {
  readLines(
    builder_profile_inst_path("builder", "session.R"),
    warn = FALSE
  )
}

builder_app_block <- function(lines, start, finish) {
  first <- grep(start, lines, fixed = TRUE)[1L]
  last <- grep(finish, lines, fixed = TRUE)
  last <- last[last > first][1L]
  expect_false(is.na(first), info = paste("Missing App marker:", start))
  expect_false(is.na(last), info = paste("Missing App marker:", finish))
  paste(lines[first:(last - 1L)], collapse = "\n")
}

test_that("worker test runtime loads the App path contract first", {
  runtime <- new.env(parent = baseenv())
  paths <- builder_app_source_runtime_prerequisites(runtime)

  expect_true(length(paths) > 0L)
  expect_true(all(file.exists(paths)))
  expect_true(exists(".pathWithin", envir = runtime, inherits = FALSE))
})

test_that("parent and worker load App assembly before build authorities", {
  app <- paste(builder_app_lines(), collapse = "\n")
  worker <- paste(
    readLines(builder_app_worker_path, warn = FALSE),
    collapse = "\n"
  )
  app_bundle_parent <- regexpr(
    'source("app_bundle.R", local = TRUE)',
    app,
    fixed = TRUE
  )[1L]
  coordinator_parent <- regexpr(
    'source("coordinator.R", local = TRUE)',
    app,
    fixed = TRUE
  )[1L]
  build_parent <- regexpr('source("build.R", local = TRUE)', app, fixed = TRUE)[
    1L
  ]
  app_bundle_worker <- regexpr(
    'file.path(dir, "app_bundle.R")',
    worker,
    fixed = TRUE
  )[1L]
  build_worker <- regexpr(
    'file.path(dir, "build.R")',
    worker,
    fixed = TRUE
  )[1L]

  expect_gt(app_bundle_parent, 0L)
  expect_lt(app_bundle_parent, coordinator_parent)
  expect_lt(app_bundle_parent, build_parent)
  expect_gt(app_bundle_worker, 0L)
  expect_lt(app_bundle_worker, build_worker)
  expect_false(grepl(
    'source(file.path(dir, "publish.R"))',
    worker,
    fixed = TRUE
  ))
})

test_that("the worker protocol exclusively owns quiescence", {
  app <- paste(builder_app_lines(), collapse = "\n")
  worker <- paste(
    readLines(builder_app_worker_path, warn = FALSE),
    collapse = "\n"
  )
  status <- paste(
    readLines(
      builder_profile_inst_path("builder", "ui", "build_status.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl(
    "builder_protocol_is_quiescent <- function",
    app,
    fixed = TRUE
  ))
  expect_match(
    worker,
    "builder_protocol_is_quiescent <- function",
    fixed = TRUE
  )
  expect_match(
    status,
    "builder_protocol_is_quiescent(protocol)",
    fixed = TRUE
  )
})

test_that("preview and coordinates apply only to the visible dataset section", {
  lines <- builder_app_lines()
  preview <- builder_app_block(
    lines,
    'identical(p$kind, "preview")',
    'identical(p$kind, "coords")'
  )
  coords <- builder_app_block(
    lines,
    'identical(p$kind, "coords")',
    'identical(p$kind, "align_all")'
  )

  expect_match(preview, "identical(current(), p$id)", fixed = TRUE)
  expect_match(coords, "identical(current(), p$id)", fixed = TRUE)
  expect_match(coords, "identical(active_slice(), p$image)", fixed = TRUE)
  expect_true(grepl("preview_frame(value)", preview, fixed = TRUE))
  expect_true(grepl("spatial_coords(value)", coords, fixed = TRUE))
})

test_that("the App polls replaceable worker results every 100 milliseconds", {
  lines <- builder_app_lines()
  poller <- builder_app_block(
    lines,
    "## -- one poller drains",
    "## Take the per-section extents"
  )

  expect_match(poller, "invalidateLater(100, session)", fixed = TRUE)
  expect_false(grepl("invalidateLater(300, session)", poller, fixed = TRUE))
})

test_that("session calls return deterministic failures as typed responses", {
  session <- paste(builder_session_lines(), collapse = "\n")

  expect_gte(
    lengths(regmatches(
      session,
      gregexpr(
        "builder_worker_response\\(request, error = conditionMessage\\(error\\)\\)",
        session,
        perl = TRUE
      )
    )),
    7L
  )
  expect_match(
    session,
    "builder_worker_response(request, error = error)",
    fixed = TRUE
  )
  expect_false(grepl(
    "builder_worker_response(request, list(error = error))",
    session,
    fixed = TRUE
  ))
})

test_that("a missing align-all fails once and later FIFO work proceeds", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list()
  )
  withr::defer(try(builder_worker_stop(worker), silent = TRUE))

  protocol <- builder_request_protocol(worker$epoch)
  protocol <- builder_enqueue(
    protocol,
    builder_command(
      "align_all",
      "missing",
      payload = list(kind = "align_all")
    )
  )
  protocol <- builder_enqueue(
    protocol,
    builder_command("drop", "missing", payload = list(kind = "drop"))
  )
  dispatched <- builder_protocol_dispatch(protocol)
  expect_identical(dispatched$request$kind, "align_all")

  builder_session_section_bounds(
    worker,
    "missing",
    sections = "section-a",
    mode = "pixels",
    extent_width = 100,
    extent_height = 100,
    request = dispatched$request
  )
  deadline <- Sys.time() + 10
  repeat {
    polled <- builder_session_poll(worker, timeout = 100)
    worker <- polled$worker
    if (!is.null(polled$result)) {
      break
    }
    if (Sys.time() >= deadline) fail("Timed out waiting for align-all failure.")
  }

  expect_null(polled$result$error)
  for (field in c(
    "epoch",
    "request_id",
    "token",
    "generation",
    "dataset_revision",
    "snapshot_identity"
  )) {
    expect_identical(
      polled$result$value[[field]],
      dispatched$request[[field]],
      info = paste("Typed failure identity field:", field)
    )
  }
  expect_match(polled$result$value$error, "not found", ignore.case = TRUE)
  completed <- builder_protocol_complete(
    dispatched$protocol,
    polled$result$value
  )
  expect_true(completed$accepted)
  expect_match(completed$error, "not found", ignore.case = TRUE)
  protocol <- builder_protocol_acknowledge(
    completed$protocol,
    dispatched$request$request_id
  )

  following <- builder_protocol_dispatch(protocol)
  expect_identical(following$request$kind, "drop")
  expect_identical(following$request$dataset, "missing")
  expect_identical(following$request$epoch, worker$epoch)
})

test_that("an invalidated query failure leaves a queued Build runnable", {
  protocol <- builder_request_protocol("worker-a")
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "dataset-a", generation = 1L)
  )
  dispatched <- builder_protocol_dispatch(protocol)
  protocol <- builder_enqueue(
    dispatched$protocol,
    builder_command("build", "session", payload = list(id = "build-a"))
  )

  completed <- builder_protocol_complete(
    protocol,
    builder_worker_response(
      dispatched$request,
      error = "The old preview no longer exists."
    )
  )
  expect_false(completed$accepted)
  expect_null(completed$protocol$pending)
  expect_identical(builder_pending_ids(completed$protocol), "build:session")

  build <- builder_protocol_dispatch(completed$protocol)
  expect_identical(build$request$kind, "build")
  expect_identical(build$protocol$build_status, "running")
})

test_that("a replaced spatial preview response is rejected", {
  protocol <- builder_request_protocol("worker-a")
  protocol <- builder_enqueue(
    protocol,
    builder_query(
      "spatial_preview",
      "dataset-a",
      generation = 1L,
      slot = "spatial_alignment"
    )
  )
  first <- builder_protocol_dispatch(protocol)
  protocol <- builder_enqueue(
    first$protocol,
    builder_query(
      "spatial_preview",
      "dataset-a",
      generation = 2L,
      slot = "spatial_alignment"
    )
  )

  stale <- builder_protocol_complete(
    protocol,
    builder_worker_response(first$request, list(available = TRUE))
  )
  expect_false(stale$accepted)

  current <- builder_protocol_dispatch(stale$protocol)
  expect_identical(current$request$generation, 2L)
  expect_identical(current$request$kind, "spatial_preview")
})

test_that("business errors are terminalized without worker recovery", {
  lines <- builder_app_lines()
  poller <- builder_app_block(
    lines,
    "## -- one poller drains",
    "## Take the per-section extents"
  )
  typed_error <- builder_app_block(
    strsplit(poller, "\n", fixed = TRUE)[[1L]],
    "if (!is.null(completed$error))",
    'if (identical(p$kind, "load"))'
  )

  expect_match(typed_error, "builder_protocol_acknowledge", fixed = TRUE)
  expect_false(grepl("restart_worker_protocol", typed_error, fixed = TRUE))
})

test_that("a successful drop forgets all stale protocol state", {
  lines <- builder_app_lines()
  drop <- builder_app_block(
    lines,
    '} else if (identical(p$kind, "drop")) {',
    "## Take the per-section extents"
  )

  released <- regexpr("builder_worker_release_snapshot", drop, fixed = TRUE)[1L]
  acknowledged <- regexpr("builder_protocol_acknowledge", drop, fixed = TRUE)[
    1L
  ]
  forgotten <- regexpr("builder_protocol_forget_dataset", drop, fixed = TRUE)[
    1L
  ]

  expect_gt(released, 0L)
  expect_gt(acknowledged, released)
  expect_gt(forgotten, acknowledged)
  expect_match(drop, "failed", fixed = TRUE)
  expect_match(drop, "discarded", fixed = TRUE)
})

test_that("workflow server exclusively owns loading and stage rendering", {
  lines <- builder_app_lines()
  app <- paste(lines, collapse = "\n")
  workbench <- builder_app_block(
    lines,
    "output$workbench <- renderUI({",
    "observeEvent(input$continue_to_review, {"
  )

  expect_identical(
    lengths(regmatches(
      app,
      gregexpr("output$workbench <- renderUI({", app, fixed = TRUE)
    )),
    1L
  )
  expect_match(app, "selected_workflow_stage <- reactive({", fixed = TRUE)
  expect_match(workbench, "loading_id <- active_import_id()", fixed = TRUE)
  expect_false(grepl("import_focus_id()", workbench, fixed = TRUE))
  expect_match(workbench, "builder_loading_workbench_ui", fixed = TRUE)
  expect_match(workbench, "stage <- selected_workflow_stage()", fixed = TRUE)
  expect_lt(
    regexpr("builder_loading_workbench_ui", workbench, fixed = TRUE),
    regexpr("stage <- selected_workflow_stage()", workbench, fixed = TRUE)
  )
  expect_match(
    workbench,
    paste0(
      "upload = tagAppendAttributes(\n",
      "      builder_empty_workbench_ui(project_active = !is.null(builder_project())),\n",
      "      class = \"builder-stage-upload\",\n",
      "      `data-workflow-stage` = \"upload\""
    ),
    fixed = TRUE
  )
  expect_match(
    workbench,
    "configure = render_configure_workbench()",
    fixed = TRUE
  )
  expect_match(workbench, "review = render_review_workbench()", fixed = TRUE)
  expect_match(workbench, "build = render_build_workbench()", fixed = TRUE)
  expect_match(workbench, "Unsupported Builder workflow stage", fixed = TRUE)
  expect_false(grepl('actionButton(\n      "build"', app, fixed = TRUE))
  expect_false(grepl('uiOutput("actionbar")', app, fixed = TRUE))
})

test_that("Build status projection keeps one stable typed host", {
  skip_if_not_installed("shiny")
  withr::local_package("shiny")
  idle <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = builder_request_protocol("worker-idle"),
    note = NULL,
    result = NULL,
    output_selected = TRUE
  )
  choosing <- builder_build_stage_status_model(
    flow = list(stage = "choosing_folder"),
    protocol = NULL,
    note = NULL,
    result = NULL,
    output_selected = FALSE
  )
  checking <- builder_build_stage_status_model(
    flow = list(stage = "checking_folder"),
    protocol = NULL,
    note = NULL,
    result = NULL,
    output_selected = FALSE
  )
  preparing <- builder_build_stage_status_model(
    flow = list(stage = "preparing"),
    protocol = builder_request_protocol("worker-preparing"),
    note = NULL,
    result = NULL,
    output_selected = TRUE
  )
  queued <- builder_build_stage_status_model(
    flow = list(stage = "building"),
    protocol = list(build_status = "queued"),
    note = NULL,
    result = NULL,
    output_selected = TRUE
  )
  running <- builder_build_stage_status_model(
    flow = list(stage = "building"),
    protocol = list(build_status = "running"),
    note = "Building 3 datasets…",
    result = NULL,
    output_selected = TRUE
  )
  cancelling <- builder_build_stage_status_model(
    flow = list(stage = "building"),
    protocol = list(build_status = "cancelling"),
    note = NULL,
    result = NULL,
    output_selected = TRUE
  )
  conflict <- builder_build_stage_status_model(
    flow = list(stage = "conflict"),
    protocol = list(build_status = "idle"),
    note = NULL,
    result = NULL,
    output_selected = TRUE
  )
  malformed <- builder_build_stage_status_model(
    flow = NULL,
    protocol = list(build_status = NA_character_),
    note = list("not a status note"),
    result = NULL,
    output_selected = TRUE
  )
  incomplete_protocol <- structure(
    list(build_status = "idle"),
    class = c("builder_request_protocol", "list")
  )
  incomplete <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = incomplete_protocol,
    note = "The background worker is not ready.",
    result = NULL,
    output_selected = TRUE
  )
  missing_protocol <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = NULL,
    note = "The background worker is not ready.",
    result = NULL,
    output_selected = TRUE
  )
  pending_protocol <- builder_request_protocol("worker-pending")
  pending_protocol <- builder_enqueue(
    pending_protocol,
    builder_query("preview", "dataset-a", generation = 1L)
  )
  pending <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = pending_protocol,
    note = "Preparing preview…",
    result = NULL,
    output_selected = TRUE
  )
  quiescent <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = builder_request_protocol("worker-ready"),
    note = NULL,
    result = NULL,
    output_selected = TRUE
  )

  expect_named(
    idle,
    c("state", "message", "pipeline_state", "can_build", "result_model"),
    ignore.order = FALSE
  )
  expect_identical(idle$state, "ready")
  expect_true(idle$can_build)
  expect_identical(choosing$state, "choosing_folder")
  expect_false(choosing$can_build)
  expect_identical(checking$state, "checking_folder")
  expect_identical(
    builder_build_stage_status_label(checking),
    "Checking output folder…"
  )
  expect_false(checking$can_build)
  expect_identical(preparing$state, "preparing")
  expect_identical(
    builder_build_stage_status_label(preparing),
    "Preparing build…"
  )
  expect_false(preparing$can_build)
  expect_identical(queued$state, "queued")
  expect_identical(queued$pipeline_state, "queued")
  expect_false(queued$can_build)
  expect_identical(running$state, "building")
  expect_identical(running$message, "Building 3 datasets…")
  expect_identical(running$pipeline_state, "building")
  expect_false(running$can_build)
  expect_identical(cancelling$state, "building")
  expect_false(cancelling$can_build)
  expect_identical(conflict$state, "ready")
  expect_false(conflict$can_build)
  expect_identical(malformed$state, "ready")
  expect_null(malformed$message)
  expect_false(malformed$can_build)
  expect_false(incomplete$can_build)
  expect_false(missing_protocol$can_build)
  expect_identical(
    missing_protocol$message,
    "The background worker is not ready."
  )
  expect_false(pending$can_build)
  expect_identical(pending$message, "Preparing preview…")
  expect_true(quiescent$can_build)
  render_status <- function(model) {
    htmltools::renderTags(htmltools::tagList(
      builder_build_stage_status_body_ui(model),
      builder_build_stage_primary_action_ui(model)
    ))$html
  }

  results <- list(
    success = builder_result_success(
      published = TRUE,
      built = "/release/dataset.crb",
      app_dir = "/release/cerebro_app",
      app_verified = TRUE,
      report_path = "/release/build-report.json"
    ),
    needs_decision = builder_result_needs_decision(
      "Choose one.",
      retry_closure = "marker_genes",
      failed_dataset_id = "dataset-a"
    ),
    failure = builder_result_failure(
      "Worker stopped.",
      restartable_worker = TRUE
    ),
    recovery_required = builder_result_recovery_required(
      "Restore the preserved backup."
    )
  )
  result_html <- lapply(results, function(value) {
    model <- builder_build_stage_status_model(
      flow = list(stage = "idle"),
      protocol = list(build_status = "idle"),
      note = NULL,
      result = value,
      output_selected = TRUE
    )
    expect_identical(model$state, "result")
    expect_s3_class(model$result_model, "builder_build_status")
    render_status(model)
  })

  ready_html <- render_status(idle)
  choosing_html <- render_status(choosing)
  queued_html <- render_status(queued)
  running_html <- render_status(running)
  missing_html <- render_status(missing_protocol)
  all_html <- c(
    list(ready_html, choosing_html, queued_html, running_html, missing_html),
    result_html
  )
  for (html in all_html) {
    expect_false(grepl('id="build-stage-status"', html, fixed = TRUE))
  }
  expect_match(ready_html, ">Build<", fixed = TRUE)
  expect_match(ready_html, "btn btn-action", fixed = TRUE)
  expect_false(grepl("Choosing output folder…", choosing_html, fixed = TRUE))
  expect_match(queued_html, "Build queued…", fixed = TRUE)
  expect_match(running_html, "Building 3 datasets…", fixed = TRUE)
  expect_match(
    missing_html,
    "The background worker is not ready.",
    fixed = TRUE
  )
  expect_match(missing_html, " disabled", fixed = TRUE)
  expect_false(grepl("Launch App", result_html$success, fixed = TRUE))
  expect_match(result_html$success, "Reveal Folder", fixed = TRUE)
  expect_match(result_html$success, "Copy Path", fixed = TRUE)
  expect_match(result_html$success, "Copy Report", fixed = TRUE)
  expect_match(result_html$needs_decision, "Retry optional work", fixed = TRUE)
  expect_match(result_html$needs_decision, "Remove and review", fixed = TRUE)
  expect_false(grepl(
    "remove and rebuild",
    result_html$needs_decision,
    ignore.case = TRUE
  ))
  expect_match(
    result_html$needs_decision,
    "remove it, review, and build again",
    ignore.case = TRUE
  )
  expect_match(result_html$failure, "Restart worker", fixed = TRUE)
  expect_match(
    result_html$recovery_required,
    "Manual recovery steps",
    fixed = TRUE
  )
  expect_error(
    render_status(list(state = "future")),
    "Build-stage status is unsupported"
  )
  expect_error(
    builder_build_stage_status_model(
      flow = NULL,
      protocol = NULL,
      note = NULL,
      result = list(error = "legacy"),
      output_selected = FALSE
    ),
    "typed"
  )
})

test_that("Build owns output mode and expanded Viewer App settings", {
  crb <- builder_build_options_ui(builder_build_options())
  crb_html <- builder_stage_html(crb)
  expect_match(crb_html, "CRB files only", fixed = TRUE)
  expect_false(grepl("Welcome message", crb_html, fixed = TRUE))

  app <- builder_build_options_ui(builder_build_options(make_app = TRUE))
  app_html <- builder_stage_html(app)
  expect_false(builder_build_options(make_app = TRUE)$app$launch_browser)
  expect_match(app_html, "CRB files + Viewer App", fixed = TRUE)
  expect_match(app_html, "Welcome message", fixed = TRUE)
  expect_false(grepl("Open App after build", app_html, fixed = TRUE))
  expect_false(grepl("build_launch_browser", app_html, fixed = TRUE))
  expect_match(app_html, "Host", fixed = TRUE)
  expect_match(app_html, "Port", fixed = TRUE)
  expect_match(app_html, "Require login", fixed = TRUE)
  expect_match(app_html, "builder-stage-section", fixed = TRUE)
  expect_match(app_html, "builder-build-options-fields", fixed = TRUE)
  expect_match(app_html, "<details", fixed = TRUE)
  expect_match(app_html, "Connection settings", fixed = TRUE)

  auth_unavailable <- builder_stage_html(builder_build_options_ui(
    builder_build_options(make_app = TRUE),
    auth = list(
      enabled = FALSE,
      account_count = 0L,
      error = NULL,
      available = FALSE,
      reason = paste(
        "Login is unavailable because this required R package is missing or too old:",
        "shinymanager (>= 1.1.0).",
        'Run install.packages("shinymanager"), then restart Builder.'
      )
    )
  ))
  expect_match(auth_unavailable, "Login is unavailable", fixed = TRUE)
  expect_match(auth_unavailable, "shinymanager (&gt;= 1.1.0)", fixed = TRUE)

  unavailable <- builder_stage_html(builder_build_options_ui(
    builder_build_options(),
    app_available = FALSE,
    app_reason = "Install Viewer dependencies."
  ))
  expect_match(unavailable, 'value="app" disabled="disabled"', fixed = TRUE)
  expect_match(unavailable, "builder-app-capability-warning", fixed = TRUE)
  expect_match(unavailable, "Viewer App unavailable", fixed = TRUE)
  expect_match(unavailable, "Install Viewer dependencies.", fixed = TRUE)
  expect_match(unavailable, "CRB-only export is still available.", fixed = TRUE)
  expect_false(grepl("remotes::install_local", unavailable, fixed = TRUE))

  locked <- builder_stage_html(builder_build_options_ui(
    builder_build_options(make_app = TRUE),
    controls_disabled = TRUE
  ))
  expect_match(
    locked,
    'class="builder-build-options-fields" disabled="disabled"',
    fixed = TRUE
  )
})

test_that("dataset mutation lock covers every active build state", {
  idle_protocol <- builder_request_protocol("worker-lock")
  expect_false(builder_mutations_locked(
    list(stage = "idle", plan = NULL),
    idle_protocol
  ))
  for (stage in c(
    "queued",
    "building",
    "choosing",
    "choosing_folder",
    "checking_folder",
    "conflict"
  )) {
    expect_true(
      builder_mutations_locked(
        list(stage = stage, plan = NULL),
        idle_protocol
      ),
      info = stage
    )
  }
  for (status in c("queued", "running", "cancelling")) {
    active_protocol <- idle_protocol
    active_protocol$build_status <- status
    expect_true(
      builder_mutations_locked(
        list(stage = "idle", plan = NULL),
        active_protocol
      ),
      info = status
    )
  }
  expect_true(builder_mutations_locked(NULL, idle_protocol))
  expect_true(builder_mutations_locked(
    list(stage = "idle", plan = NULL),
    list(build_status = NA_character_)
  ))
})

test_that("completed preview protocols enable a selected Build", {
  protocol <- builder_request_protocol("worker-completed-preview")
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "dataset-a", generation = 1L)
  )
  dispatched <- builder_protocol_dispatch(protocol)
  completed <- builder_protocol_complete(
    dispatched$protocol,
    builder_worker_response(dispatched$request, value = list())
  )$protocol

  expect_false("pending" %in% names(completed))
  model <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = completed,
    note = NULL,
    result = NULL,
    output_selected = TRUE
  )
  expect_true(model$can_build)
})

test_that("native output directory selection normalizes selection and preserves cancellation", {
  root <- withr::local_tempdir()
  nested <- file.path(root, "nested", "..", "output")
  dir.create(file.path(root, "nested"))
  dir.create(file.path(root, "output"))

  selected <- builder_native_picker_result("output_directory", function() {
    nested
  })
  cancelled <- builder_native_picker_result("output_directory", function() NULL)
  failed <- builder_native_picker_result("output_directory", function() {
    stop("picker unavailable")
  })

  expect_identical(selected$status, "selected")
  expect_identical(selected$path, normalizePath(file.path(root, "output")))
  expect_identical(cancelled, list(status = "cancelled", path = NULL))
  expect_identical(failed$status, "error")
  expect_match(failed$error, "picker unavailable", fixed = TRUE)
})

test_that("native pickers can run without blocking the Shiny process", {
  root <- withr::local_tempdir()
  output <- file.path(root, "output")
  dir.create(output)
  process <- new.env(parent = emptyenv())
  process$is_alive <- function() FALSE
  process$get_exit_status <- function() 0L
  process$read_all_output_lines <- function() output
  process$read_all_error_lines <- function() character()
  process$kill <- function() invisible(TRUE)

  started <- builder_start_native_picker(
    "output_directory",
    .spec = list(command = "picker", args = character()),
    .process_new = function(...) process
  )

  expect_identical(started$kind, "output_directory")
  expect_identical(started$process, process)
  expect_false(builder_native_picker_is_alive(started))
  expect_identical(
    builder_collect_native_picker(started),
    list(status = "selected", path = normalizePath(output))
  )
})

test_that("native dataset selection returns only supported regular files", {
  root <- withr::local_tempdir()
  first <- file.path(root, "first.rds")
  second <- file.path(root, "second.qs2")
  unsupported <- file.path(root, "notes.txt")
  writeBin(charToRaw("first"), first)
  writeBin(charToRaw("second"), second)
  writeBin(charToRaw("notes"), unsupported)

  selected <- builder_native_picker_result("dataset_files", function() {
    c(first, second)
  })
  cancelled <- builder_native_picker_result("dataset_files", function() NULL)
  rejected <- builder_native_picker_result("dataset_files", function() {
    unsupported
  })

  expect_identical(selected$status, "selected")
  expect_identical(selected$paths, normalizePath(c(first, second)))
  expect_identical(cancelled, list(status = "cancelled", paths = character()))
  expect_identical(rejected$status, "error")
  expect_match(rejected$error, "supported dataset", fixed = TRUE)
})

test_that("native table selection accepts every supported workbook format", {
  root <- withr::local_tempdir()
  paths <- file.path(root, c("clinical.csv", "qc.tsv", "atlas.xlsx"))
  invisible(lapply(paths, function(path) writeLines("value", path)))
  unsupported <- file.path(root, "notes.pdf")
  writeLines("notes", unsupported)

  selected <- builder_native_picker_result("table_files", function() paths)
  cancelled <- builder_native_picker_result("table_files", function() NULL)
  rejected <- builder_native_picker_result("table_files", function() {
    unsupported
  })

  expect_identical(selected$status, "selected")
  expect_identical(selected$paths, normalizePath(paths))
  expect_identical(cancelled, list(status = "cancelled", paths = character()))
  expect_identical(rejected$status, "error")
  expect_match(rejected$error, "supported table", fixed = TRUE)
})

test_that("Build flow requires one confirmed frozen Review plan", {
  app <- paste(builder_app_lines(), collapse = "\n")

  expect_match(app, "builder_workflow_confirmation_matches", fixed = TRUE)
  expect_match(app, "workflow()$review_plan", fixed = TRUE)
  expect_match(app, "input$confirm_review", fixed = TRUE)
  expect_false(grepl("reviewed_revision", app, fixed = TRUE))
  expect_false(grepl("review_current_dataset", app, fixed = TRUE))
  expect_false(grepl("next_unreviewed", app, fixed = TRUE))
  expect_match(app, 'identical(state$stage, "build")', fixed = TRUE)
  expect_match(app, "builder_require_confirmed_build_plan", fixed = TRUE)
  expect_match(app, "builder_review_plan_identity(plan)", fixed = TRUE)
  expect_gte(
    lengths(regmatches(
      app,
      gregexpr("builder_require_confirmed_build_plan", app, fixed = TRUE)
    )),
    4L
  )
  expect_match(app, "selected_output(NULL)", fixed = TRUE)
  expect_match(
    app,
    '"Settings changed. Review the updated plan before building."',
    fixed = TRUE
  )
})

test_that("Build stage renders only the confirmed stored plan", {
  app <- paste(builder_app_lines(), collapse = "\n")
  workflow_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "workflow.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  workflow_ui_lines <- readLines(
    builder_profile_inst_path("builder", "ui", "workflow.R"),
    warn = FALSE
  )
  build_ui_start <- grep(
    "builder_build_workbench_ui <- function",
    workflow_ui_lines,
    fixed = TRUE
  )
  workflow_ui <- paste(
    workflow_ui_lines[build_ui_start:length(workflow_ui_lines)],
    collapse = "\n"
  )

  expect_match(workflow_server, "state$review_plan", fixed = TRUE)
  expect_identical(
    lengths(regmatches(
      app,
      gregexpr("render_build_workbench <- function", app, fixed = TRUE)
    )),
    1L
  )
  expect_match(
    workflow_server,
    "builder_workflow_confirmation_matches(state, plan)",
    fixed = TRUE
  )
  expect_match(workflow_server, "selected_output()", fixed = TRUE)
  expect_match(workflow_server, "input$back_to_review", fixed = TRUE)
  expect_match(workflow_server, 'list(type = "back_to_review")', fixed = TRUE)
  expect_match(workflow_ui, '`data-workflow-stage` = "build"', fixed = TRUE)
  expect_match(workflow_ui, 'builder_stage_header_ui(', fixed = TRUE)
  expect_match(workflow_ui, '"Build your output"', fixed = TRUE)
  expect_match(
    paste(workflow_ui_lines, collapse = "\n"),
    '"No output folder selected"',
    fixed = TRUE
  )
  expect_match(workflow_ui, 'uiOutput("build_stage_controls")', fixed = TRUE)
  expect_match(workflow_ui, 'id = "build-stage-status"', fixed = TRUE)
  expect_match(
    workflow_ui,
    'uiOutput("build_stage_status_content")',
    fixed = TRUE
  )
  expect_match(workflow_ui, 'uiOutput("build_stage_footer")', fixed = TRUE)
  expect_false(grepl(
    'actionButton(\n        "build"',
    workflow_ui,
    fixed = TRUE
  ))
  expect_false(grepl("make_app|Configure", workflow_ui))

  render_build <- builder_app_block(
    readLines(
      builder_profile_inst_path("builder", "server", "workflow.R"),
      warn = FALSE
    ),
    "render_build_workbench <- function() {",
    "output$build_stage_controls <- renderUI({"
  )
  for (volatile in c(
    "result()",
    "build_flow()",
    "protocol()",
    "busy_note()",
    "selected_output()"
  )) {
    expect_false(grepl(volatile, render_build, fixed = TRUE), info = volatile)
  }
})

test_that("group color changes use the existing settings revision path", {
  app <- paste(builder_app_lines(), collapse = "\n")

  expect_match(app, 'input[["core-group_color"]]', fixed = TRUE)
  expect_match(app, "builder_update_color_override", fixed = TRUE)
  expect_match(app, 'input[["core-reset_colors"]]', fixed = TRUE)
  expect_match(app, "builder_reset_color_overrides", fixed = TRUE)
  expect_match(app, "replace_entry(entry)", fixed = TRUE)
  expect_false(grepl("umap_palette|pca_palette|tsne_palette", app))
})

test_that("parameter writes do not eagerly freeze Review or rebuild editors", {
  review_lines <- readLines(
    builder_profile_inst_path("builder", "server", "review.R"),
    warn = FALSE
  )
  review <- paste(review_lines, collapse = "\n")
  dataset_lines <- readLines(
    builder_profile_inst_path("builder", "server", "datasets.R"),
    warn = FALSE
  )
  datasets <- paste(dataset_lines, collapse = "\n")

  frozen <- builder_app_block(
    review_lines,
    "frozen_review_plan <- reactive({",
    "review_report <- reactive({"
  )
  expect_match(frozen, "bindEvent(", fixed = TRUE)
  expect_match(frozen, "dataset_check_marks()", fixed = TRUE)
  expect_match(frozen, "imports()", fixed = TRUE)

  colors <- regmatches(
    datasets,
    regexpr(
      'output\\[\\["core-group_colors"\\]\\] <- renderUI\\(\\{[\\s\\S]+?output\\[\\["core-projection_gallery"',
      datasets,
      perl = TRUE
    )
  )
  expect_match(colors, "group_color_ui_revision()", fixed = TRUE)
  expect_match(colors, "isolate(entry_of(id))", fixed = TRUE)

  metadata <- builder_app_block(
    dataset_lines,
    'output[["core-metadata_preview"]] <- renderUI({',
    'output[["core-group_colors"]] <- renderUI({'
  )
  expect_match(metadata, "group_preview_ui_revision()", fixed = TRUE)
  expect_match(metadata, "isolate(entry_of(id))", fixed = TRUE)

  group_action <- builder_app_block(
    dataset_lines,
    'input[["core-group_action"]],',
    'input[["core-cell_cycle"]],'
  )
  expect_match(group_action, "group_preview_ui_revision(", fixed = TRUE)

  tables <- builder_app_block(
    review_lines,
    'output[["enhance-table_list"]] <- renderUI({',
    'output[["inspect_stage"]] <- renderUI({'
  )
  expect_match(tables, "enhance_table_ui_revision()", fixed = TRUE)
  expect_match(tables, "isolate(entry_of(id))", fixed = TRUE)
})

test_that("parameter writes do not rebuild workflow navigation", {
  workflow_lines <- readLines(
    builder_profile_inst_path("builder", "server", "workflow.R"),
    warn = FALSE
  )
  progress <- builder_app_block(
    workflow_lines,
    "output$workflow_progress <- renderUI({",
    "navigate_workflow_stage <- function(stage) {"
  )

  expect_match(progress, "workflow_has_datasets()", fixed = TRUE)
  expect_false(grepl("store()$datasets", progress, fixed = TRUE))
})

test_that("the App describes object isolation accurately", {
  app <- paste(builder_app_lines(), collapse = "\n")

  expect_false(grepl("Objects are read into this process", app, fixed = TRUE))
  expect_match(app, "isolated worker process", fixed = TRUE)
})

test_that("artifact replacement invalidates only the configure surface", {
  foundation <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "foundation.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  review <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    foundation,
    "configure_workbench_surface <- reactiveVal",
    fixed = TRUE
  )
  expect_match(foundation, "entry$load_state %||% \"loaded\"", fixed = TRUE)
  expect_match(review, "configure_workbench_surface()", fixed = TRUE)
  expect_match(review, "entry <- isolate(entry_of(id))", fixed = TRUE)
})

test_that("unchanged settings do not write state or advance revisions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  settings_observer <- builder_app_block(
    builder_app_lines(),
    "## -- keep the current entry's settings",
    "## Assay-dependent controls"
  )
  expect_match(
    settings_observer,
    "e <- isolate(entry_of(id))",
    fixed = TRUE
  )
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }

  shiny::testServer(app_env$server, {
    protocol(builder_request_protocol("worker-a"))
    entry <- list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "current", extras = list()),
      settings = list(name = "Dataset A")
    )
    use_state_only_fixture(list(entry))

    expect_false(replace_entry(entry))
    expect_identical(sets()[[1L]]$revision, 0L)
    expect_null(protocol()$datasets[["dataset-a"]])

    changed <- entry
    changed$settings$name <- "Dataset A renamed"
    changed$profile$marker <- "stale candidate"
    expect_true(replace_entry(changed))
    expect_identical(sets()[[1L]]$revision, 1L)
    expect_identical(sets()[[1L]]$profile$marker, "current")
    expect_identical(
      protocol()$datasets[["dataset-a"]]$revision,
      1L
    )

    expect_false(replace_entry(sets()[[1L]]))
    expect_identical(sets()[[1L]]$revision, 1L)
  })
})

test_that("BuildPlan snapshots must match the worker registry exactly", {
  expect_true(exists(
    ".builder_session_plan_snapshot_error",
    mode = "function",
    inherits = TRUE
  ))
  if (
    !exists(
      ".builder_session_plan_snapshot_error",
      mode = "function",
      inherits = TRUE
    )
  ) {
    return(invisible(NULL))
  }

  snapshot <- list(
    path = "/private/snapshot-a",
    object_file = "/private/snapshot-a/object.rds",
    owner_token = "owner-a",
    created_at = as.POSIXct("2026-08-04 12:00:00", tz = "UTC"),
    object_md5 = strrep("a", 32L),
    closure_bytes = 1024
  )
  identity <- c(
    list(available = TRUE, snapshot = snapshot, source = list(id = "a")),
    snapshot
  )
  plan <- list(
    items = list(list(
      id = "dataset-a",
      name = "Dataset A",
      source_snapshot_identity = identity
    ))
  )

  expect_null(.builder_session_plan_snapshot_error(
    plan,
    list(`dataset-a` = snapshot)
  ))

  replaced <- snapshot
  replaced$object_md5 <- strrep("b", 32L)
  expect_match(
    .builder_session_plan_snapshot_error(
      plan,
      list(`dataset-a` = replaced)
    ),
    "does not match the frozen BuildPlan",
    fixed = TRUE
  )
  expect_match(
    .builder_session_plan_snapshot_error(plan, list()),
    "missing",
    fixed = TRUE
  )

  unavailable <- plan
  unavailable$items[[1L]]$source_snapshot_identity$available <- FALSE
  expect_match(
    .builder_session_plan_snapshot_error(
      unavailable,
      list(`dataset-a` = snapshot)
    ),
    "does not contain an owned frozen snapshot",
    fixed = TRUE
  )
})

test_that("direct and request-bearing Builds reject a replaced same-id snapshot", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  frozen <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-frozen"),
    available_bytes = 2^40
  )
  replacement <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-replacement"),
    available_bytes = 2^40
  )
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list(`dataset-a` = replacement)
  )
  withr::defer({
    try(builder_worker_stop(worker), silent = TRUE)
    if (isTRUE(.builder_snapshot_owned(frozen))) {
      .builder_snapshot_release(frozen)
    }
    if (isTRUE(.builder_snapshot_owned(replacement))) {
      .builder_snapshot_release(replacement)
    }
  })

  identity <- c(
    list(available = TRUE, snapshot = frozen, source = list(id = "a")),
    frozen
  )
  plan <- list(
    items = list(list(
      id = "dataset-a",
      name = "Dataset A",
      source_snapshot_identity = identity
    ))
  )
  wait_for_result <- function(worker) {
    deadline <- Sys.time() + 10
    repeat {
      polled <- builder_session_poll(worker, timeout = 100)
      worker <- polled$worker
      if (!is.null(polled$result)) {
        return(list(worker = worker, result = polled$result))
      }
      if (Sys.time() >= deadline) {
        fail("Timed out waiting for the frozen-snapshot rejection.")
      }
    }
  }

  builder_session_build(worker, plan)
  direct <- wait_for_result(worker)
  worker <- direct$worker
  expect_true(direct$result$done)
  expect_match(
    direct$result$value$error,
    "does not match the frozen BuildPlan",
    fixed = TRUE
  )

  protocol <- builder_enqueue(
    builder_request_protocol(worker$epoch),
    builder_command("build", "session", payload = list(id = "build-1"))
  )
  dispatched <- builder_protocol_dispatch(protocol)

  builder_session_build(worker, plan, dispatched$request)
  requested <- wait_for_result(worker)
  polled <- list(worker = requested$worker, result = requested$result)
  worker <- polled$worker

  expect_true(polled$result$done)
  expect_match(
    polled$result$value$error,
    "does not match the frozen BuildPlan",
    fixed = TRUE
  )
  expect_identical(polled$result$value$build_id, "build-1")
})

test_that("worker failures recover or terminate every protocol barrier", {
  app <- paste(builder_app_lines(), collapse = "\n")

  expect_match(app, "builder_protocol_recover", fixed = TRUE)
  expect_match(app, 'identical(got$event, "restart_failed")', fixed = TRUE)
  expect_match(app, "retry_persistent = FALSE", fixed = TRUE)
  expect_match(app, "restart_worker_protocol <- function", fixed = TRUE)
  expect_gte(
    lengths(regmatches(
      app,
      gregexpr("restart_worker_protocol\\(", app, perl = TRUE)
    )),
    4L
  )

  register_failure <- builder_app_block(
    builder_app_lines(),
    "if (inherits(updated_worker, \"try-error\"))",
    "worker(updated_worker)"
  )
  release_failure <- builder_app_block(
    builder_app_lines(),
    "if (inherits(released, \"try-error\"))",
    "worker(released$worker)"
  )
  expect_match(
    app,
    "builder_snapshot_release_transition(",
    fixed = TRUE
  )
  expect_match(register_failure, "restart_worker_protocol(", fixed = TRUE)
  expect_match(release_failure, "restart_worker_protocol(", fixed = TRUE)
})

test_that("serialized session build arguments contain paths but no login secrets", {
  username <- "serialized-user-42b8"
  password <- "serialized-password-42b8"
  auth_runtime <- new.env(parent = globalenv())
  builder_repo_source("app_bundle.R", local = auth_runtime)
  validate_payload <- get(
    "builder_auth_validate_payload",
    envir = auth_runtime
  )
  create_material <- get("builder_auth_create_material", envir = auth_runtime)
  read_env_file <- get("builder_auth_read_env_file", envir = auth_runtime)
  validate_material <- get(
    "builder_auth_validate_material",
    envir = auth_runtime
  )
  stage <- withr::local_tempdir()
  accounts <- validate_payload(
    TRUE,
    list(list(
      id = "auth-account-1",
      username = username,
      password = password
    ))
  )$accounts
  material <- create_material(
    accounts,
    stage,
    .random_bytes = function(n) as.raw(rep(12L, n)),
    .create_db = function(credentials_data, sqlite_path, passphrase) {
      writeBin(as.raw(1:8), sqlite_path)
    },
    .capability = function() list(available = TRUE, reason = NULL)
  )
  passphrase <- read_env_file(material$env_file)
  captured_args <- NULL
  captured_callback <- NULL
  process <- list(call = function(callback, args) {
    captured_callback <<- callback
    captured_args <<- args
    invisible(TRUE)
  })
  plan <- list(
    make_app = FALSE,
    app_contract_version = 1L,
    app_auth = list(
      enabled = TRUE,
      account_count = 1L,
      timeout_minutes = 15L
    ),
    items = list()
  )
  request_with_accounts <- builder_command(
    "build",
    "session",
    payload = list(kind = "build", auth_accounts = accounts)
  )
  expect_true(builder_auth_value_contains(request_with_accounts, username))
  expect_true(builder_auth_value_contains(request_with_accounts, password))
  request <- builder_request_redact_auth(request_with_accounts)
  request$build_id <- "serialized-build"
  coordinator <- list(stage = stage, control = dirname(stage))
  original_handle <- get0(
    ".builder_coordinator_handle",
    envir = environment(builder_session_build),
    inherits = TRUE
  )
  assign(
    ".builder_coordinator_handle",
    function(value) value,
    envir = environment(builder_session_build)
  )
  original_validate <- get0(
    "builder_auth_validate_material",
    envir = environment(builder_session_build),
    inherits = FALSE
  )
  assign(
    "builder_auth_validate_material",
    validate_material,
    envir = environment(builder_session_build)
  )
  on.exit(
    {
      if (is.null(original_handle)) {
        rm(
          ".builder_coordinator_handle",
          envir = environment(builder_session_build)
        )
      } else {
        assign(
          ".builder_coordinator_handle",
          original_handle,
          envir = environment(builder_session_build)
        )
      }
      if (is.null(original_validate)) {
        rm(
          "builder_auth_validate_material",
          envir = environment(builder_session_build)
        )
      } else {
        assign(
          "builder_auth_validate_material",
          original_validate,
          envir = environment(builder_session_build)
        )
      }
    },
    add = TRUE
  )

  builder_session_build(
    process,
    plan,
    request,
    coordinator = coordinator,
    auth_material = material
  )

  serialized <- serialize(captured_args, NULL)
  expect_false(builder_auth_raw_contains(serialized, username))
  expect_false(builder_auth_raw_contains(serialized, password))
  expect_false(builder_auth_raw_contains(serialized, passphrase))
  expect_null(captured_args$request$payload$auth_accounts)
  expect_false("auth_accounts" %in% names(captured_args))
  expect_false("accounts" %in% names(captured_args$plan$app_auth))
  expect_identical(
    names(captured_args$auth_material),
    c("source_dir", "credentials", "env_file", "descriptor")
  )
  expect_false("auth_accounts" %in% names(formals(builder_session_build)))
  expect_false("auth_accounts" %in% names(formals(captured_callback)))
})

test_that("the App keeps Build execution private until the workflow reaches it", {
  lines <- builder_app_lines()
  app <- paste(lines, collapse = "\n")
  workbench <- builder_app_block(
    lines,
    "output$workbench <- renderUI({",
    "observeEvent(input$continue_to_review, {"
  )

  expect_match(
    app,
    "build_state <- reactiveVal(builder_build_state())",
    fixed = TRUE
  )
  expect_match(app, "builder_reduce_build", fixed = TRUE)
  expect_false(grepl("observeEvent(input$cancel_build, {", app, fixed = TRUE))
  expect_false(grepl("builder_protocol_cancel", app, fixed = TRUE))
  expect_false(grepl('"cancel_build"', workbench, fixed = TRUE))
  expect_false(grepl('actionButton(\n      "build"', app, fixed = TRUE))
  expect_match(workbench, "build = render_build_workbench()", fixed = TRUE)
})

test_that("session shutdown stops the worker before releasing snapshots", {
  lines <- builder_app_lines()
  shutdown <- builder_app_block(
    lines,
    "session$onSessionEnded(function() {",
    "## -- native file picker and examples"
  )
  stopped <- regexpr("builder_worker_stop", shutdown, fixed = TRUE)[1L]
  confirmed <- regexpr("isTRUE(stopped$stopped)", shutdown, fixed = TRUE)[1L]
  cleanup_safe <- regexpr(
    "isTRUE(stopped$worker$cleanup_safe)",
    shutdown,
    fixed = TRUE
  )[1L]
  released <- regexpr(".builder_snapshot_release", shutdown, fixed = TRUE)[1L]

  expect_gt(stopped, 0L)
  expect_gt(confirmed, stopped)
  expect_gt(cleanup_safe, confirmed)
  expect_gt(released, cleanup_safe)
  expect_match(
    shutdown,
    "unlink(current_worker$snapshot_root, recursive = TRUE, force = TRUE)",
    fixed = TRUE
  )
  expect_false(grepl("process$close()", shutdown, fixed = TRUE))
})

test_that("session build validates frozen snapshots before execution", {
  session <- paste(
    readLines(
      builder_profile_inst_path("builder", "session.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  validate <- regexpr(
    ".builder_session_plan_snapshot_error",
    session,
    fixed = TRUE
  )[1L]
  execute <- regexpr(
    "value <- builder_execute_plan(",
    session,
    fixed = TRUE
  )[1L]

  expect_gt(validate, 0L)
  expect_gt(execute, validate)
  expect_match(session, "snapshot_validator", fixed = TRUE)
})
