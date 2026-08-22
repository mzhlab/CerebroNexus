## Builder server: build.

## -- build ---------------------------------------------------------------
## The whole export runs in the worker: analyses, matrix write, bundle. This
## process only sends a plan and waits for the report, so the page keeps
## answering while a marker-gene run takes its minutes.
auth_accounts_state <- auth_accounts
conflict_nonce_sequence <- 0L
build_output_preflight_process <- reactiveVal(NULL)
build_output_preflight_request <- reactiveVal(NULL)
selected_output_release_state <- reactiveVal(NULL)

builder_selected_output_release_state <- function(path) {
  cached <- isolate(selected_output_release_state())
  normalized <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (
    !is.list(cached) ||
      !builder_has_text(normalized %||% "") ||
      !identical(cached$path, normalized)
  ) {
    return(NULL)
  }
  cached$prior_state %||% NULL
}

observeEvent(
  selected_output(),
  {
    cached <- isolate(selected_output_release_state())
    selected <- selected_output()
    if (
      is.list(cached) &&
        (!builder_has_text(selected %||% "") ||
          !identical(cached$path, selected))
    ) {
      selected_output_release_state(NULL)
    }
  },
  ignoreNULL = FALSE
)

next_conflict_nonce <- function() {
  conflict_nonce_sequence <<- conflict_nonce_sequence + 1L
  paste(session$token, conflict_nonce_sequence, sep = "-")
}

observeEvent(
  input$build_output_mode,
  {
    if (builder_mutations_locked(isolate(build_flow()), isolate(protocol()))) {
      return()
    }
    app_required <- isTRUE(isolate(workflow())$review_plan$make_app)
    requested <- identical(input$build_output_mode, "app")
    enabled <- (requested || app_required) &&
      isTRUE(app_capability$available)
    build_mode(enabled)
    if (!enabled) {
      auth_enabled(FALSE)
      auth_accounts(builder_auth_empty_accounts())
      auth_validation(list(ok = TRUE, error = NULL))
      session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
    }
  },
  ignoreInit = TRUE
)

observeEvent(
  input$build_initial_dataset,
  {
    if (builder_mutations_locked(isolate(build_flow()), isolate(protocol()))) {
      return()
    }
    value <- input$build_initial_dataset
    if (builder_has_text(value %||% "")) {
      build_initial_dataset(value)
    }
  },
  ignoreInit = TRUE
)

output$build_output_options <- renderUI({
  req(identical(workflow()$stage, "build"))
  plan <- workflow()$review_plan
  auth <- auth_capability()
  controls_disabled <- builder_mutations_locked(build_flow(), protocol())
  items <- plan$items %||% list()
  dataset_choices <- stats::setNames(
    vapply(items, `[[`, character(1), "id"),
    vapply(items, `[[`, character(1), "name")
  )
  selected_dataset <- build_initial_dataset()
  if (is.null(selected_dataset) && length(dataset_choices)) {
    selected_dataset <- unname(dataset_choices[[1L]])
  }
  selected_index <- match(selected_dataset, unname(dataset_choices))
  page_expectations <- if (
    length(selected_index) == 1L && !is.na(selected_index)
  ) {
    items[[selected_index]]$viewer_page_expectations %||% list()
  } else {
    list()
  }
  builder_build_options_ui(
    builder_build_options(
      make_app = isTRUE(build_mode()),
      welcome_message = isolate(review_options()$welcome_message),
      initial_page = isolate(review_options()$initial_page),
      point_size = isolate(review_options()$point_size),
      variable_to_compare = isolate(review_options()$variable_to_compare),
      host = isolate(review_options()$host),
      port = isolate(review_options()$port),
      max_request_size = isolate(review_options()$max_request_size),
      display_mode = isolate(review_options()$display_mode),
      launch_browser = FALSE,
      show_upload_ui = isolate(review_options()$show_upload_ui),
      initial_dataset = selected_dataset
    ),
    app_available = isTRUE(app_capability$available),
    app_reason = app_capability$reason,
    app_required = isTRUE(plan$make_app),
    initial_page_choices = builder_review_initial_page_choices(
      page_expectations
    ),
    dataset_choices = dataset_choices,
    auth = list(
      enabled = isTRUE(auth_enabled()),
      account_count = as.integer(length(auth_accounts())),
      error = auth_validation()$error %||% NULL,
      available = isTRUE(auth$available)
    ),
    controls_disabled = controls_disabled
  )
})

current_build_options <- function() {
  options <- isolate(review_options())
  builder_build_options(
    make_app = isTRUE(isolate(build_mode())),
    welcome_message = options$welcome_message,
    initial_page = options$initial_page,
    point_size = options$point_size,
    variable_to_compare = options$variable_to_compare,
    host = options$host,
    port = options$port,
    max_request_size = options$max_request_size,
    display_mode = options$display_mode,
    launch_browser = FALSE,
    show_upload_ui = options$show_upload_ui,
    initial_dataset = isolate(build_initial_dataset())
  )
}

build_stage_status_projection <- reactive({
  req(identical(workflow()$stage, "build"))
  builder_build_stage_status_model(
    flow = build_flow(),
    protocol = protocol(),
    note = busy_note(),
    result = result(),
    output_selected = builder_stage_has_text(selected_output() %||% "")
  )
})

output$build_stage_status_content <- renderUI({
  body <- builder_build_stage_status_body_ui(build_stage_status_projection())
  if (is.null(body)) {
    return(NULL)
  }
  tags$section(
    class = "builder-stage-section builder-build-status-section",
    tags$h3("Build status"),
    body
  )
})

output$build_stage_footer <- renderUI({
  builder_build_stage_footer_ui(
    build_stage_status_projection(),
    controls_disabled = builder_build_controls_locked(build_flow())
  )
})

observe({
  session$sendCustomMessage(
    "builder_dataset_mutation_lock",
    list(locked = builder_mutations_locked(build_flow(), protocol()))
  )
})

builder_build_confirmation_status <- function(state, plan) {
  if (!identical(state$stage, "build")) {
    return(list(ok = FALSE, reason = "stage_mismatch"))
  }
  stored <- state$review_plan
  if (!builder_review_can_build(stored)) {
    return(list(ok = FALSE, reason = "stored_plan_unavailable"))
  }
  if (
    !inherits(plan, "builder_build_plan") ||
      !is.list(plan) ||
      !identical(plan$readiness, "ready") ||
      !is.null(plan$error)
  ) {
    return(list(ok = FALSE, reason = "candidate_plan_unavailable"))
  }
  if (!builder_workflow_confirmation_matches(state, stored)) {
    return(list(ok = FALSE, reason = "confirmation_mismatch"))
  }
  if (
    !identical(
      builder_review_plan_identity(stored),
      builder_review_plan_identity(plan)
    )
  ) {
    return(list(ok = FALSE, reason = "identity_mismatch"))
  }
  list(ok = TRUE, reason = NULL)
}

builder_build_recovery_needs_fresh_review <- function(message) {
  state <- isolate(workflow())
  if (state$stage %in% c("review", "build")) {
    workflow(builder_reduce_workflow(state, list(type = "invalidate")))
  }
  selected_output(NULL)
  build_flow(list(stage = "idle", plan = NULL))
  result(NULL)
  session$sendCustomMessage(
    "builder_build_dialog",
    list(action = "close")
  )
  showNotification(
    message,
    type = "warning",
    duration = 6
  )
  invisible(FALSE)
}

builder_build_recovery_ready <- function() {
  state <- isolate(workflow())
  stored <- state$review_plan
  auth_required <- (isTRUE(isolate(build_mode())) &&
    isTRUE(isolate(auth_enabled()))) ||
    (is.list(stored) && isTRUE(stored$app_auth$enabled))
  if (auth_required) {
    parsed <- builder_auth_validate_payload(TRUE, isolate(auth_accounts()))
    expected <- if (is.list(stored) && isTRUE(stored$app_auth$enabled)) {
      stored$app_auth$account_count %||% 1L
    } else {
      length(isolate(auth_accounts()))
    }
    if (
      !isTRUE(parsed$ok) ||
        !identical(as.integer(length(parsed$accounts)), as.integer(expected))
    ) {
      return(builder_build_recovery_needs_fresh_review(
        "Re-enter login accounts and review the plan before retrying."
      ))
    }
  }
  live <- freeze_materialized_plan_for_output(
    file.path(tempdir(), "cerebro-builder-output-preview"),
    overwrite = FALSE
  )
  if (!isTRUE(builder_build_confirmation_status(state, live)$ok)) {
    return(builder_build_recovery_needs_fresh_review(
      "Settings changed. Review the updated plan before retrying."
    ))
  }
  invisible(TRUE)
}

builder_require_confirmed_build_plan <- function(plan, output_path = NULL) {
  state <- isolate(workflow())
  status <- builder_build_confirmation_status(state, plan)
  if (
    isTRUE(status$ok) &&
      !is.null(output_path) &&
      (!builder_has_text(output_path) ||
        !identical(isolate(selected_output()), output_path))
  ) {
    status <- list(ok = FALSE, reason = "output_mismatch")
  }
  if (isTRUE(status$ok)) {
    return(invisible(TRUE))
  }
  builder_build_recovery_needs_fresh_review(
    "Settings changed. Review the updated plan before building."
  )
}

builder_build_attempt_failed <- function(message) {
  build_flow(list(stage = "idle", plan = NULL))
  session$sendCustomMessage(
    "builder_build_dialog",
    list(action = "close")
  )
  showNotification(
    message,
    type = "warning",
    duration = 6
  )
  invisible(FALSE)
}

enqueue_build_plan <- function(
  plan,
  auth_accounts,
  expected_identity = builder_final_build_identity(plan)
) {
  if (!isTRUE(builder_require_confirmed_build_plan(plan, plan$out_dir))) {
    return(invisible(FALSE))
  }
  rs <- isolate(worker())
  if (is.null(rs)) {
    return(builder_build_attempt_failed(
      "The background worker is not ready. Try Build again in a moment."
    ))
  }
  current_protocol <- isolate(protocol())
  protocol_ready <- !is.null(current_protocol) &&
    isTRUE(tryCatch(
      builder_protocol_is_quiescent(current_protocol),
      error = function(error) FALSE
    ))
  if (!protocol_ready) {
    return(builder_build_attempt_failed(
      "The background worker is busy. Try Build again when it is ready."
    ))
  }
  if (!identical(builder_final_build_identity(plan), expected_identity)) {
    return(builder_build_attempt_failed(
      "Output settings changed before the build was queued. Try Build again."
    ))
  }
  parsed_auth <- builder_auth_validate_payload(
    isTRUE(plan$app_auth$enabled),
    auth_accounts
  )
  if (
    !isTRUE(parsed_auth$ok) ||
      length(parsed_auth$accounts) != plan$app_auth$account_count
  ) {
    return(invisible(FALSE))
  }
  result(NULL)
  queued <- enqueue(list(
    kind = "build",
    plan = plan,
    auth_accounts = parsed_auth$accounts,
    note = paste0(
      "Building ",
      length(plan$items),
      " dataset",
      if (length(plan$items) == 1L) "" else "s",
      "…"
    )
  ))
  if (!isTRUE(queued)) {
    return(builder_build_attempt_failed(
      "The build could not be queued. Try Build again."
    ))
  }
  if (exists("builder_project_capture_build_plan", mode = "function")) {
    builder_project_capture_build_plan(plan)
  }
  build_flow(list(stage = "building", plan = NULL))
  auth_accounts_state(builder_auth_empty_accounts())
  auth_validation(list(
    ok = FALSE,
    error = "Set up login accounts again before the next build."
  ))
  session$sendCustomMessage("builder_auth_reset", list(reset = TRUE))
  invisible(TRUE)
}

builder_build_conflict_files <- function(plan, prior_state = NULL) {
  conflicts <- basename(plan$existing_targets %||% character())
  if (is.null(prior_state)) {
    prior_state <- builder_release_state(
      plan$out_dir,
      exact_record = FALSE,
      allow_abandoned = TRUE
    )
  }
  record <- prior_state$record
  if (!is.null(record) && !isTRUE(record$abandoned)) {
    owned_paths <- vapply(
      record$members,
      `[[`,
      character(1),
      "path"
    )
    conflicts <- c(conflicts, sub("/.*$", "", owned_paths))
  }
  sort(unique(conflicts[nzchar(conflicts)]), method = "radix")
}

prepare_selected_output <- function(path, overwrite = FALSE) {
  isolate({
    plan <- freeze_materialized_plan_for_output(
      path,
      overwrite = overwrite,
      output_options = current_build_options()
    )
    if (
      !inherits(plan, "builder_build_plan") ||
        !identical(plan$readiness, "ready")
    ) {
      build_flow(list(stage = "idle", plan = NULL))
      showNotification(
        plan$error %||% "The selected folder cannot be used.",
        type = "error",
        duration = 6
      )
      return(invisible(FALSE))
    }
    if (!isTRUE(builder_require_confirmed_build_plan(plan, path))) {
      return(invisible(FALSE))
    }
    conflict_files <- try(
      builder_build_conflict_files(
        plan,
        builder_selected_output_release_state(plan$out_dir)
      ),
      silent = TRUE
    )
    if (inherits(conflict_files, "try-error")) {
      build_flow(list(stage = "idle", plan = NULL))
      showNotification(
        conditionMessage(attr(conflict_files, "condition")),
        type = "error",
        duration = 6
      )
      return(invisible(FALSE))
    }
    if (length(conflict_files) && !isTRUE(overwrite)) {
      nonce <- next_conflict_nonce()
      build_flow(list(stage = "conflict", plan = plan, nonce = nonce))
      session$sendCustomMessage(
        "builder_build_dialog",
        list(
          type = "conflict",
          title = "Files already exist",
          files = conflict_files,
          nonce = nonce
        )
      )
      return(invisible(FALSE))
    }
    request_identity <- builder_final_build_identity(plan)
    enqueue_build_plan(
      plan,
      auth_accounts = isolate(auth_accounts()),
      expected_identity = request_identity
    )
  })
}

builder_build_foreign_output_error <- function(foreign) {
  foreign <- sort(unique(sub("/.*$", "", foreign)), method = "radix")
  if (!length(foreign)) {
    return(NULL)
  }
  shown <- head(foreign, 8L)
  remainder <- length(foreign) - length(shown)
  paste0(
    "The selected folder contains files that do not belong to this release: ",
    paste(shown, collapse = ", "),
    if (remainder > 0L) paste0(" and ", remainder, " more") else "",
    ". Choose an empty folder or an existing Builder output folder."
  )
}

builder_build_output_preflight <- function(path) {
  state <- isolate(workflow())
  plan <- state$review_plan
  if (!builder_workflow_confirmation_matches(state, plan)) {
    return(list(
      ok = FALSE,
      error = "Settings changed. Review the updated plan before building."
    ))
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  options <- current_build_options()
  expected <- if (isTRUE(options$make_app)) {
    c(
      "cerebro_app",
      if (isTRUE(isolate(auth_enabled()))) "viewer-auth.env" else character()
    )
  } else {
    unlist(
      lapply(plan$items, function(item) {
        c(item$filename, item$sidecars %||% character())
      }),
      use.names = FALSE
    )
  }
  expected_roots <- unique(sub("/.*$", "", expected))
  list(
    ok = TRUE,
    error = NULL,
    path = path,
    expected_roots = expected_roots
  )
}

builder_start_build_output_preflight_process <- function(path, expected_roots) {
  runtime_files <- builder_release_runtime_files()
  callr::r_bg(
    function(path, expected_roots, contract_file, publish_file) {
      runtime <- new.env(parent = globalenv())
      runtime$`%||%` <- function(left, right) {
        if (is.null(left)) right else left
      }
      sys.source(contract_file, envir = runtime)
      sys.source(publish_file, envir = runtime)
      runtime$builder_release_output_preflight(path, expected_roots)
    },
    args = list(
      path = path,
      expected_roots = expected_roots,
      contract_file = runtime_files$contract,
      publish_file = runtime_files$publish
    ),
    supervise = TRUE,
    stdout = "|",
    stderr = "|"
  )
}

builder_finish_build_output_preflight <- function(checked) {
  request <- isolate(build_output_preflight_request())
  build_output_preflight_process(NULL)
  build_output_preflight_request(NULL)
  build_flow(list(stage = "idle", plan = NULL))
  if (inherits(checked, "condition")) {
    showNotification(
      paste0(
        "The selected folder could not be checked: ",
        conditionMessage(checked)
      ),
      type = "error",
      duration = 10
    )
    return(invisible(FALSE))
  }
  if (
    !is.list(checked) || !is.logical(checked$ok) || length(checked$ok) != 1L
  ) {
    showNotification(
      "The selected folder check returned an invalid result.",
      type = "error",
      duration = 10
    )
    return(invisible(FALSE))
  }
  if (!isTRUE(checked$ok)) {
    message <- checked$error %||%
      builder_build_foreign_output_error(checked$foreign %||% character()) %||%
      "The selected folder cannot be used."
    showNotification(message, type = "warning", duration = 10)
    return(invisible(FALSE))
  }
  valid_prior_state <- is.list(checked$prior_state) &&
    identical(checked$prior_state$schema_version, 1L) &&
    .builder_release_identity_valid(checked$prior_state$identity)
  if (!valid_prior_state) {
    showNotification(
      "The selected folder check returned an invalid release state.",
      type = "error",
      duration = 10
    )
    return(invisible(FALSE))
  }
  state <- isolate(workflow())
  if (
    !is.list(request) ||
      !builder_workflow_confirmation_matches(state, state$review_plan)
  ) {
    showNotification(
      "Settings changed. Review the updated plan before building.",
      type = "warning",
      duration = 8
    )
    return(invisible(FALSE))
  }
  selected_output_release_state(list(
    path = request$path,
    prior_state = checked$prior_state
  ))
  selected_output(request$path)
  result(NULL)
  invisible(TRUE)
}

builder_poll_build_output_preflight <- function() {
  process <- isolate(build_output_preflight_process())
  if (is.null(process)) {
    return(invisible(FALSE))
  }
  if (builder_session_closed()) {
    try(process$kill(), silent = TRUE)
    build_output_preflight_process(NULL)
    build_output_preflight_request(NULL)
    return(invisible(FALSE))
  }
  alive <- tryCatch(process$is_alive(), error = identity)
  if (inherits(alive, "condition")) {
    return(builder_finish_build_output_preflight(alive))
  }
  if (isTRUE(alive)) {
    later::later(
      function() {
        shiny::withReactiveDomain(
          builder_lifecycle_session,
          builder_poll_build_output_preflight()
        )
      },
      delay = 0.1
    )
    return(invisible(TRUE))
  }
  checked <- tryCatch(process$get_result(), error = identity)
  builder_finish_build_output_preflight(checked)
}

start_builder_build_output_preflight <- function(path) {
  if (!is.null(isolate(build_output_preflight_process()))) {
    return(invisible(FALSE))
  }
  build_flow(list(stage = "checking_folder", plan = NULL))
  request <- tryCatch(
    shiny::isolate(builder_build_output_preflight(path)),
    error = identity
  )
  if (inherits(request, "condition") || !isTRUE(request$ok)) {
    build_flow(list(stage = "idle", plan = NULL))
    showNotification(
      if (inherits(request, "condition")) {
        paste0(
          "The selected folder could not be checked: ",
          conditionMessage(request)
        )
      } else {
        request$error %||% "The selected folder cannot be used."
      },
      type = "warning",
      duration = 10
    )
    return(invisible(FALSE))
  }
  entries <- tryCatch(
    list.files(request$path, all.files = TRUE, no.. = TRUE),
    error = function(error) NULL
  )
  if (!is.null(entries) && !length(entries)) {
    build_output_preflight_request(request)
    return(builder_finish_build_output_preflight(list(
      ok = TRUE,
      error = NULL,
      foreign = character(),
      prior_state = list(
        schema_version = 1L,
        identity = list(schema_version = 1L, exists = TRUE, entries = list()),
        record = NULL
      )
    )))
  }
  if (!requireNamespace("callr", quietly = TRUE)) {
    build_flow(list(stage = "idle", plan = NULL))
    showNotification(
      "The selected folder cannot be checked because callr is unavailable.",
      type = "error",
      duration = 8
    )
    return(invisible(FALSE))
  }
  process <- tryCatch(
    builder_start_build_output_preflight_process(
      request$path,
      request$expected_roots
    ),
    error = identity
  )
  if (inherits(process, "condition")) {
    build_flow(list(stage = "idle", plan = NULL))
    showNotification(
      paste0(
        "The selected folder could not be checked: ",
        conditionMessage(process)
      ),
      type = "error",
      duration = 10
    )
    return(invisible(FALSE))
  }
  build_output_preflight_request(request)
  build_output_preflight_process(process)
  if (!isTRUE(tryCatch(process$is_alive(), error = function(error) TRUE))) {
    return(builder_poll_build_output_preflight())
  }
  later::later(
    function() {
      shiny::withReactiveDomain(
        builder_lifecycle_session,
        builder_poll_build_output_preflight()
      )
    },
    delay = 0
  )
  invisible(TRUE)
}

session$onSessionEnded(function() {
  process <- isolate(build_output_preflight_process())
  if (!is.null(process)) {
    try(process$kill(), silent = TRUE)
  }
})

show_builder_build_server_path <- function() {
  shiny::showModal(builder_server_path_dialog(
    title = "Choose build destination from server path",
    input_id = "builder_build_server_folder",
    action_id = "use_builder_build_server_folder",
    label = "Output folder",
    action_label = "Use folder"
  ))
  invisible(TRUE)
}

choose_build_folder <- function() {
  build_flow(list(stage = "choosing_folder", plan = NULL))
  builder_schedule_native_picker(
    "output_directory",
    key = "build-output",
    on_result = function(choice) {
      tryCatch(
        {
          if (identical(choice$status, "cancelled")) {
            build_flow(list(stage = "idle", plan = NULL))
            return()
          }
          if (!identical(choice$status, "selected")) {
            build_flow(list(stage = "idle", plan = NULL))
            showNotification(
              choice$error %||% "The folder picker could not be opened.",
              type = "error",
              duration = 6
            )
            show_builder_build_server_path()
            return()
          }
          start_builder_build_output_preflight(choice$path)
        },
        error = function(error) {
          build_flow(list(stage = "idle", plan = NULL))
          showNotification(
            paste0(
              "The selected folder could not be checked: ",
              conditionMessage(error)
            ),
            type = "error",
            duration = 10
          )
        }
      )
    },
    on_error = function(error) {
      build_flow(list(stage = "idle", plan = NULL))
      showNotification(conditionMessage(error), type = "error", duration = 6)
      show_builder_build_server_path()
    }
  )
}

observeEvent(input$choose_output_folder, {
  if (builder_build_controls_locked(isolate(build_flow()))) {
    return()
  }
  choose_build_folder()
})

observeEvent(input$use_builder_build_server_folder, {
  if (builder_build_controls_locked(isolate(build_flow()))) {
    return()
  }
  path <- tryCatch(
    builder_server_path_resolve(
      input$builder_build_server_folder %||% "",
      "directory"
    ),
    error = identity
  )
  if (inherits(path, "condition")) {
    showNotification(conditionMessage(path), type = "error", duration = 7)
    return()
  }
  accepted <- tryCatch(
    start_builder_build_output_preflight(path),
    error = function(error) {
      build_flow(list(stage = "idle", plan = NULL))
      showNotification(
        paste0(
          "The selected folder could not be checked: ",
          conditionMessage(error)
        ),
        type = "error",
        duration = 10
      )
      FALSE
    }
  )
  if (isTRUE(accepted)) {
    shiny::removeModal()
  }
})

start_confirmed_build <- function() {
  if (
    exists("builder_operation_allowed", mode = "function", inherits = TRUE) &&
      !isTRUE(builder_operation_allowed("build"))
  ) {
    return(invisible(FALSE))
  }
  if (builder_build_controls_locked(isolate(build_flow()))) {
    return(invisible(FALSE))
  }
  output_path <- isolate(selected_output())
  if (!builder_has_text(output_path)) {
    return(invisible(FALSE))
  }
  result(NULL)
  build_flow(list(stage = "preparing", plan = NULL))
  session$sendCustomMessage("builder_focus_build_status", list())
  later::later(
    function() {
      shiny::withReactiveDomain(builder_lifecycle_session, {
        prepare_selected_output(output_path)
      })
    },
    delay = 0
  )
  invisible(TRUE)
}

retry_confirmed_build <- function() {
  if (!isTRUE(builder_build_recovery_ready())) {
    return(invisible(FALSE))
  }
  start_confirmed_build()
}

observeEvent(input$build, {
  start_confirmed_build()
})

observeEvent(input$builder_build_dialog, {
  event <- input$builder_build_dialog
  flow <- isolate(build_flow())
  if (!identical(flow$stage, "conflict")) {
    return()
  }
  if (
    !is.list(event) ||
      is.object(event) ||
      !identical(sort(names(event)), c("action", "nonce")) ||
      !is.character(event$action) ||
      length(event$action) != 1L ||
      is.na(event$action) ||
      !is.character(event$nonce) ||
      length(event$nonce) != 1L ||
      is.na(event$nonce) ||
      !nzchar(event$nonce) ||
      !identical(event$nonce, flow$nonce)
  ) {
    return()
  }
  action <- event$action
  if (
    identical(action, "replace") &&
      inherits(flow$plan, "builder_build_plan")
  ) {
    if (
      !isTRUE(builder_require_confirmed_build_plan(
        flow$plan,
        flow$plan$out_dir
      ))
    ) {
      return()
    }
    prepare_selected_output(flow$plan$out_dir, overwrite = TRUE)
  } else if (identical(action, "choose_another")) {
    if (
      !isTRUE(builder_require_confirmed_build_plan(
        flow$plan,
        flow$plan$out_dir
      ))
    ) {
      return()
    }
    build_flow(list(stage = "idle", plan = NULL))
    choose_build_folder()
  } else if (identical(action, "cancel")) {
    build_flow(list(stage = "idle", plan = NULL))
  }
})

validate_rail_removal <- function(next_state, id) {
  builder_validate_next_plan(
    next_state,
    out_dir = file.path(tempdir(), "cerebro-builder-output-preview"),
    make_app = FALSE,
    overwrite = FALSE
  )
}

remove_dataset <- function(
  previous_state,
  updated,
  id,
  validation
) {
  ids <- vapply(previous_state$datasets, `[[`, character(1), "id")
  entry <- previous_state$datasets[[match(id, ids)]]
  if (!is.null(isolate(import_of(id)))) {
    remove_pending_import(id)
  }
  previous_removed <- previous_state$last_removed
  if (is.list(previous_removed)) {
    identity <- .builder_worker_identity(previous_removed$entry$snapshot)
    pending_drops <- isolate(pending_snapshot_drops())
    pending_drops[[previous_removed$id]] <- identity
    pending_snapshot_drops(pending_drops)
    queued <- enqueue(list(
      kind = "drop",
      id = previous_removed$id,
      dataset_revision = previous_removed$entry$revision %||% 0L,
      snapshot_identity = identity,
      note = "Releasing memory…"
    ))
    if (!isTRUE(queued)) {
      pending_drops[[previous_removed$id]] <- NULL
      pending_snapshot_drops(pending_drops)
    }
  }
  result(NULL)
  showNotification(
    tagList(
      paste0("Removed ", entry$settings$name, ". "),
      actionLink("undo_remove", "Undo")
    ),
    type = "message",
    duration = 10
  )
}

# observeEvent(input$drop_ds, ...) is owned by builder_dataset_rail_server().
builder_dataset_rail_server(
  input = input,
  session = session,
  store = store,
  validate_remove = validate_rail_removal,
  select_dataset = function(id, commit, switch_token = NULL) {
    if (
      exists("builder_operation_allowed", mode = "function", inherits = TRUE) &&
        !isTRUE(builder_operation_allowed("select_dataset"))
    ) {
      if (!is.null(switch_token)) {
        session$sendCustomMessage(
          "builder_dataset_switch_state",
          list(dataset = id, state = "error", switch_token = switch_token)
        )
      }
      return(invisible(FALSE))
    }
    alignment_server$request_dataset_switch(id, commit, switch_token)
  },
  on_select = function(id) {
    active_import_id(NULL)
    result(NULL)
  },
  on_reorder = function(previous, updated) {
    if (identical(previous$datasets, updated$datasets)) {
      return(invisible(FALSE))
    }
    state <- isolate(workflow())
    if (!is.null(state$review_plan) || !is.null(state$confirmation)) {
      workflow(builder_reduce_workflow(state, list(type = "invalidate")))
      build_flow(list(stage = "idle", plan = NULL))
      result(NULL)
      session$sendCustomMessage(
        "builder_build_dialog",
        list(action = "close")
      )
    }
    invisible(TRUE)
  },
  on_remove = remove_dataset,
  on_undo = function() result(NULL),
  on_validation = function(validation) {
    if (isTRUE(validation$ok)) {
      add_error(NULL)
    } else if (!identical(validation$code, "confirmation_required")) {
      add_error(validation$message)
    }
  },
  mutations_locked = dataset_mutations_locked,
  on_locked = function() dataset_mutations_locked(notify = TRUE)
)

output$busy <- renderUI({
  note <- busy_note()
  if (is.null(note) || identical(workflow()$stage, "build")) {
    return(NULL)
  }
  div(
    class = "busy",
    span(class = "spinner"),
    span(note)
  )
})
