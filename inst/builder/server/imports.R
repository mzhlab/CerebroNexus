## Builder server: imports.

dataset_mutations_locked <- function(notify = TRUE) {
  if (exists("builder_operation_allowed", mode = "function", inherits = TRUE)) {
    return(
      !isTRUE(builder_operation_allowed(
        "mutate_datasets",
        notify = notify
      ))
    )
  }
  locked <- builder_mutations_locked(
    isolate(build_flow()),
    isolate(protocol())
  )
  if (isTRUE(locked) && isTRUE(notify)) {
    showNotification(
      "Wait for the active build to finish before changing datasets.",
      type = "warning",
      duration = 6
    )
  }
  isTRUE(locked)
}

start_load <- function(
  kind,
  arg,
  label,
  file_meta = NULL,
  client_id = NULL,
  dataset_id = NULL,
  source_origin = NULL,
  example_id = NULL,
  retained_path = NULL
) {
  if (isTRUE(replay_existing_client_import(client_id))) {
    return(invisible(FALSE))
  }
  restoring_source <- !is.null(dataset_id) &&
    exists("builder_project_pending_entries", inherits = TRUE) &&
    dataset_id %in% names(isolate(builder_project_pending_entries()))
  if (!isTRUE(restoring_source) && dataset_mutations_locked()) {
    release_client_import(
      client_id,
      outcome = "rejected",
      message = "Dataset changes are locked while the active build finishes."
    )
    return(invisible(FALSE))
  }
  rs <- worker()
  if (is.null(rs)) {
    add_error("The background worker is not ready yet.")
    release_client_import(
      client_id,
      outcome = "rejected",
      message = "The background worker is not ready yet."
    )
    return(invisible(FALSE))
  }
  reservation_entries <- sets()
  if (isTRUE(restoring_source)) {
    reservation_entries <- Filter(
      function(entry) !identical(entry$id, dataset_id),
      reservation_entries
    )
  }
  reservation <- builder_source_reserve(
    reservation_entries,
    pending_sources(),
    kind,
    arg
  )
  if (!isTRUE(reservation$ok)) {
    release_client_import(
      client_id,
      outcome = "rejected",
      message = "This dataset has already been added or is already waiting."
    )
    return(invisible(FALSE))
  }
  pending_sources(reservation$pending)
  add_error(NULL)
  existing_ids <- builder_project_allocation_ids(
    entries = sets(),
    import_entries = imports()$entries %||% list(),
    replacing_id = if (isTRUE(restoring_source)) dataset_id else NULL
  )
  allocation <- builder_project_allocate_dataset_id(
    seq_id(),
    existing_ids,
    restored_id = dataset_id
  )
  seq_id(allocation$sequence)
  id <- allocation$id
  filename <- NULL
  file_type <- NULL
  file_size <- NA_real_
  retained_source <- if (
    builder_has_text(retained_path) && file.exists(retained_path)
  ) {
    normalizePath(retained_path, winslash = "/", mustWork = TRUE)
  } else {
    NULL
  }
  if (identical(kind, "file") && is.list(file_meta)) {
    uploads <- pending_uploads()
    filename <- builder_safe_file_name(file_meta$name, paste0(label, ".rds"))
    file_type <- builder_file_type_label(filename, file_meta$type)
    file_size <- suppressWarnings(as.numeric(file_meta$size %||% NA_real_))
    uploads[[id]] <- list(
      id = id,
      filename = filename,
      type = file_type,
      size = file_size,
      visible = TRUE
    )
    pending_uploads(uploads)
  } else if (identical(kind, "example")) {
    example_source <- try(
      builder_project_example_source(arg, builder_example_catalog()),
      silent = TRUE
    )
    if (!inherits(example_source, "try-error")) {
      filename <- example_source$filename
      file_size <- suppressWarnings(as.numeric(
        file.info(example_source$path)$size[[1L]]
      ))
      retained_source <- example_source$path
    }
  }
  generation <- 1L
  progress_path <- NULL
  snapshot_root <- rs$snapshot_root %||% NULL
  if (
    is.character(snapshot_root) &&
      length(snapshot_root) == 1L &&
      !is.na(snapshot_root) &&
      dir.exists(snapshot_root)
  ) {
    candidate <- try(
      builder_import_progress_path(snapshot_root, id, generation),
      silent = TRUE
    )
    if (!inherits(candidate, "try-error")) {
      progress_path <- candidate
    }
  }
  retain_example <- identical(kind, "example") &&
    builder_has_text(retained_source) &&
    builder_has_text(snapshot_root) &&
    dir.exists(snapshot_root)
  retain_upload <- identical(kind, "file") &&
    !identical(source_origin, "local") &&
    !builder_has_text(retained_source)
  retain_in_worker <- FALSE
  if (retain_upload) {
    retained <- try(
      builder_project_session_source_path(
        arg,
        filename,
        snapshot_root,
        id
      ),
      silent = TRUE
    )
    retain_in_worker <- !inherits(retained, "try-error")
  } else if (retain_example) {
    retained <- try(
      builder_project_retain_session_source(
        retained_source,
        filename,
        snapshot_root,
        id
      ),
      silent = TRUE
    )
  } else {
    retained <- retained_source
  }
  if (retain_upload || retain_example) {
    if (inherits(retained, "try-error")) {
      pending_sources(builder_source_release(
        pending_sources(),
        reservation$key
      ))
      uploads <- pending_uploads()
      uploads[[id]] <- NULL
      pending_uploads(uploads)
      release_client_import(
        client_id,
        outcome = "error",
        message = "The uploaded source could not be retained for this session."
      )
      add_error("The uploaded source could not be retained for this session.")
      return(invisible(FALSE))
    }
    retained_source <- retained
  }
  source_descriptor <- list(
    kind = kind,
    origin = source_origin %||%
      if (identical(kind, "example")) {
        "example"
      } else {
        "upload"
      },
    staged_path = retained_source %||%
      if (identical(kind, "file")) arg else NULL,
    transport_path = if (isTRUE(retain_in_worker)) arg else NULL,
    example = example_id %||% if (identical(kind, "example")) arg else NULL,
    reservation_key = reservation$key,
    fingerprint = if (identical(kind, "example")) {
      paste0("example:", arg, ":builder-profile-v1")
    } else {
      info <- suppressWarnings(file.info(arg))
      paste(
        suppressWarnings(as.numeric(info$size[[1L]] %||% file_size)),
        suppressWarnings(as.numeric(info$mtime[[1L]] %||% NA_real_)),
        sep = ":"
      )
    }
  )
  pending_entry <- builder_import_entry(
    id = id,
    label = label,
    source = source_descriptor,
    filename = filename,
    file_type = file_type,
    size = file_size,
    generation = generation
  )
  imports(builder_import_add(isolate(imports()), pending_entry))
  next_focus <- builder_import_auto_focus(
    isolate(current()),
    isolate(active_import_id()),
    id
  )
  if (!is.null(next_focus)) {
    active_import_id(next_focus)
  }
  if (builder_has_text(client_id)) {
    bind_client_import(client_id, id, filename %||% label, kind)
  }
  queued <- enqueue(list(
    kind = "load",
    source = kind,
    id = id,
    path = if (identical(kind, "file")) arg else NA_character_,
    retained_path = retained_source,
    retain_source = retain_in_worker,
    example = example_id %||% if (identical(kind, "example")) arg else NULL,
    source_origin = source_descriptor$origin,
    label = label,
    filename = filename,
    reservation_key = reservation$key,
    import_generation = generation,
    progress_path = progress_path,
    note = paste0("Loading ", label, "…")
  ))
  if (!isTRUE(queued)) {
    pending_sources(builder_source_release(
      pending_sources(),
      reservation$key
    ))
    uploads <- pending_uploads()
    uploads[[id]] <- NULL
    pending_uploads(uploads)
    forget_import(id)
    builder_import_progress_remove(progress_path %||% "")
    release_client_import(
      client_id,
      server_id = NULL,
      outcome = "error",
      message = "The background worker is not ready yet."
    )
  }
  invisible(isTRUE(queued))
}

process_client_import_upload <- function(uploads, dispatch) {
  pending_client_id <- if (is.list(dispatch)) dispatch$client_id else NULL
  if (
    !is.data.frame(uploads) ||
      nrow(uploads) != 1L ||
      !all(c("name", "datapath") %in% names(uploads))
  ) {
    release_client_import(
      pending_client_id,
      outcome = "rejected",
      message = "Expected exactly one uploaded file."
    )
    return()
  }
  path <- as.character(uploads$datapath[[1L]])
  label <- as.character(uploads$name[[1L]])
  size <- if ("size" %in% names(uploads)) {
    suppressWarnings(as.numeric(uploads$size[[1L]]))
  } else {
    NA_real_
  }
  type <- if ("type" %in% names(uploads)) {
    as.character(uploads$type[[1L]])
  } else {
    ""
  }
  metadata_matches <- is.list(dispatch) &&
    builder_has_text(pending_client_id) &&
    identical(dispatch$name, label) &&
    (is.na(dispatch$size) ||
      is.na(size) ||
      identical(as.numeric(dispatch$size), as.numeric(size)))
  if (
    !metadata_matches || !builder_has_text(path) || !builder_has_text(label)
  ) {
    release_client_import(
      pending_client_id,
      outcome = "rejected",
      message = "The uploaded file did not match its dispatch metadata."
    )
    return()
  }
  supported_extensions <- unique(tolower(unlist(lapply(
    builder_formats,
    `[[`,
    "extensions"
  ))))
  if (!tolower(tools::file_ext(label)) %in% supported_extensions) {
    release_client_import(
      pending_client_id,
      outcome = "rejected",
      message = "This file format is not supported."
    )
    return()
  }
  start_load(
    "file",
    path,
    tools::file_path_sans_ext(basename(label)),
    file_meta = list(name = label, type = type, size = size),
    client_id = pending_client_id
  )
  invisible(TRUE)
}

consume_client_import_upload <- function() {
  dispatch <- isolate(pending_client_import_dispatch())
  upload <- isolate(pending_client_upload())
  if (is.null(dispatch) || is.null(upload)) {
    return(invisible(FALSE))
  }
  pending_client_import_dispatch(NULL)
  pending_client_upload(NULL)
  process_client_import_upload(upload$files, dispatch)
}

expire_pending_client_upload <- function(token) {
  if (builder_session_closed()) {
    return(invisible(FALSE))
  }
  upload <- isolate(pending_client_upload())
  dispatch <- isolate(pending_client_import_dispatch())
  if (
    is.null(upload) ||
      !identical(upload$token, token) ||
      !is.null(dispatch)
  ) {
    return(invisible(FALSE))
  }
  pending_client_upload(NULL)
  invisible(TRUE)
}

observeEvent(input$builder_client_import_dispatch, {
  event <- input$builder_client_import_dispatch
  if (!is.list(event) || is.object(event)) {
    pending_client_import_dispatch(NULL)
    return()
  }
  pending_client_import_dispatch(list(
    client_id = event$client_id %||% NULL,
    name = event$name %||% NULL,
    size = suppressWarnings(as.numeric(event$size %||% NA_real_)[1L]),
    token = event$nonce %||% NULL
  ))
  consumed <- consume_client_import_upload()
  if (!isTRUE(consumed)) {
    current <- isolate(pending_client_import_dispatch())
    if (is.list(current) && identical(current$token, event$nonce %||% NULL)) {
      session$sendCustomMessage(
        "builder_client_import_dispatch_ready",
        list(client_id = current$client_id)
      )
    }
  }
  token <- event$nonce %||% NULL
  later::later(
    function() {
      if (builder_session_closed()) {
        return(invisible(FALSE))
      }
      dispatch <- isolate(pending_client_import_dispatch())
      if (is.null(dispatch) || !identical(dispatch$token, token)) {
        return()
      }
      pending_client_import_dispatch(NULL)
      release_client_import(
        dispatch$client_id,
        outcome = "rejected",
        message = "The file upload was not received in time."
      )
    },
    delay = 30
  )
})

observeEvent(input$dataset_files, {
  token <- isolate(pending_client_upload_sequence()) + 1L
  pending_client_upload_sequence(token)
  pending_client_upload(list(
    files = input$dataset_files,
    received = Sys.time(),
    token = token
  ))
  consume_client_import_upload()
  later::later(
    function() expire_pending_client_upload(token),
    delay = 30
  )
})

observeEvent(input$choose_local_datasets, {
  if (dataset_mutations_locked()) {
    return()
  }
  session$sendCustomMessage(
    "builder_import_status",
    list(text = "Choose dataset files…")
  )
  builder_schedule_native_picker(
    "dataset_files",
    key = "datasets",
    on_result = function(choice) {
      if (
        identical(choice$status, "cancelled") || dataset_mutations_locked(FALSE)
      ) {
        return()
      }
      if (!identical(choice$status, "selected")) {
        showNotification(
          choice$error %||% "The file picker could not be opened.",
          type = "error",
          duration = 6
        )
        return()
      }
      for (path in choice$paths) {
        info <- suppressWarnings(file.info(path))
        filename <- basename(path)
        start_load(
          "file",
          path,
          tools::file_path_sans_ext(filename),
          file_meta = list(
            name = filename,
            type = "",
            size = suppressWarnings(as.numeric(info$size[[1L]]))
          ),
          source_origin = "local"
        )
      }
    },
    on_error = function(error) {
      showNotification(conditionMessage(error), type = "error", duration = 6)
    }
  )
})

observeEvent(input$builder_import_example, {
  event <- input$builder_import_example
  client_id <- if (is.list(event) && !is.object(event)) {
    event$client_id
  } else {
    NULL
  }
  example_id <- if (is.list(event) && !is.object(event)) event$example else NULL
  ex <- Filter(
    function(e) identical(e$id, example_id),
    builder_examples()
  )
  if (!builder_has_text(client_id) || !length(ex)) {
    release_client_import(
      client_id,
      outcome = "rejected",
      message = "The selected example is unavailable."
    )
    return()
  }
  start_load(
    "example",
    ex[[1L]]$id,
    ex[[1L]]$label,
    client_id = client_id
  )
})

observeEvent(input$builder_import_sync_request, {
  ids <- isolate(client_import_server_ids())
  queue <- isolate(imports())
  active <- lapply(names(ids), function(server_id) {
    entry <- builder_import_find(queue, server_id)
    if (is.null(entry)) {
      return(NULL)
    }
    list(
      client_id = unname(ids[[server_id]]),
      server_id = server_id,
      state = entry$load_state,
      message = entry$error
    )
  })
  active <- Filter(Negate(is.null), active)
  terminal <- unname(isolate(released_client_import_records()))
  dispatch <- isolate(pending_client_import_dispatch())
  if (is.list(dispatch) && builder_has_text(dispatch$client_id)) {
    active <- c(
      active,
      list(list(
        client_id = dispatch$client_id,
        server_id = NULL,
        state = "awaiting_upload",
        message = NULL
      ))
    )
  }
  session$sendCustomMessage(
    "builder_import_sync",
    list(
      imports = unname(c(active, terminal)),
      server_busy = builder_has_text(isolate(external_import_active()))
    )
  )
})

remove_pending_import <- function(id) {
  entry <- isolate(import_of(id))
  if (
    !is.character(id) ||
      length(id) != 1L ||
      is.na(id) ||
      !nzchar(id) ||
      is.null(entry)
  ) {
    return(invisible(FALSE))
  }
  current_protocol <- isolate(protocol())
  if (is.null(current_protocol)) {
    return(invisible(FALSE))
  }
  belongs <- function(request) {
    !is.null(request) &&
      identical(request$dataset, id) &&
      identical(request$kind, "load")
  }
  running <- belongs(current_protocol$pending) ||
    any(vapply(current_protocol$awaiting_ack, belongs, logical(1)))
  if (running) {
    uploads <- isolate(pending_uploads())
    if (!is.null(uploads[[id]])) {
      uploads[[id]]$visible <- FALSE
      pending_uploads(uploads)
    }
    cancelled_loads(unique(c(cancelled_loads(), id)))
    forget_import(id)
    return(invisible(TRUE))
  }
  if (identical(entry$load_state, "error")) {
    release_pending_source(list(
      kind = "load",
      source = entry$source$kind,
      id = entry$id,
      path = entry$source$transport_path %||% entry$source$staged_path,
      example = entry$source$example,
      reservation_key = entry$source$reservation_key
    ))
    return(invisible(TRUE))
  }
  forgotten <- try(
    builder_protocol_forget_dataset(
      current_protocol,
      id,
      reason = "upload_cancelled"
    ),
    silent = TRUE
  )
  if (inherits(forgotten, "try-error")) {
    add_error("This file could not be removed while it was loading.")
    return(invisible(FALSE))
  }
  removed <- c(forgotten$failed, forgotten$discarded)
  if (!length(removed)) {
    return(invisible(FALSE))
  }
  protocol(forgotten$protocol)
  invisible(lapply(removed, function(request) {
    release_client_import(
      client_import_id_for(request$payload$id),
      server_id = request$payload$id,
      outcome = "cancelled"
    )
    release_pending_source(request$payload)
  }))
  invisible(TRUE)
}

observeEvent(input$cancel_pending_upload, {
  event <- input$cancel_pending_upload
  id <- if (is.list(event) && !is.object(event)) {
    .subset2(event, "id")
  } else {
    NULL
  }
  remove_pending_import(id)
})

observeEvent(input$remove_import, {
  event <- input$remove_import
  id <- if (is.list(event) && !is.object(event)) {
    .subset2(event, "id")
  } else {
    NULL
  }
  remove_pending_import(id)
})

observeEvent(input$pick_import, {
  event <- input$pick_import
  id <- if (is.list(event) && !is.object(event)) {
    .subset2(event, "id")
  } else {
    event
  }
  if (
    is.character(id) &&
      length(id) == 1L &&
      !is.na(id) &&
      !is.null(isolate(import_of(id)))
  ) {
    active_import_id(id)
  }
})

observeEvent(input$retry_import, {
  event <- input$retry_import
  id <- if (is.list(event) && !is.object(event)) {
    .subset2(event, "id")
  } else {
    NULL
  }
  entry <- if (is.character(id) && length(id) == 1L) {
    isolate(import_of(id))
  } else {
    NULL
  }
  if (is.null(entry) || !identical(entry$load_state, "error")) {
    return()
  }
  next_queue <- builder_import_retry(isolate(imports()), id)
  entry <- builder_import_find(next_queue, id)
  current_worker <- isolate(worker())
  progress_path <- NULL
  if (
    is.list(current_worker) &&
      is.character(current_worker$snapshot_root) &&
      length(current_worker$snapshot_root) == 1L &&
      dir.exists(current_worker$snapshot_root)
  ) {
    candidate <- try(
      builder_import_progress_path(
        current_worker$snapshot_root,
        id,
        entry$generation
      ),
      silent = TRUE
    )
    if (!inherits(candidate, "try-error")) {
      progress_path <- candidate
    }
  }
  imports(next_queue)
  active_import_id(id)
  external_import_active(id)
  session$sendCustomMessage(
    "builder_import_scheduler_state",
    list(active = TRUE, server_id = id)
  )
  cancelled_loads(setdiff(cancelled_loads(), id))
  retry_path <- entry$source$transport_path %||% entry$source$staged_path
  queued <- enqueue(list(
    kind = "load",
    source = entry$source$kind,
    id = entry$id,
    path = retry_path,
    retained_path = entry$source$staged_path,
    retain_source = builder_has_text(entry$source$transport_path),
    example = entry$source$example,
    source_origin = entry$source$origin,
    label = entry$label,
    filename = entry$filename,
    reservation_key = entry$source$reservation_key,
    import_generation = entry$generation,
    progress_path = progress_path,
    note = paste0("Loading ", entry$label, "…")
  ))
  if (!isTRUE(queued)) {
    external_import_active(NULL)
    session$sendCustomMessage(
      "builder_import_scheduler_state",
      list(active = FALSE, server_id = id)
    )
    set_import_state(
      id,
      "error",
      entry$generation,
      "The background worker is not ready yet."
    )
  }
})

schedule_failed_import_cleanup <- function(current_worker) {
  if (
    inherits(current_worker, "builder_worker") &&
      !length(current_worker$snapshot_registry)
  ) {
    failed_import_cleanup_pending(TRUE)
  }
  invisible(TRUE)
}

observe({
  if (!isTRUE(failed_import_cleanup_pending())) {
    return()
  }
  current_protocol <- protocol()
  current_worker <- worker()
  if (is.null(current_protocol) || is.null(current_worker)) {
    return()
  }
  if (length(current_worker$snapshot_registry)) {
    failed_import_cleanup_pending(FALSE)
    return()
  }
  if (!builder_protocol_is_quiescent(current_protocol)) {
    return()
  }
  failed_import_cleanup_pending(FALSE)
  recycled <- restart_worker_protocol(
    current_worker,
    current_protocol,
    "Reclaiming memory after a failed dataset import."
  )
  if (isTRUE(recycled)) {
    add_error(NULL)
  }
})

## -- dispatcher: send the next request when the worker is free ----------
observe({
  current_protocol <- protocol()
  if (is.null(current_protocol)) {
    return()
  }
  dispatched <- builder_protocol_dispatch(current_protocol)
  if (is.null(dispatched$request)) {
    return()
  }
  request <- dispatched$request
  nxt <- request$payload
  coordinator <- NULL
  auth_material <- NULL
  build_auth_accounts <- NULL
  handed_off <- FALSE
  if (identical(nxt$kind, "build")) {
    build_auth_accounts <- nxt$auth_accounts
    nxt$auth_accounts <- NULL
    request$payload$auth_accounts <- NULL
    dispatched$request <- builder_request_redact_auth(dispatched$request)
    dispatched$protocol <- builder_protocol_redact_auth(dispatched$protocol)
    current_protocol <- builder_protocol_redact_auth(current_protocol)
    on.exit(
      {
        build_auth_accounts <- NULL
        auth_material <- NULL
        current_protocol <- NULL
        dispatched$request <- NULL
        nxt$auth_accounts <- NULL
        request$payload$auth_accounts <- NULL
        latest_protocol <- isolate(protocol())
        if (!is.null(latest_protocol)) {
          protocol(builder_protocol_redact_auth(latest_protocol))
        }
        if (!is.null(coordinator) && !handed_off) {
          try(builder_coordinator_abort(coordinator), silent = TRUE)
        }
      },
      add = TRUE
    )
    protocol(dispatched$protocol)
  } else {
    protocol(dispatched$protocol)
  }
  current_worker <- worker()
  req(current_worker)
  if (identical(nxt$kind, "load")) {
    set_import_state(
      nxt$id,
      "reading",
      nxt$import_generation %||% 1L
    )
  }
  if (identical(nxt$kind, "build")) {
    # Legacy prohibition: never use `plan <- builder_make_plan` here.
    plan <- nxt$plan
    plan_error <- plan$error
    if (
      is.null(plan_error) &&
        length(plan$existing_targets) &&
        !isTRUE(plan$overwrite)
    ) {
      plan_error <- paste0(
        "These outputs already exist: ",
        paste(basename(plan$existing_targets), collapse = ", "),
        ". Choose another folder or replace the matching files."
      )
    }
    if (!is.null(plan_error)) {
      completed <- builder_protocol_complete(
        dispatched$protocol,
        builder_worker_response(
          request,
          list(error = plan_error)
        )
      )
      result(builder_result_failure(plan_error))
      protocol(builder_protocol_acknowledge(
        completed$protocol,
        request$request_id
      ))
      busy_note(NULL)
      return()
    }
    prior_state <- if (
      exists(
        "builder_selected_output_release_state",
        mode = "function",
        inherits = TRUE
      )
    ) {
      builder_selected_output_release_state(plan$out_dir)
    } else {
      NULL
    }
    coordinator <- try(
      builder_coordinator_prepare(
        plan,
        request$build_id,
        prior_state = prior_state
      ),
      silent = TRUE
    )
    if (inherits(coordinator, "try-error")) {
      plan_error <- conditionMessage(attr(coordinator, "condition"))
      completed <- builder_protocol_complete(
        dispatched$protocol,
        builder_worker_response(request, list(error = plan_error))
      )
      result(builder_release_error_result(
        plan_error,
        plan$output_release$directory
      ))
      protocol(builder_protocol_acknowledge(
        completed$protocol,
        request$request_id
      ))
      busy_note(NULL)
      return()
    }
    if (isTRUE(plan$app_auth$enabled)) {
      auth_material <- try(
        builder_auth_create_material(build_auth_accounts, coordinator$stage),
        silent = TRUE
      )
      build_auth_accounts <- NULL
      if (inherits(auth_material, "try-error")) {
        plan_error <- conditionMessage(attr(auth_material, "condition"))
        completed <- builder_protocol_complete(
          dispatched$protocol,
          builder_worker_response(request, list(error = plan_error))
        )
        protocol(builder_protocol_acknowledge(
          completed$protocol,
          request$request_id
        ))
        try(builder_coordinator_abort(coordinator), silent = TRUE)
        result(builder_release_error_result(
          plan_error,
          plan$output_release$directory
        ))
        busy_note(NULL)
        return()
      }
    }
    build_auth_accounts <- NULL
    nxt$plan <- plan
  }
  started_call <- try(
    switch(
      nxt$kind,
      load = if (identical(nxt$source, "file")) {
        builder_session_load(
          current_worker,
          nxt$id,
          if (isTRUE(nxt$retain_source)) {
            list(source = nxt$path, retained_path = nxt$retained_path)
          } else {
            nxt$path
          },
          request,
          progress_path = nxt$progress_path,
          import_generation = nxt$import_generation %||% 1L,
          .importer = if (isTRUE(nxt$retain_source)) {
            builder_project_load_retained_source
          } else {
            NULL
          }
        )
      } else {
        builder_session_example(
          current_worker,
          nxt$id,
          nxt$example,
          request,
          progress_path = nxt$progress_path,
          import_generation = nxt$import_generation %||% 1L
        )
      },
      preview = builder_session_preview(
        current_worker,
        nxt$id,
        nxt$reduction,
        nxt$group,
        BUILDER_PREVIEW_MAX,
        request
      ),
      projection_previews = builder_session_projection_previews(
        current_worker,
        nxt$id,
        nxt$projections,
        nxt$group,
        nxt$max_cells %||% BUILDER_PREVIEW_MAX,
        request
      ),
      trajectory_previews = builder_session_trajectory_previews(
        current_worker,
        nxt$id,
        nxt$trajectories,
        nxt$max_cells %||% BUILDER_PREVIEW_MAX,
        request
      ),
      coords = builder_session_coords(
        current_worker,
        nxt$id,
        nxt$image,
        BUILDER_PREVIEW_MAX,
        request
      ),
      spatial_preview = builder_session_spatial_preview(
        current_worker,
        nxt$id,
        nxt$default_projection,
        nxt$group,
        nxt$section,
        nxt$assay,
        nxt$layer,
        nxt$coordinate_transforms,
        BUILDER_PREVIEW_MAX,
        request
      ),
      align_all = builder_session_section_bounds(
        current_worker,
        nxt$id,
        nxt$sections,
        nxt$mode,
        nxt$extent_width,
        nxt$extent_height,
        nxt$um_per_px,
        nxt$dx,
        nxt$dy,
        nxt$scale,
        request
      ),
      build = builder_session_build(
        current_worker,
        nxt$plan,
        request,
        coordinator = coordinator,
        auth_material = auth_material
      ),
      drop = builder_session_drop(current_worker, nxt$id, request)
    ),
    silent = TRUE
  )
  if (inherits(started_call, "try-error")) {
    if (identical(nxt$kind, "build")) {
      release <- isolate(active_release())
      if (!is.null(release)) {
        try(builder_coordinator_abort(release$handle), silent = TRUE)
        active_release(NULL)
      }
    }
    dispatch_error <- conditionMessage(attr(started_call, "condition"))
    if (identical(nxt$kind, "load")) {
      dispatch_error <- builder_import_public_error(
        dispatch_error,
        nxt$path %||% character()
      )
    }
    restart_worker_protocol(
      current_worker,
      dispatched$protocol,
      dispatch_error
    )
    return()
  }
  if (identical(nxt$kind, "build")) {
    handed_off <- TRUE
    auth_material <- NULL
    active_release(list(
      id = request$build_id,
      handle = coordinator,
      plan = plan
    ))
    update_build_state(list(
      type = "start",
      id = request$build_id,
      revision = plan$revision
    ))
  }
  busy_note(nxt$note)
})

fail_spatial_preview <- function(payload) {
  if (!identical(payload$kind, "spatial_preview")) {
    return(invisible(FALSE))
  }
  failure <- builder_spatial_preview_failure(
    isolate(spatial_previews()),
    payload
  )
  spatial_previews(failure$cache)
  if (isTRUE(failure$matched)) {
    if (
      exists("alignment_server", inherits = TRUE) &&
        is.function(alignment_server$fail_preview_switch)
    ) {
      alignment_server$fail_preview_switch(
        payload$id,
        payload$section,
        payload$switch_token
      )
    } else {
      session$sendCustomMessage(
        "builder_dataset_switch_state",
        failure$message
      )
    }
  }
  invisible(TRUE)
}

builder_release_package_runtime <- function(builder_root) {
  source_root <- normalizePath(
    file.path(builder_root, "..", ".."),
    winslash = "/",
    mustWork = FALSE
  )
  source_checkout <- file.exists(file.path(source_root, "DESCRIPTION")) &&
    dir.exists(file.path(source_root, "R"))
  if (!source_checkout) {
    source_root <- .builder_worker_package_source(builder_root)
  }
  if (is.null(source_root)) {
    return(list(source_root = NULL, files = character()))
  }
  description <- read.dcf(file.path(source_root, "DESCRIPTION"))
  collate <- if ("Collate" %in% colnames(description)) {
    description[1L, "Collate"]
  } else {
    NA_character_
  }
  files <- if (is.na(collate) || !nzchar(collate)) {
    sort(list.files(
      file.path(source_root, "R"),
      pattern = "[.][Rr]$",
      full.names = TRUE
    ))
  } else {
    file.path(
      source_root,
      "R",
      scan(text = collate, what = character(), quiet = TRUE)
    )
  }
  if (!length(files) || any(!file.exists(files))) {
    stop("The source-tree release runtime is incomplete.", call. = FALSE)
  }
  list(source_root = source_root, files = files)
}

builder_start_release_settlement_process <- function(release, value) {
  runtime_files <- builder_release_runtime_files()
  package_runtime <- builder_release_package_runtime(runtime_files$root)
  callr::r_bg(
    function(
      handle,
      value,
      runtime_files,
      package_source,
      package_files
    ) {
      runtime <- new.env(parent = globalenv())
      runtime$`%||%` <- function(left, right) {
        if (is.null(left)) right else left
      }
      if (is.null(package_source)) {
        suppressPackageStartupMessages(library(CerebroNexus))
      } else {
        Sys.setenv(CEREBRO_PACKAGE_SOURCE = package_source)
        for (path in package_files) {
          sys.source(path, envir = runtime)
        }
      }
      for (name in c(
        "contract",
        "publish",
        "app_bundle",
        "report",
        "coordinator"
      )) {
        sys.source(runtime_files[[name]], envir = runtime)
      }
      if (!is.null(package_source)) {
        validator <- get0(
          ".viewerAuthValidateDatabase",
          envir = runtime,
          inherits = FALSE
        )
        verify_pair <- get0(
          "builder_auth_verify_database_pair",
          envir = runtime,
          inherits = FALSE
        )
        if (!is.function(validator) || !is.function(verify_pair)) {
          stop("The source-tree authentication runtime is incomplete.")
        }
        verify_formals <- formals(verify_pair)
        verify_formals$.validate <- quote(.viewerAuthValidateDatabase)
        formals(verify_pair) <- verify_formals
        assign(
          "builder_auth_verify_database_pair",
          verify_pair,
          envir = runtime
        )
      }
      handle <- runtime$builder_coordinator_claim(handle)
      runtime$builder_coordinator_settle(handle, value)
    },
    args = list(
      handle = release$handle,
      value = value,
      runtime_files = runtime_files,
      package_source = package_runtime$source_root,
      package_files = package_runtime$files
    ),
    supervise = TRUE,
    stdout = "|",
    stderr = "|"
  )
}

builder_release_settlement_result <- function(settled, release) {
  target <- release$handle$target %||% ""
  if (inherits(settled, "condition")) {
    try(builder_coordinator_abort(release$handle), silent = TRUE)
    return(builder_release_error_result(
      paste0(
        "The completed Build could not be published: ",
        conditionMessage(settled)
      ),
      target
    ))
  }
  if (
    !is.list(settled) ||
      !is.logical(settled$ok) ||
      length(settled$ok) != 1L ||
      is.na(settled$ok)
  ) {
    try(builder_coordinator_abort(release$handle), silent = TRUE)
    return(builder_release_error_result(
      "The release process returned an invalid result.",
      target
    ))
  }
  if (!isTRUE(settled$ok)) {
    recovery <- settled$recovery
    if (
      is.list(recovery) &&
        identical(recovery$state, "recovery_required")
    ) {
      return(builder_result_recovery_required(
        recovery$message %||% settled$error,
        recovery = recovery
      ))
    }
    return(builder_result_failure(
      settled$error %||% "The completed Build could not be published."
    ))
  }
  typed <- try(builder_as_result(settled$value), silent = TRUE)
  if (inherits(typed, "try-error")) {
    return(builder_result_failure(
      "The release process returned an unsupported Build result."
    ))
  }
  typed
}

acknowledge_builder_build <- function(request_id) {
  acknowledged <- try(
    builder_app_acknowledge_build(isolate(protocol()), request_id),
    silent = TRUE
  )
  if (inherits(acknowledged, "try-error")) {
    protocol(NULL)
    worker_available(FALSE)
    add_error(paste0(
      "The completed Build could not be acknowledged. ",
      "Restart this Builder session."
    ))
    return(invisible(FALSE))
  }
  protocol(acknowledged)
  invisible(TRUE)
}

finish_builder_release_settlement <- function(settled) {
  context <- isolate(release_settlement_context())
  release_settlement_process(NULL)
  release_settlement_context(NULL)
  if (!is.list(context) || !is.list(context$release)) {
    busy_note(NULL)
    add_error("The completed Build lost its release context.")
    return(invisible(FALSE))
  }
  value <- builder_release_settlement_result(settled, context$release)
  active <- isolate(active_release())
  if (
    is.null(active) ||
      !identical(active$id, context$build_id)
  ) {
    value <- builder_result_failure(
      "The completed Build no longer matches its release coordinator."
    )
  }
  result(value)
  update_build_state(builder_app_build_action(value, context$build_id))
  if (identical(value$state, "success") && isTRUE(value$published)) {
    selected_output_release_state(NULL)
    selected_output(NULL)
  }
  active_release(NULL)
  acknowledge_builder_build(context$request_id)
  busy_note(NULL)
  build_flow(list(stage = "idle", plan = NULL))
  invisible(TRUE)
}

poll_builder_release_settlement <- function() {
  process <- isolate(release_settlement_process())
  if (is.null(process) || builder_session_closed()) {
    return(invisible(FALSE))
  }
  alive <- tryCatch(process$is_alive(), error = identity)
  if (inherits(alive, "condition")) {
    return(finish_builder_release_settlement(alive))
  }
  if (isTRUE(alive)) {
    later::later(
      function() {
        shiny::withReactiveDomain(
          builder_lifecycle_session,
          poll_builder_release_settlement()
        )
      },
      delay = 0.1
    )
    return(invisible(TRUE))
  }
  settled <- tryCatch(process$get_result(), error = identity)
  finish_builder_release_settlement(settled)
}

start_builder_release_settlement <- function(release, value, request) {
  existing_process <- isolate(release_settlement_process())
  if (!is.null(existing_process)) {
    existing_context <- isolate(release_settlement_context())
    if (
      is.list(existing_context) &&
        identical(existing_context$request_id, request$request_id)
    ) {
      return(invisible(TRUE))
    }
    add_error("Another release is already being finalized.")
    return(invisible(FALSE))
  }
  if (
    !is.list(release) ||
      !is.list(release$handle) ||
      !identical(release$id, request$build_id)
  ) {
    actual_release <- isolate(active_release())
    mismatch <- builder_result_failure(
      "The completed Build no longer matches its release coordinator."
    )
    settled <- if (is.list(actual_release) && is.list(actual_release$handle)) {
      builder_app_settle_release(actual_release, mismatch)
    } else {
      mismatch
    }
    result(settled)
    update_build_state(builder_app_build_action(settled, request$build_id))
    acknowledge_builder_build(request$request_id)
    active_release(NULL)
    busy_note(NULL)
    build_flow(list(stage = "idle", plan = NULL))
    return(invisible(FALSE))
  }
  busy_note("Verifying and publishing…")
  release_settlement_context(list(
    release = release,
    build_id = request$build_id,
    request_id = request$request_id
  ))
  process <- tryCatch(
    builder_start_release_settlement_process(release, value),
    error = identity
  )
  if (inherits(process, "condition")) {
    settled <- list(
      ok = FALSE,
      value = NULL,
      error = paste0(
        "The release process could not start: ",
        conditionMessage(process)
      ),
      target = release$handle$target,
      recovery = tryCatch(
        {
          try(builder_coordinator_abort(release$handle), silent = TRUE)
          builder_coordinator_recovery(release$handle$target)
        },
        error = function(error) NULL
      )
    )
    finish_builder_release_settlement(settled)
    return(invisible(FALSE))
  }
  release_settlement_process(process)
  later::later(
    function() {
      shiny::withReactiveDomain(
        builder_lifecycle_session,
        poll_builder_release_settlement()
      )
    },
    delay = 0
  )
  invisible(TRUE)
}

stop_builder_release_settlement <- function() {
  process <- isolate(release_settlement_process())
  if (is.null(process)) {
    return(TRUE)
  }
  context <- isolate(release_settlement_context())
  alive <- tryCatch(process$is_alive(), error = function(error) NA)
  if (isTRUE(alive)) {
    try(process$kill(), silent = TRUE)
    try(process$wait(timeout = 5000L), silent = TRUE)
    alive <- tryCatch(process$is_alive(), error = function(error) NA)
  }
  safe <- identical(alive, FALSE)
  if (safe) {
    if (is.list(context) && is.list(context$release$handle)) {
      try(builder_coordinator_abort(context$release$handle), silent = TRUE)
      active <- isolate(active_release())
      if (
        is.list(active) &&
          identical(active$id, context$build_id)
      ) {
        active_release(NULL)
      }
    }
    release_settlement_process(NULL)
    release_settlement_context(NULL)
  }
  safe
}

## -- one poller drains whatever the worker was asked to do ---------------
observe({
  current_protocol <- protocol()
  if (is.null(current_protocol) || is.null(current_protocol$pending)) {
    return()
  }
  current_worker <- worker()
  req(current_worker)
  invalidateLater(100, session)
  got <- try(builder_session_poll(current_worker), silent = TRUE)
  if (inherits(got, "try-error")) {
    poll_error <- conditionMessage(attr(got, "condition"))
    pending_payload <- current_protocol$pending$payload
    if (identical(pending_payload$kind, "load")) {
      poll_error <- builder_import_public_error(
        poll_error,
        pending_payload$path %||% character()
      )
    }
    restart_worker_protocol(
      current_worker,
      current_protocol,
      poll_error
    )
    return()
  }
  worker(got$worker)
  if (identical(got$event, "restarted")) {
    apply_protocol_recovery(
      current_protocol,
      got$worker,
      "The background worker stopped before returning its result.",
      retry_persistent = TRUE
    )
    return()
  }
  if (identical(got$event, "restart_failed")) {
    apply_protocol_recovery(
      current_protocol,
      got$worker,
      "The background worker stopped and could not be restored.",
      retry_persistent = FALSE,
      error = got$error %||% "The background worker could not restart."
    )
    return()
  }
  request <- current_protocol$pending
  p <- request$payload
  if (is.null(got$result)) {
    if (identical(p$kind, "load") && !is.null(p$progress_path)) {
      progress <- builder_import_progress_read(
        p$progress_path,
        p$import_generation %||% 1L
      )
      if (!is.null(progress)) {
        set_import_state(
          p$id,
          progress$stage,
          progress$generation
        )
      }
    }
    return()
  }
  if (!is.null(got$result$error)) {
    worker_error <- if (identical(p$kind, "load")) {
      builder_import_public_error(
        got$result$error,
        p$path %||% character()
      )
    } else {
      got$result$error
    }
    if (identical(request$kind, "build")) {
      release <- isolate(active_release())
      release_result <- builder_result_failure(worker_error)
      if (!is.null(release)) {
        release_result <- abort_release_result(release, worker_error)
        active_release(NULL)
      }
      result(release_result)
    } else {
      add_error(worker_error)
    }
    fail_spatial_preview(p)
    restart_worker_protocol(
      got$worker,
      current_protocol,
      worker_error
    )
    return()
  }
  completed <- builder_protocol_complete(
    current_protocol,
    got$result$value
  )
  if (!is.null(completed$protocol$pending)) {
    restart_worker_protocol(
      got$worker,
      current_protocol,
      "A worker response did not match the pending request."
    )
    return()
  }
  protocol(completed$protocol)
  busy_note(NULL)
  if (!isTRUE(completed$accepted)) {
    if (identical(request$kind, "build")) {
      release <- isolate(active_release())
      if (
        is.null(release) ||
          !identical(release$id, request$build_id)
      ) {
        release <- NULL
      }
      start_builder_release_settlement(
        release,
        builder_result_failure(
          "A stale Build result was rejected. Retry the action."
        ),
        request
      )
      return()
    }
    if (identical(p$kind, "load")) {
      release_client_import(
        client_import_id_for(p$id),
        server_id = p$id,
        outcome = "error",
        message = "A stale dataset import result was rejected."
      )
    }
    release_pending_source(p)
    if (isTRUE(request$persistent)) {
      protocol(builder_protocol_acknowledge(
        protocol(),
        request$request_id
      ))
      if (identical(request$kind, "drop")) {
        restart_worker_protocol(
          got$worker,
          protocol(),
          "A stale dataset release result was rejected."
        )
        return()
      }
      add_error(
        "A stale persistent worker result was discarded. Retry the action."
      )
    }
    return()
  }
  value <- completed$value
  if (!is.null(completed$error)) {
    fail_spatial_preview(p)
    cancelled <- identical(p$kind, "load") && p$id %in% cancelled_loads()
    if (identical(request$kind, "build")) {
      release <- isolate(active_release())
      if (
        is.null(release) ||
          !identical(release$id, request$build_id)
      ) {
        release <- NULL
      }
      start_builder_release_settlement(
        release,
        builder_result_failure(completed$error),
        request
      )
      return()
    } else if (identical(p$kind, "load") && !cancelled) {
      set_import_state(
        p$id,
        "error",
        p$import_generation %||% 1L,
        completed$error
      )
      builder_import_progress_remove(p$progress_path %||% "")
      add_error(NULL)
    } else if (!cancelled) {
      add_error(completed$error)
    }
    if (cancelled) {
      release_pending_source(p)
    }
    if (isTRUE(request$persistent)) {
      protocol(builder_protocol_acknowledge(
        protocol(),
        request$request_id
      ))
    }
    if (identical(p$kind, "load")) {
      schedule_failed_import_cleanup(got$worker)
    }
    return()
  }
  if (identical(p$kind, "load")) {
    cancelled <- p$id %in% cancelled_loads()
    if (!is.null(value$error)) {
      if (!cancelled) {
        set_import_state(
          p$id,
          "error",
          p$import_generation %||% 1L,
          value$error
        )
        builder_import_progress_remove(p$progress_path %||% "")
        add_error(NULL)
      } else {
        release_pending_source(p)
      }
      protocol(builder_protocol_acknowledge(protocol(), request$request_id))
      schedule_failed_import_cleanup(got$worker)
      return()
    }
    pending_entry <- isolate(import_of(p$id))
    if (
      is.null(pending_entry) ||
        !identical(
          pending_entry$generation,
          as.integer(p$import_generation %||% 1L)
        )
    ) {
      cancelled <- TRUE
    }
    if (cancelled) {
      updated_worker <- try(
        builder_worker_register_snapshot(got$worker, p$id, value$snapshot),
        silent = TRUE
      )
      if (inherits(updated_worker, "try-error")) {
        restart_worker_protocol(
          got$worker,
          protocol(),
          conditionMessage(attr(updated_worker, "condition"))
        )
        return()
      }
      worker(updated_worker)
      identity <- .builder_worker_identity(value$snapshot)
      accepted <- builder_protocol_dataset(protocol(), p$id, 1L, identity)
      acknowledged <- try(
        builder_protocol_acknowledge(accepted, request$request_id),
        silent = TRUE
      )
      if (inherits(acknowledged, "try-error")) {
        protocol(NULL)
        worker_available(FALSE)
        add_error(paste0(
          "The cancelled upload could not be released safely. ",
          "Restart this Builder session."
        ))
        return()
      }
      protocol(acknowledged)
      pending_drops <- pending_snapshot_drops()
      pending_drops[[p$id]] <- identity
      pending_snapshot_drops(pending_drops)
      release_pending_source(p)
      queued <- enqueue(list(
        kind = "drop",
        id = p$id,
        dataset_revision = 1L,
        snapshot_identity = identity,
        note = "Releasing cancelled upload…"
      ))
      if (!isTRUE(queued)) {
        pending_drops[[p$id]] <- NULL
        pending_snapshot_drops(pending_drops)
        add_error(
          "The cancelled upload will be released when this session closes."
        )
      }
      return()
    }
    profile <- value$profile
    set_import_state(
      p$id,
      "preparing",
      p$import_generation %||% 1L
    )
    recommendations <- list(
      metadata = builder_recommend_metadata(value$dataset_profile)
    )
    settings <- builder_default_settings(
      profile,
      unique_name(p$label),
      dataset_profile = value$dataset_profile
    )
    settings$recommendations <- recommendations
    settings$metadata_policy <- recommendations$metadata
    loaded_path <- value$retained_path %||% p$retained_path %||% p$path
    entry <- list(
      id = p$id,
      source_id = p$id,
      output_id = p$id,
      selector_value = p$id,
      path = loaded_path,
      filename = p$filename %||% basename(loaded_path),
      source_origin = p$source_origin %||%
        if (identical(p$source, "example")) "example" else "upload",
      ## Which built-in example produced this, so removing it puts the
      ## example back on offer. NULL for anything read from a file.
      example = p$example,
      format = value$format,
      profile = profile,
      dataset_profile = value$dataset_profile,
      snapshot = value$snapshot,
      revision = 1L,
      ## Level names per grouping variable, in the order the exporter will
      ## produce them -- the keys a configured palette has to match.
      levels = value$levels %||% list(),
      settings = settings
    )
    prepared <- try(
      builder_prepare_loaded_entry_attachment(entry),
      silent = TRUE
    )
    if (inherits(prepared, "try-error")) {
      message <- conditionMessage(attr(prepared, "condition"))
      released <- try(.builder_snapshot_release(value$snapshot), silent = TRUE)
      acknowledged <- try(
        builder_protocol_acknowledge(protocol(), request$request_id),
        silent = TRUE
      )
      if (inherits(acknowledged, "try-error")) {
        protocol(NULL)
        worker_available(FALSE)
      } else {
        protocol(acknowledged)
      }
      set_import_state(
        p$id,
        "error",
        p$import_generation %||% 1L,
        error = message
      )
      add_error(
        if (isTRUE(released)) {
          message
        } else {
          paste0(
            message,
            " The temporary snapshot will be cleaned up when Builder stops."
          )
        }
      )
      builder_import_progress_remove(p$progress_path %||% "")
      release_pending_source(p)
      return()
    }
    entry <- prepared$entry
    next_state <- prepared$state
    updated_worker <- try(
      builder_worker_register_snapshot(
        got$worker,
        p$id,
        value$snapshot
      ),
      silent = TRUE
    )
    if (inherits(updated_worker, "try-error")) {
      restart_worker_protocol(
        got$worker,
        protocol(),
        conditionMessage(attr(updated_worker, "condition"))
      )
      return()
    }
    worker(updated_worker)
    started_at_ms <- suppressWarnings(as.numeric(
      pending_entry$started_at_ms %||% NA_real_
    ))
    entry$import_elapsed_ms <- if (
      length(started_at_ms) == 1L &&
        !is.na(started_at_ms) &&
        is.finite(started_at_ms)
    ) {
      max(0, as.numeric(Sys.time()) * 1000 - started_at_ms)
    } else {
      NULL
    }
    next_state <- builder_reduce_state(
      next_state,
      list(type = "replace", id = entry$id, entry = entry),
      trusted = TRUE
    )
    store(next_state)
    if (exists("builder_project_mark_restored_entry", mode = "function")) {
      builder_project_mark_restored_entry(entry)
    }
    protocol(builder_protocol_dataset(
      isolate(protocol()),
      p$id,
      entry$revision,
      .builder_worker_identity(entry$snapshot)
    ))
    watched <- identical(isolate(active_import_id()), p$id)
    next_current <- builder_import_ready_target(
      watched = watched,
      current_id = isolate(current()),
      loaded_id = p$id
    )
    if (!identical(next_current, isolate(current()))) {
      current(next_current)
    }
    session$sendCustomMessage(
      "builder_import_status",
      list(text = paste0(entry$settings$name, " is ready."))
    )
    set_import_state(
      p$id,
      "ready",
      p$import_generation %||% 1L
    )
    workflow(builder_reduce_workflow(
      isolate(workflow()),
      list(type = "datasets_ready")
    ))
    session$onFlushed(
      function() {
        session$sendCustomMessage("builder_focus_stage", list(id = "configure"))
      },
      once = TRUE
    )
    release_pending_source(p)
    result(NULL)
  } else if (identical(p$kind, "preview")) {
    if (identical(current(), p$id)) {
      preview_frame(value)
    }
  } else if (identical(p$kind, "projection_previews")) {
    projection_previews(builder_preview_cache_store(
      isolate(projection_previews()),
      p$id,
      value
    ))
  } else if (identical(p$kind, "trajectory_previews")) {
    trajectory_previews(builder_preview_cache_store(
      isolate(trajectory_previews()),
      p$id,
      value
    ))
  } else if (identical(p$kind, "coords")) {
    if (
      identical(current(), p$id) &&
        identical(active_slice(), p$image)
    ) {
      spatial_coords(value)
    }
  } else if (identical(p$kind, "spatial_preview")) {
    cache_key <- p$preview_cache_key %||%
      builder_spatial_preview_cache_key(p$id, p$section)
    cache <- isolate(spatial_previews())
    if (is.list(p$preview_contract)) {
      cache <- builder_spatial_preview_cache_store_if_match(
        cache,
        cache_key,
        p$preview_contract,
        value
      )
      spatial_previews(cache)
    }
    if (
      identical(current(), p$id) &&
        identical(active_slice(), p$section)
    ) {
      alignment_preview(value)
      if (isTRUE(value$available)) {
        spatial_coords(list(
          x = value$spatial$x,
          y = value$spatial$y,
          sx = value$spatial$x,
          sy = value$spatial$y
        ))
      }
    }
  } else if (identical(p$kind, "align_all")) {
    apply_section_bounds(p$id, value, p$picture)
  } else if (identical(p$kind, "build")) {
    release <- isolate(active_release())
    if (
      is.null(release) ||
        !identical(release$id, request$build_id)
    ) {
      release <- NULL
    }
    start_builder_release_settlement(release, value, request)
  } else if (identical(p$kind, "drop")) {
    active_state <- store()
    retained <- active_state$datasets
    if (is.list(active_state$last_removed)) {
      retained <- c(retained, list(active_state$last_removed$entry))
    }
    pending_drops <- pending_snapshot_drops()
    # The transition excludes other_drop_ids with a shared pending identity.
    released <- try(
      builder_snapshot_release_transition(
        worker = got$worker,
        id = p$id,
        identity = request$snapshot_identity,
        retained = retained,
        pending = pending_drops,
        release = function(worker, id, identity) {
          builder_worker_release_snapshot(
            worker,
            id,
            expected_identity = identity
          )
        },
        unregister = function(worker, id) {
          worker$snapshot_registry[[id]] <- NULL
          worker
        },
        identity_of = .builder_worker_identity
      ),
      silent = TRUE
    )
    if (inherits(released, "try-error")) {
      restart_worker_protocol(
        got$worker,
        protocol(),
        conditionMessage(attr(released, "condition"))
      )
      return()
    }
    worker(released$worker)
    pending_snapshot_drops(released$pending)
    all <- Filter(function(e) !identical(e$id, p$id), sets())
    sets(all)
    projection_previews(builder_preview_cache_drop_dataset(
      isolate(projection_previews()),
      p$id
    ))
    trajectory_previews(builder_preview_cache_drop_dataset(
      isolate(trajectory_previews()),
      p$id
    ))
    spatial_previews(builder_spatial_preview_cache_drop_dataset(
      isolate(spatial_previews()),
      p$id
    ))
    if (identical(current(), p$id)) {
      current(if (length(all)) all[[1]]$id else NULL)
      result(NULL)
    }
    acknowledged <- try(
      builder_protocol_acknowledge(protocol(), request$request_id),
      silent = TRUE
    )
    if (inherits(acknowledged, "try-error")) {
      protocol(NULL)
      worker_available(FALSE)
      add_error(paste0(
        "The dataset was removed, but its protocol acknowledgement failed. ",
        "Restart this Builder session."
      ))
      return()
    }
    forgotten <- try(
      builder_protocol_forget_dataset(acknowledged, p$id),
      silent = TRUE
    )
    if (inherits(forgotten, "try-error")) {
      protocol(NULL)
      worker_available(FALSE)
      add_error(paste0(
        "The dataset was removed, but its queued actions could not be ",
        "cleared. Restart this Builder session."
      ))
      return()
    }
    protocol(forgotten$protocol)
    cancelled <- length(forgotten$failed) + length(forgotten$discarded)
    if (cancelled) {
      showNotification(
        paste0(
          "Dataset removed; ",
          cancelled,
          " obsolete queued action",
          if (cancelled == 1L) " was" else "s were",
          " cancelled."
        ),
        type = "message",
        duration = 5
      )
    }
    return()
  }
  if (isTRUE(request$persistent) && !identical(request$kind, "build")) {
    protocol(builder_protocol_acknowledge(protocol(), request$request_id))
  }
})

update_enhance_histology_choices <- function(entry) {
  collection <- builder_image_collection_normalize(
    entry$settings$images %||% list()
  )
  choices <- lapply(collection, names)
  invisible(choices)
}

commit_enhance_images <- function(entry, images, internal = FALSE) {
  entry$settings$images <- builder_image_collection_normalize(images)
  changed <- replace_entry(entry, internal = internal)
  if (!isTRUE(changed)) {
    return(invisible(FALSE))
  }
  update_enhance_histology_choices(entry)
  invisible(entry)
}

## Take the per-section extents the worker computed and pair each with the
## one shared picture.
apply_section_bounds <- function(id, per_section, a) {
  if (is.null(a) || !length(per_section)) {
    return()
  }
  e <- isolate(entry_of(id))
  if (is.null(e)) {
    return()
  }
  paired <- builder_pair_sections(a, per_section)
  imgs <- utils::modifyList(e$settings$images %||% list(), paired)
  short <- names(Filter(function(x) x$outside > 0, paired))
  commit_enhance_images(e, imgs)

  ## Saying "done" when four of five slides have every cell off the image is
  ## how the earlier version of this hid its own bug.
  if (length(short)) {
    showNotification(
      paste0(
        "Applied to ",
        length(per_section),
        " sections, but cells still fall outside the image in: ",
        paste(short, collapse = ", "),
        ". Bounding-box mode usually works best for multi-section objects."
      ),
      type = "warning",
      duration = 10
    )
  } else {
    showNotification(
      paste0(
        "The image was fitted to the coordinates of all ",
        length(per_section),
        " sections."
      ),
      type = "message",
      duration = 5
    )
  }
}

output$add_error <- renderUI({
  msg <- add_error()
  if (is.null(msg)) {
    return(NULL)
  }
  div(class = "notice bad", msg)
})
