##----------------------------------------------------------------------------##
## Pure, session-local state for datasets that have not reached Builder state.
##
## A loading entry is deliberately small. It may contain a private source
## descriptor on the server, but UI helpers project only safe display fields.
## Ready datasets continue to use the established builder_state contract.
##----------------------------------------------------------------------------##

.builder_import_states <- c(
  "queued",
  "reading",
  "inspecting",
  "validating",
  "preparing",
  "ready",
  "error",
  "cancelled"
)

.builder_import_active_states <- c(
  "reading",
  "inspecting",
  "validating",
  "preparing"
)

.builder_import_labels <- c(
  queued = "Waiting to load",
  reading = "Reading dataset…",
  inspecting = "Checking cells, genes and metadata…",
  validating = "Validating detected content…",
  preparing = "Preparing dataset settings…",
  ready = "Ready",
  error = "Could not load dataset",
  cancelled = "Load cancelled"
)

.builder_import_abort <- function(message) {
  stop(structure(
    list(message = message, call = NULL),
    class = c("builder_import_state_error", "error", "condition")
  ))
}

.builder_import_or <- function(value, fallback) {
  if (is.null(value) || !length(value)) fallback else value
}

.builder_import_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

.builder_import_generation <- function(value) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 1 ||
      value > .Machine$integer.max ||
      value != floor(value)
  ) {
    .builder_import_abort("An import generation must be a positive integer.")
  }
  as.integer(value)
}

builder_import_public_error <- function(error, private_paths = character()) {
  message <- if (inherits(error, "condition")) {
    conditionMessage(error)
  } else {
    as.character(.builder_import_or(error, ""))
  }
  message <- paste(message, collapse = " ")
  message <- gsub("[\r\n\t]+", " ", message)
  message <- gsub("\\s+", " ", message, perl = TRUE)
  message <- trimws(message)

  paths <- unique(as.character(private_paths))
  paths <- paths[!is.na(paths) & nzchar(paths)]
  paths <- paths[order(nchar(paths), decreasing = TRUE)]
  for (path in paths) {
    message <- gsub(path, "the selected file", message, fixed = TRUE)
  }

  unsafe <- !nzchar(message) ||
    grepl(
      "callr subprocess|traceback|stack trace",
      message,
      ignore.case = TRUE
    ) ||
    grepl("(^|[[:space:]('\"=:])/[^[:space:]]", message) ||
    grepl("(^|[[:space:]('\"=:])\\\\\\\\", message) ||
    grepl("[A-Za-z]:[/\\\\]", message)
  if (unsafe) {
    return(paste(
      "The selected file could not be read.",
      "Check that it is a supported Seurat object and try again."
    ))
  }
  if (nchar(message, type = "chars") > 240L) {
    message <- paste0(substr(message, 1L, 237L), "…")
  }
  message
}

builder_import_entry <- function(
  id,
  label,
  source,
  filename = NULL,
  file_type = NULL,
  size = NA_real_,
  generation = 1L
) {
  if (!.builder_import_text(id) || !.builder_import_text(label)) {
    .builder_import_abort("A loading dataset requires an id and display name.")
  }
  if (!is.list(source) || is.object(source)) {
    .builder_import_abort(
      "A loading dataset requires an inert source descriptor."
    )
  }
  generation <- .builder_import_generation(generation)
  structure(
    list(
      id = id,
      label = label,
      source = source,
      filename = if (.builder_import_text(filename)) filename else NULL,
      file_type = if (.builder_import_text(file_type)) file_type else NULL,
      size = suppressWarnings(as.numeric(size)[1L]),
      generation = generation,
      started_at_ms = as.numeric(Sys.time()) * 1000,
      load_state = "queued",
      progress_label = unname(.builder_import_labels[["queued"]]),
      error = NULL,
      profile = NULL,
      settings = NULL
    ),
    class = c("builder_import_entry", "list")
  )
}

builder_import_queue <- function(entries = list(), max_active = 1L) {
  max_active <- .builder_import_generation(max_active)
  if (!is.list(entries)) {
    .builder_import_abort("Import entries must be a list.")
  }
  structure(
    list(entries = entries, max_active = max_active, revision = 0L),
    class = c("builder_import_queue", "list")
  )
}

.builder_import_queue_assert <- function(queue) {
  if (!inherits(queue, "builder_import_queue") || !is.list(queue)) {
    .builder_import_abort("Expected a typed import queue.")
  }
  invisible(queue)
}

builder_import_find <- function(queue, id) {
  .builder_import_queue_assert(queue)
  queue$entries[[id]]
}

builder_import_active_ids <- function(queue) {
  .builder_import_queue_assert(queue)
  names(Filter(
    function(entry) entry$load_state %in% .builder_import_active_states,
    queue$entries
  )) |>
    .builder_import_or(character())
}

builder_import_focus_id <- function(queue) {
  .builder_import_queue_assert(queue)
  ids <- names(Filter(
    function(entry) {
      !entry$load_state %in% c("ready", "error", "cancelled")
    },
    queue$entries
  ))
  if (length(ids)) ids[[1L]] else NULL
}

builder_import_add <- function(queue, entry) {
  .builder_import_queue_assert(queue)
  if (!inherits(entry, "builder_import_entry")) {
    .builder_import_abort("Expected a typed import entry.")
  }
  if (!is.null(queue$entries[[entry$id]])) {
    .builder_import_abort("Import dataset ids must be unique.")
  }
  queue$entries[[entry$id]] <- entry
  queue$revision <- as.integer(queue$revision) + 1L
  queue
}

.builder_import_allowed <- list(
  queued = c("reading", "error", "cancelled"),
  reading = c("inspecting", "validating", "preparing", "error", "cancelled"),
  inspecting = c("validating", "preparing", "error", "cancelled"),
  validating = c("preparing", "error", "cancelled"),
  preparing = c("ready", "error", "cancelled"),
  ready = character(),
  error = c("cancelled"),
  cancelled = character()
)

builder_import_transition <- function(
  queue,
  id,
  state,
  generation,
  error = NULL
) {
  .builder_import_queue_assert(queue)
  entry <- queue$entries[[id]]
  if (is.null(entry)) {
    .builder_import_abort("The import transition refers to an unknown dataset.")
  }
  generation <- .builder_import_generation(generation)
  if (!identical(generation, entry$generation)) {
    return(queue)
  }
  if (!.builder_import_text(state) || !state %in% .builder_import_states) {
    .builder_import_abort("The requested import state is not supported.")
  }
  if (identical(state, entry$load_state)) {
    return(queue)
  }
  if (!state %in% .builder_import_allowed[[entry$load_state]]) {
    .builder_import_abort(paste0(
      "Import state cannot move from ",
      entry$load_state,
      " to ",
      state,
      "."
    ))
  }
  if (
    state %in%
      .builder_import_active_states &&
      !entry$load_state %in% .builder_import_active_states &&
      length(builder_import_active_ids(queue)) >= queue$max_active
  ) {
    .builder_import_abort("The import worker concurrency limit was reached.")
  }
  entry$load_state <- state
  entry$progress_label <- unname(.builder_import_labels[[state]])
  if (identical(state, "error")) {
    private <- unlist(entry$source[c("staged_path", "path")], use.names = FALSE)
    entry$error <- builder_import_public_error(error, private)
    started_at_ms <- suppressWarnings(as.numeric(entry$started_at_ms))
    entry$import_elapsed_ms <- if (
      length(started_at_ms) == 1L &&
        !is.na(started_at_ms) &&
        is.finite(started_at_ms)
    ) {
      max(0, as.numeric(Sys.time()) * 1000 - started_at_ms)
    } else {
      NULL
    }
  } else {
    entry$error <- NULL
  }
  queue$entries[[id]] <- entry
  queue$revision <- as.integer(queue$revision) + 1L
  queue
}

builder_import_retry <- function(queue, id) {
  .builder_import_queue_assert(queue)
  entry <- queue$entries[[id]]
  if (is.null(entry) || !identical(entry$load_state, "error")) {
    .builder_import_abort("Only a failed import can be retried.")
  }
  entry$generation <- .builder_import_generation(entry$generation + 1L)
  entry$started_at_ms <- as.numeric(Sys.time()) * 1000
  entry$load_state <- "queued"
  entry$progress_label <- unname(.builder_import_labels[["queued"]])
  entry$error <- NULL
  entry$import_elapsed_ms <- NULL
  queue$entries[[id]] <- entry
  queue$revision <- as.integer(queue$revision) + 1L
  queue
}

builder_import_remove <- function(queue, id) {
  .builder_import_queue_assert(queue)
  if (!is.null(queue$entries[[id]])) {
    queue$entries[[id]] <- NULL
    queue$revision <- as.integer(queue$revision) + 1L
  }
  queue
}

builder_import_ready_target <- function(
  watched,
  current_id,
  loaded_id
) {
  if (isTRUE(watched)) {
    return(loaded_id)
  }
  if (.builder_import_text(current_id)) {
    return(current_id)
  }
  loaded_id
}

builder_import_auto_focus <- function(current_id, active_id, new_id) {
  if (.builder_import_text(active_id)) {
    return(active_id)
  }
  if (.builder_import_text(current_id)) {
    return(NULL)
  }
  new_id
}

## -- Private worker-to-app progress records --------------------------------

.builder_import_progress_token <- function(id) {
  if (!.builder_import_text(id)) {
    .builder_import_abort("A progress record requires a dataset id.")
  }
  bytes <- as.integer(charToRaw(enc2utf8(id)))
  positions <- seq_along(bytes)
  modulus <- 2147483629
  left <- sum((bytes + 17) * positions) %% modulus
  right <- sum((bytes + 31) * rev(positions)) %% modulus
  paste0(
    sprintf("%08x", as.integer(left)),
    sprintf("%08x", as.integer(right))
  )
}

builder_import_progress_path <- function(root, id, generation) {
  generation <- .builder_import_generation(generation)
  if (!.builder_import_text(root) || !dir.exists(root)) {
    .builder_import_abort("The private import progress directory is missing.")
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  file.path(
    root,
    paste0(
      ".import-",
      .builder_import_progress_token(id),
      "-g",
      generation,
      ".rds"
    )
  )
}

.builder_import_progress_record <- function(stage, generation, elapsed_ms) {
  generation <- .builder_import_generation(generation)
  if (
    !.builder_import_text(stage) || !stage %in% .builder_import_active_states
  ) {
    .builder_import_abort("The worker reported an unsupported import stage.")
  }
  elapsed_ms <- suppressWarnings(as.numeric(elapsed_ms)[1L])
  if (is.na(elapsed_ms) || !is.finite(elapsed_ms) || elapsed_ms < 0) {
    .builder_import_abort("Import progress elapsed time must be non-negative.")
  }
  list(
    stage = stage,
    generation = generation,
    elapsed_ms = elapsed_ms
  )
}

builder_import_progress_write <- function(
  path,
  stage,
  generation,
  elapsed_ms
) {
  if (!.builder_import_text(path) || !dir.exists(dirname(path))) {
    return(FALSE)
  }
  record <- .builder_import_progress_record(stage, generation, elapsed_ms)
  temporary <- tempfile(".progress-", tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saved <- try(saveRDS(record, temporary, version = 2), silent = TRUE)
  if (inherits(saved, "try-error")) {
    return(FALSE)
  }
  try(Sys.chmod(temporary, mode = "0600"), silent = TRUE)
  moved <- isTRUE(file.rename(temporary, path))
  if (!moved && file.exists(path)) {
    unlink(path, force = TRUE)
    moved <- isTRUE(file.rename(temporary, path))
  }
  if (moved) {
    try(Sys.chmod(path, mode = "0600"), silent = TRUE)
  }
  moved
}

builder_import_progress_read <- function(path, generation) {
  generation <- .builder_import_generation(generation)
  if (!.builder_import_text(path) || !file.exists(path)) {
    return(NULL)
  }
  record <- try(readRDS(path), silent = TRUE)
  if (
    inherits(record, "try-error") ||
      !is.list(record) ||
      !identical(names(record), c("stage", "generation", "elapsed_ms")) ||
      !identical(record$generation, generation)
  ) {
    return(NULL)
  }
  checked <- try(
    .builder_import_progress_record(
      record$stage,
      record$generation,
      record$elapsed_ms
    ),
    silent = TRUE
  )
  if (inherits(checked, "try-error")) NULL else checked
}

builder_import_progress_remove <- function(path) {
  if (!.builder_import_text(path)) {
    return(FALSE)
  }
  if (file.exists(path)) {
    unlink(path, force = TRUE)
  }
  !file.exists(path)
}

builder_import_progress_cleanup <- function(root) {
  if (!.builder_import_text(root) || !dir.exists(root)) {
    return(0L)
  }
  records <- list.files(
    root,
    pattern = "^\\.import-[0-9a-f]{16}-g[1-9][0-9]*\\.rds$",
    full.names = TRUE,
    all.files = TRUE
  )
  if (!length(records)) {
    return(0L)
  }
  existed <- file.exists(records)
  unlink(records[existed], force = TRUE)
  as.integer(sum(existed & !file.exists(records)))
}

.builder_import_progress_callback <- function(path, generation) {
  generation <- .builder_import_generation(generation)
  started <- proc.time()[["elapsed"]]
  function(stage) {
    if (is.null(path)) {
      return(invisible(FALSE))
    }
    elapsed <- (proc.time()[["elapsed"]] - started) * 1000
    invisible(builder_import_progress_write(
      path,
      stage,
      generation,
      elapsed
    ))
  }
}
