## Builder projects keep durable user choices separate from session-only
## workers. Schema v3 is an index: exact configuration lives in small typed
## sidecars while source-derived profiles are regenerated from source data.

.builder_project_schema_version <- 3L
.builder_project_supported_schema_versions <- c(1L, 2L, 3L)
.builder_project_config_schema_version <- 1L
.builder_project_configuration_contract_version <- 1L
.builder_project_manifest_max_bytes <- 10L * 1024L^2
.builder_project_inline_image_max_encoded_bytes <- 8L * 1024L^2

.builder_project_phases <- c(
  "none",
  "clean",
  "dirty",
  "choosing",
  "saving",
  "opening",
  "restoring",
  "registering",
  "save_failed",
  "conflict"
)

builder_activity_state <- function(
  connection = "connected",
  client_imports = 0L,
  server_imports = FALSE,
  project_phase = "none",
  spatial_dirty = FALSE,
  source_syncing = FALSE,
  build_locked = FALSE,
  has_project = FALSE,
  has_datasets = FALSE
) {
  if (
    !identical(connection, "connected") &&
      !identical(connection, "disconnected")
  ) {
    stop("A valid Builder connection state is required.", call. = FALSE)
  }
  client_imports <- suppressWarnings(as.integer(client_imports))
  if (
    length(client_imports) != 1L || is.na(client_imports) || client_imports < 0L
  ) {
    stop("A valid client import count is required.", call. = FALSE)
  }
  if (
    !is.logical(server_imports) ||
      length(server_imports) != 1L ||
      is.na(server_imports) ||
      !is.character(project_phase) ||
      length(project_phase) != 1L ||
      is.na(project_phase) ||
      !project_phase %in% .builder_project_phases ||
      !is.logical(spatial_dirty) ||
      length(spatial_dirty) != 1L ||
      is.na(spatial_dirty) ||
      !is.logical(source_syncing) ||
      length(source_syncing) != 1L ||
      is.na(source_syncing) ||
      !is.logical(build_locked) ||
      length(build_locked) != 1L ||
      is.na(build_locked) ||
      !is.logical(has_project) ||
      length(has_project) != 1L ||
      is.na(has_project) ||
      !is.logical(has_datasets) ||
      length(has_datasets) != 1L ||
      is.na(has_datasets)
  ) {
    stop("A valid Builder activity state is required.", call. = FALSE)
  }
  structure(
    list(
      connection = connection,
      client_imports = client_imports,
      server_imports = server_imports,
      project_phase = project_phase,
      spatial_dirty = spatial_dirty,
      source_syncing = source_syncing,
      build_locked = build_locked,
      has_project = has_project,
      has_datasets = has_datasets
    ),
    class = c("builder_activity_state", "list")
  )
}

builder_activity_capabilities <- function(activity) {
  if (!inherits(activity, "builder_activity_state")) {
    stop("A Builder activity state is required.", call. = FALSE)
  }
  connected <- identical(activity$connection, "connected")
  importing <- activity$client_imports > 0L || isTRUE(activity$server_imports)
  project_busy <- activity$project_phase %in%
    c("choosing", "saving", "opening", "restoring", "registering", "conflict")
  mutable <- connected && !isTRUE(activity$build_locked) && !project_busy
  stable <- mutable && !importing
  open_safe <- connected &&
    !isTRUE(activity$build_locked) &&
    !importing &&
    !isTRUE(activity$source_syncing) &&
    !activity$project_phase %in%
      c("choosing", "saving", "opening", "restoring", "registering")
  list(
    select_dataset = connected &&
      !activity$project_phase %in%
        c("choosing", "saving", "registering"),
    add_dataset = mutable,
    edit_dataset = mutable,
    mutate_datasets = mutable,
    check_dataset = stable,
    create_project = mutable &&
      !isTRUE(activity$has_project) &&
      isTRUE(activity$has_datasets),
    save_project = stable &&
      isTRUE(activity$has_project) &&
      activity$project_phase %in%
        c("clean", "dirty", "save_failed"),
    open_project = open_safe,
    prepare_crbs = stable && isTRUE(activity$has_project),
    navigate_workflow = stable,
    build = stable,
    page_inert = connected &&
      activity$project_phase %in%
        c("choosing", "saving", "opening", "restoring", "registering"),
    warn_before_unload = importing ||
      isTRUE(activity$build_locked) ||
      isTRUE(activity$source_syncing) ||
      isTRUE(activity$spatial_dirty) ||
      activity$project_phase %in%
        c(
          "dirty",
          "choosing",
          "saving",
          "opening",
          "restoring",
          "registering",
          "save_failed",
          "conflict"
        )
  )
}

builder_activity_reason <- function(activity, operation) {
  capabilities <- builder_activity_capabilities(activity)
  if (isTRUE(capabilities[[operation]])) {
    return(NULL)
  }
  if (identical(activity$connection, "disconnected")) {
    return("Reconnect to Builder before continuing.")
  }
  if (isTRUE(activity$build_locked)) {
    return("Wait for the active build to finish.")
  }
  if (identical(activity$project_phase, "conflict")) {
    return("Reopen the project before saving more changes.")
  }
  if (identical(activity$project_phase, "choosing")) {
    return("Choose a project folder before continuing.")
  }
  if (activity$project_phase %in% c("saving", "registering")) {
    return("Wait for the project operation to finish.")
  }
  if (activity$project_phase %in% c("opening", "restoring")) {
    return("Wait for the selected project datasets to finish loading.")
  }
  if (activity$client_imports > 0L || isTRUE(activity$server_imports)) {
    return("Wait for all dataset imports to finish.")
  }
  if (isTRUE(activity$source_syncing)) {
    return("Wait for the current Project source files to finish saving.")
  }
  switch(
    operation,
    create_project = "Add a dataset before creating a Builder project.",
    save_project = "Add a dataset before saving the project.",
    open_project = "Wait for the current operation to finish.",
    prepare_crbs = "Save a Builder project before preparing reusable CRBs.",
    "This action is not available right now."
  )
}

builder_project_abandoned_entries <- function(
  pending,
  live_ids = character(),
  import_ids = character(),
  operation = "idle"
) {
  if (!identical(operation, "restoring") || !length(pending)) {
    return(character())
  }
  setdiff(names(pending), c(live_ids, import_ids))
}

builder_project_live_dirty <- function(
  entries,
  checked_ids,
  manifest,
  identity_cache = NULL,
  ignored_ids = character()
) {
  if (!is.list(manifest) || !is.list(manifest$datasets)) {
    return(FALSE)
  }
  record_ids <- vapply(
    manifest$datasets,
    function(record) as.character(record$id),
    character(1)
  )
  records <- stats::setNames(manifest$datasets, record_ids)
  ids <- vapply(entries, `[[`, character(1), "id")
  if (anyDuplicated(ids) || any(!ids %in% names(records))) {
    return(TRUE)
  }
  saved_order <- names(sort(vapply(
    records,
    function(record) as.integer(record$order %||% 0L),
    integer(1)
  )))
  active_saved_order <- saved_order[vapply(
    records[saved_order],
    function(record) isTRUE(record$release$included %||% TRUE),
    logical(1)
  )]
  ignored_ids <- unique(as.character(ignored_ids %||% character()))
  ignored_ids <- ignored_ids[ignored_ids %in% active_saved_order]
  expected_live_order <- active_saved_order[
    !active_saved_order %in% ignored_ids
  ]
  if (!identical(ids, expected_live_order)) {
    return(TRUE)
  }
  any(vapply(
    seq_along(entries),
    function(index) {
      entry <- entries[[index]]
      record <- records[[ids[[index]]]]
      !identical(
        builder_project_cached_configuration_digest(entry, identity_cache),
        as.character(record$configuration$digest %||% "")
      ) ||
        !identical(
          ids[[index]] %in% checked_ids,
          isTRUE(record$configuration$checked)
        )
    },
    logical(1)
  ))
}

builder_project_retain_check_marks <- function(
  marks,
  live_ids,
  last_removed = NULL,
  can_undo_remove = FALSE
) {
  retained_ids <- as.character(live_ids %||% character())
  if (
    isTRUE(can_undo_remove) &&
      is.list(last_removed) &&
      .builder_project_identifier(last_removed$id %||% NULL)
  ) {
    retained_ids <- c(retained_ids, as.character(last_removed$id))
  }
  marks[names(marks) %in% unique(retained_ids)]
}
.builder_project_manifest_name <- "builder-project.json"
.builder_project_managed_root_names <- c(
  "artifacts",
  "checkpoints",
  "datasets",
  "session-sources",
  "spatial-assets",
  "sources"
)

.builder_project_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

.builder_project_integer <- function(value, default = 0L) {
  parsed <- suppressWarnings(as.integer(value %||% default))
  if (length(parsed) != 1L || is.na(parsed) || parsed < 0L) {
    return(as.integer(default))
  }
  parsed
}

.builder_project_identifier <- function(value) {
  .builder_project_text(value) &&
    nchar(value, type = "bytes") <= 128L &&
    grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", value)
}

builder_project_allocate_dataset_id <- function(
  sequence,
  existing_ids = character(),
  restored_id = NULL
) {
  sequence <- .builder_project_integer(sequence)
  existing_ids <- as.character(existing_ids)
  if (!is.null(restored_id)) {
    if (!.builder_project_identifier(restored_id)) {
      stop("A restored dataset requires a safe stable id.", call. = FALSE)
    }
    if (restored_id %in% existing_ids) {
      stop("The restored dataset id is already in use.", call. = FALSE)
    }
    matched <- regmatches(
      restored_id,
      regexec("^ds([0-9]+)$", restored_id)
    )[[1L]]
    if (length(matched) == 2L) {
      restored_sequence <- suppressWarnings(as.integer(matched[[2L]]))
      if (!is.na(restored_sequence)) {
        sequence <- max(sequence, restored_sequence)
      }
    }
    return(list(id = restored_id, sequence = as.integer(sequence)))
  }
  repeat {
    if (sequence >= .Machine$integer.max) {
      stop("No dataset id remains available in this session.", call. = FALSE)
    }
    sequence <- sequence + 1L
    id <- paste0("ds", sequence)
    if (!id %in% existing_ids) {
      return(list(id = id, sequence = as.integer(sequence)))
    }
  }
}

builder_project_allocation_ids <- function(
  entries,
  import_entries = list(),
  replacing_id = NULL
) {
  entry_ids <- vapply(
    entries %||% list(),
    function(entry) {
      as.character(entry$id %||% "")
    },
    character(1)
  )
  if (.builder_project_identifier(replacing_id)) {
    entry_ids <- entry_ids[entry_ids != replacing_id]
  }
  import_ids <- vapply(
    import_entries %||% list(),
    function(entry) {
      as.character(entry$id %||% "")
    },
    character(1)
  )
  c(entry_ids[nzchar(entry_ids)], import_ids[nzchar(import_ids)])
}

builder_project_reused_plan_matches <- function(saved, expected) {
  if (!is.list(saved) || !is.list(expected)) {
    return(FALSE)
  }
  fields <- c(
    "id",
    "name",
    "filename",
    "sidecars",
    "viewer_bundle_assets",
    "private_assets",
    "viewer_bundle_asset_claims",
    "private_asset_claims"
  )
  all(vapply(
    fields,
    function(field) identical(saved[[field]], expected[[field]]),
    logical(1)
  ))
}

builder_project_finish_pending_build <- function(
  manifest,
  status = c("completed", "failed", "interrupted"),
  error = NULL,
  finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
) {
  status <- match.arg(status)
  if (!is.list(manifest) || !is.list(manifest$pending_build)) {
    return(manifest)
  }
  manifest$pending_build$status <- status
  manifest$pending_build$finished_at <- as.character(finished_at)
  manifest$pending_build$error <- if (.builder_project_text(error)) {
    as.character(error)
  } else {
    NULL
  }
  manifest
}

builder_project_id <- function() {
  paste0(
    "project-",
    format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"),
    "-",
    sprintf("%08x", sample.int(.Machine$integer.max, 1L))
  )
}

builder_project_manifest_path <- function(root) {
  file.path(root, .builder_project_manifest_name)
}

builder_project_normalize_root <- function(root, must_work = TRUE) {
  if (!.builder_project_text(root)) {
    stop("Choose a project folder.", call. = FALSE)
  }
  root <- normalizePath(
    path.expand(root),
    winslash = "/",
    mustWork = must_work
  )
  if (!dir.exists(root)) {
    stop("The project folder is not available.", call. = FALSE)
  }
  root
}

builder_project_manifest_lock_path <- function(root) {
  file.path(
    builder_project_normalize_root(root),
    ".builder-project-write.lock"
  )
}

.builder_project_manifest_lock_owned <- function(path, token) {
  if (
    !.builder_project_text(path) ||
      !.builder_project_text(token) ||
      !dir.exists(path) ||
      nzchar(tryCatch(Sys.readlink(path), error = function(error) ""))
  ) {
    return(FALSE)
  }
  expected <- c("owner-token", "owner.txt")
  entries <- tryCatch(
    list.files(path, all.files = TRUE, no.. = TRUE),
    error = function(error) NULL
  )
  if (!is.character(entries) || !setequal(entries, expected)) {
    return(FALSE)
  }
  metadata <- file.path(path, expected)
  if (
    !all(utils::file_test("-f", metadata)) ||
      any(vapply(
        metadata,
        function(candidate) {
          nzchar(tryCatch(Sys.readlink(candidate), error = function(error) ""))
        },
        logical(1)
      ))
  ) {
    return(FALSE)
  }
  sizes <- suppressWarnings(as.double(file.info(metadata)$size))
  if (
    anyNA(sizes) ||
      any(sizes < 1) ||
      sizes[[1L]] > 4096 ||
      sizes[[2L]] > 65536
  ) {
    return(FALSE)
  }
  stored <- tryCatch(
    readLines(metadata[[1L]], warn = FALSE),
    error = function(error) NULL
  )
  is.character(stored) &&
    length(stored) == 1L &&
    identical(stored, token)
}

builder_project_acquire_manifest_lock <- function(root) {
  root <- builder_project_normalize_root(root)
  path <- builder_project_manifest_lock_path(root)
  if (!dir.create(path, mode = "0700", showWarnings = FALSE)) {
    stop(
      paste0(
        "Another Builder window is saving this project, or its write lock ",
        "needs recovery. Wait and retry; if no save is running, inspect the ",
        "stale lock in the project folder."
      ),
      call. = FALSE
    )
  }
  token <- basename(tempfile(pattern = paste0("owner-", Sys.getpid(), "-")))
  token_path <- file.path(path, "owner-token")
  owner_path <- file.path(path, "owner.txt")
  host <- unname(Sys.info()[["nodename"]])
  if (!.builder_project_text(host)) {
    host <- "unknown"
  }
  initialized <- tryCatch(
    {
      writeLines(token, token_path, useBytes = TRUE)
      writeLines(
        c(
          "schema=1",
          paste0("pid=", Sys.getpid()),
          paste0("host=", encodeString(host, quote = '"')),
          paste0("acquired_at=", format(Sys.time(), tz = "UTC", usetz = TRUE))
        ),
        owner_path,
        useBytes = TRUE
      )
      Sys.chmod(c(token_path, owner_path), mode = "0600")
      TRUE
    },
    error = function(error) error
  )
  if (inherits(initialized, "condition")) {
    stop(
      paste0(
        "The Project write lock could not be initialized. An incomplete ",
        "lock may remain and should be inspected before retrying."
      ),
      call. = FALSE
    )
  }
  structure(
    list(path = path, token = token, root = root),
    class = c("builder_project_manifest_lock", "list")
  )
}

builder_project_release_manifest_lock <- function(lock) {
  if (
    !inherits(lock, "builder_project_manifest_lock") ||
      !.builder_project_text(lock$path %||% NULL) ||
      !.builder_project_text(lock$token %||% NULL)
  ) {
    warning(
      "The Project write lock could not be verified and was preserved.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  if (!dir.exists(lock$path)) {
    return(invisible(FALSE))
  }
  if (!.builder_project_manifest_lock_owned(lock$path, lock$token)) {
    warning(
      "The Project write lock ownership changed and was preserved.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  isolated <- tempfile(
    pattern = ".builder-project-write.lock-release-",
    tmpdir = dirname(lock$path)
  )
  if (!file.rename(lock$path, isolated)) {
    warning(
      "The Project write lock could not be isolated for release.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  if (!.builder_project_manifest_lock_owned(isolated, lock$token)) {
    if (!dir.exists(lock$path)) {
      file.rename(isolated, lock$path)
    }
    warning(
      "The Project write lock changed during release and was preserved.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  metadata <- file.path(isolated, c("owner-token", "owner.txt"))
  file.remove(metadata)
  if (any(file.exists(metadata)) || !file.remove(isolated)) {
    warning(
      "The isolated Project write lock could not be removed.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

builder_project_folder_state <- function(root) {
  root <- builder_project_normalize_root(root)
  if (file.exists(builder_project_manifest_path(root))) {
    return(list(
      kind = "project",
      root = root,
      managed_conflicts = character()
    ))
  }
  entries <- list.files(root, all.files = TRUE, no.. = TRUE)
  managed_conflicts <- entries[
    tolower(entries) %in% tolower(.builder_project_managed_root_names)
  ]
  list(
    kind = if (length(entries)) "nonempty" else "empty",
    root = root,
    managed_conflicts = managed_conflicts
  )
}

builder_project_relative_path <- function(path, root) {
  root <- builder_project_normalize_root(root)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  prefix <- paste0(root, "/")
  if (!startsWith(path, prefix)) {
    stop("A managed project file escaped the project folder.", call. = FALSE)
  }
  substring(path, nchar(prefix) + 1L)
}

builder_project_resolve_path <- function(path, root, kind = "managed") {
  if (!.builder_project_text(path)) {
    return(NULL)
  }
  if (identical(kind, "external")) {
    return(normalizePath(path.expand(path), winslash = "/", mustWork = FALSE))
  }
  root <- builder_project_normalize_root(root)
  normalized <- gsub("\\\\", "/", as.character(path), fixed = FALSE)
  if (
    grepl("^/", normalized) ||
      grepl("^[A-Za-z]:/", normalized) ||
      grepl("^//", normalized)
  ) {
    stop("A managed project path is unsafe.", call. = FALSE)
  }
  parts <- strsplit(normalized, "/", fixed = TRUE)[[1L]]
  if (!length(parts) || any(parts %in% c("", ".", ".."))) {
    stop("A managed project path is unsafe.", call. = FALSE)
  }
  candidate <- root
  for (part in parts) {
    candidate <- file.path(candidate, part)
    link <- tryCatch(Sys.readlink(candidate), error = function(error) "")
    if (.builder_project_text(link)) {
      stop(
        "A managed project path contains a symbolic link.",
        call. = FALSE
      )
    }
  }
  prefix <- paste0(root, "/")
  resolved <- if (file.exists(candidate) || dir.exists(candidate)) {
    normalizePath(candidate, winslash = "/", mustWork = TRUE)
  } else {
    candidate
  }
  if (!startsWith(resolved, prefix)) {
    stop("A managed project path is unsafe.", call. = FALSE)
  }
  candidate
}

builder_project_file_fingerprint <- function(path, content = FALSE) {
  if (!.builder_project_text(path) || !file.exists(path) || dir.exists(path)) {
    return(NULL)
  }
  info <- file.info(path)
  list(
    bytes = as.double(info$size[[1L]]),
    modified_at = format(
      info$mtime[[1L]],
      "%Y-%m-%dT%H:%M:%OS3Z",
      tz = "UTC"
    ),
    md5 = if (isTRUE(content)) unname(tools::md5sum(path)) else NULL
  )
}

builder_project_file_fingerprint_with_md5 <- function(path, md5) {
  fingerprint <- builder_project_file_fingerprint(path, content = FALSE)
  if (is.null(fingerprint) || !.builder_project_text(md5)) {
    return(NULL)
  }
  fingerprint$md5 <- as.character(md5)
  fingerprint
}

builder_project_fingerprint_metadata_matches <- function(recorded, current) {
  if (is.null(recorded) || is.null(current)) {
    return(FALSE)
  }
  identical(
    suppressWarnings(as.double(recorded$bytes)),
    suppressWarnings(as.double(current$bytes))
  ) &&
    identical(
      as.character(recorded$modified_at %||% ""),
      as.character(current$modified_at %||% "")
    )
}

builder_project_content_fingerprint_matches <- function(recorded, current) {
  if (!is.list(recorded) || !is.list(current)) {
    return(FALSE)
  }
  recorded_md5 <- recorded$md5 %||% NULL
  current_md5 <- current$md5 %||% NULL
  if (
    .builder_project_text(recorded_md5) && .builder_project_text(current_md5)
  ) {
    return(identical(as.character(recorded_md5), as.character(current_md5)))
  }
  is.null(recorded_md5) &&
    is.null(current_md5) &&
    builder_project_fingerprint_metadata_matches(recorded, current)
}

builder_project_content_addressed_source <- function(path, id) {
  parts <- strsplit(as.character(path %||% ""), "/", fixed = TRUE)[[1L]]
  length(parts) >= 5L &&
    identical(parts[[1L]], "sources") &&
    identical(parts[[2L]], as.character(id %||% "")) &&
    identical(parts[[3L]], "blobs") &&
    grepl("^[[:xdigit:]]{32}$", parts[[4L]])
}

builder_project_managed_file_matches <- function(recorded, path) {
  metadata <- builder_project_file_fingerprint(path, content = FALSE)
  if (builder_project_fingerprint_metadata_matches(recorded, metadata)) {
    return(TRUE)
  }
  builder_project_content_fingerprint_matches(
    recorded,
    builder_project_file_fingerprint(path, content = TRUE)
  )
}

builder_project_example_source <- function(example_id, catalog) {
  if (!.builder_project_text(example_id) || !is.list(catalog)) {
    stop("A valid Builder example is required.", call. = FALSE)
  }
  record <- catalog[[example_id]] %||% NULL
  source <- if (is.list(record)) record$serialized_path %||% NULL else NULL
  if (!.builder_project_text(source) || !file.exists(source)) {
    stop("The Builder example source file was not found.", call. = FALSE)
  }
  list(
    origin = "example",
    example = example_id,
    path = normalizePath(source, winslash = "/", mustWork = TRUE),
    filename = basename(source)
  )
}

builder_project_snapshot_source_md5 <- function(entry) {
  fingerprint <- entry$snapshot$source_fingerprint %||% NULL
  if (
    !.builder_project_text(fingerprint) ||
      !startsWith(as.character(fingerprint), "builder-snapshot-v2:")
  ) {
    return(NULL)
  }
  md5 <- sub("^.*:", "", as.character(fingerprint))
  if (grepl("^[[:xdigit:]]{32}$", md5)) tolower(md5) else NULL
}

builder_project_source_job <- function(entry, root) {
  if (!is.list(entry) || !.builder_project_identifier(entry$id)) {
    stop("A dataset entry is required.", call. = FALSE)
  }
  root <- builder_project_normalize_root(root)
  source <- entry$path %||% NULL
  filename <- entry$filename %||%
    if (.builder_project_text(source)) {
      basename(source)
    } else {
      NULL
    }
  if (!.builder_project_text(filename)) {
    filename <- paste0(entry$id, ".rds")
  }
  filename <- basename(filename)
  target <- builder_project_resolve_path(
    paste("sources", entry$id, filename, sep = "/"),
    root,
    "managed"
  )
  list(
    id = entry$id,
    source = source,
    root = root,
    relative = paste("sources", entry$id, filename, sep = "/"),
    target = target,
    part = paste0(target, ".part"),
    filename = filename,
    source_md5 = builder_project_snapshot_source_md5(entry),
    origin = entry$source_origin %||%
      if (!is.null(entry$example)) {
        "example"
      } else {
        "upload"
      },
    example = entry$example %||% NULL
  )
}

builder_project_prepare_source <- function(entry, root, prior = NULL) {
  job <- builder_project_source_job(entry, root)
  relative <- job$relative
  prior_source <- if (is.list(prior)) prior$source %||% list() else list()
  prior_path <- if (
    identical(prior_source$kind %||% NULL, "managed") &&
      .builder_project_text(prior_source$path %||% NULL)
  ) {
    tryCatch(
      builder_project_resolve_path(prior_source$path, root, "managed"),
      error = function(error) NULL
    )
  } else {
    NULL
  }
  entry_path <- if (.builder_project_text(entry$path %||% NULL)) {
    tryCatch(
      normalizePath(entry$path, winslash = "/", mustWork = TRUE),
      error = function(error) NULL
    )
  } else {
    NULL
  }
  prior_content_matches <-
    identical(prior_source$status %||% NULL, "ready") &&
    builder_project_content_addressed_source(
      prior_source$path %||% NULL,
      entry$id
    ) &&
    .builder_project_text(job$source_md5) &&
    identical(
      tolower(as.character(job$source_md5)),
      tolower(as.character(prior_source$fingerprint$md5 %||% ""))
    )
  prior_ready <- .builder_project_text(prior_path) &&
    file.exists(prior_path) &&
    !dir.exists(prior_path) &&
    (identical(
      entry_path,
      normalizePath(prior_path, winslash = "/", mustWork = TRUE)
    ) ||
      prior_content_matches)
  ready_target <- if (prior_ready) prior_path else job$target
  if (prior_ready) {
    relative <- prior_source$path
  }
  target_ready <- file.exists(ready_target) && !dir.exists(ready_target)
  if (target_ready) {
    current_metadata <- builder_project_file_fingerprint(
      ready_target,
      content = FALSE
    )
    prior_fingerprint <- prior_source$fingerprint %||% NULL
    fingerprint <- if (
      identical(as.character(prior_source$path %||% ""), relative) &&
        .builder_project_text(prior_fingerprint$md5 %||% NULL) &&
        builder_project_fingerprint_metadata_matches(
          prior_fingerprint,
          current_metadata
        )
    ) {
      prior_fingerprint
    } else {
      builder_project_file_fingerprint(ready_target, content = TRUE)
    }
    return(list(
      entry = entry,
      source = list(
        kind = "managed",
        origin = job$origin,
        example = job$example,
        filename = job$filename,
        path = relative,
        status = "ready",
        fingerprint = fingerprint
      ),
      job = NULL
    ))
  }
  if (!.builder_project_text(job$source) || !file.exists(job$source)) {
    source <- prior_source
    source$status <- "failed"
    source$origin <- source$origin %||% job$origin
    source$example <- source$example %||% job$example
    source$filename <- source$filename %||% job$filename
    source$path <- source$path %||% relative
    source$error <- "The dataset source is no longer available."
    return(list(entry = entry, source = source, job = NULL))
  }
  list(
    entry = entry,
    source = list(
      kind = "managed",
      origin = job$origin,
      example = job$example,
      filename = job$filename,
      path = relative,
      status = "pending",
      fingerprint = NULL
    ),
    job = job
  )
}

builder_project_copy_file <- function(source, target) {
  cloned <- FALSE
  if (
    identical(unname(Sys.info()[["sysname"]]), "Darwin") &&
      file.exists("/bin/cp")
  ) {
    status <- suppressWarnings(system2(
      "/bin/cp",
      c("-c", "--", shQuote(source), shQuote(target)),
      stdout = FALSE,
      stderr = FALSE
    ))
    cloned <- isTRUE(status == 0L) && file.exists(target)
    if (!cloned) {
      unlink(target, force = TRUE)
    }
  }
  cloned ||
    file.copy(
      source,
      target,
      overwrite = TRUE,
      copy.mode = TRUE
    )
}

builder_project_copy_source_job <- function(job) {
  fail <- function(message) {
    if (is.character(job$part) && length(job$part) == 1L) {
      unlink(job$part, force = TRUE)
    }
    list(id = job$id, status = "failed", error = message)
  }
  if (
    !is.list(job) ||
      !.builder_project_identifier(job$id) ||
      !.builder_project_text(job$source) ||
      !file.exists(job$source)
  ) {
    return(fail("The dataset source is no longer available."))
  }
  source_md5 <- job$source_md5 %||% NULL
  if (
    !.builder_project_text(source_md5) ||
      !grepl("^[[:xdigit:]]{32}$", as.character(source_md5))
  ) {
    source_md5 <- unname(as.character(tools::md5sum(job$source)))
  }
  source_md5 <- tolower(as.character(source_md5))
  if (!.builder_project_text(source_md5)) {
    return(fail("The dataset source fingerprint could not be calculated."))
  }
  if (
    .builder_project_text(job$root %||% NULL) &&
      .builder_project_identifier(job$id %||% NULL)
  ) {
    job$relative <- paste(
      "sources",
      job$id,
      "blobs",
      source_md5,
      job$filename,
      sep = "/"
    )
    resolved <- tryCatch(
      builder_project_resolve_path(job$relative, job$root, "managed"),
      error = function(error) error
    )
    if (inherits(resolved, "condition")) {
      return(fail(conditionMessage(resolved)))
    }
    job$target <- resolved
    job$part <- paste0(resolved, ".part")
  }
  target_dir <- dirname(job$target)
  if (!dir.exists(target_dir) && !dir.create(target_dir, recursive = TRUE)) {
    return(fail("The Project source directory could not be created."))
  }
  if (file.exists(job$target)) {
    target_md5 <- unname(as.character(tools::md5sum(job$target)))
    if (identical(target_md5, source_md5)) {
      return(list(
        id = job$id,
        status = "ready",
        path = job$target,
        fingerprint = builder_project_file_fingerprint_with_md5(
          job$target,
          target_md5
        )
      ))
    }
    return(fail("An immutable Project source failed its integrity check."))
  }
  unlink(job$part, force = TRUE)
  if (!builder_project_copy_file(job$source, job$part)) {
    return(fail("The dataset source could not be copied."))
  }
  source_size <- suppressWarnings(as.numeric(file.info(job$source)$size[[1L]]))
  part_size <- suppressWarnings(as.numeric(file.info(job$part)$size[[1L]]))
  part_md5 <- unname(as.character(tools::md5sum(job$part)))
  if (!identical(source_size, part_size) || !identical(source_md5, part_md5)) {
    return(fail("The copied dataset source did not pass verification."))
  }
  if (!file.rename(job$part, job$target)) {
    if (file.exists(job$target)) {
      concurrent_md5 <- unname(as.character(tools::md5sum(job$target)))
      if (identical(concurrent_md5, source_md5)) {
        unlink(job$part, force = TRUE)
      } else {
        return(fail("The immutable Project source could not be committed."))
      }
    } else {
      return(fail("The verified Project source could not be committed."))
    }
  }
  list(
    id = job$id,
    status = "ready",
    path = job$target,
    fingerprint = builder_project_file_fingerprint_with_md5(
      job$target,
      part_md5
    )
  )
}

builder_project_copy_source_jobs <- function(jobs, progress_path = NULL) {
  results <- vector("list", length(jobs))
  failed <- 0L
  for (index in seq_along(jobs)) {
    results[[index]] <- builder_project_copy_source_job(jobs[[index]])
    failed <- failed + as.integer(identical(results[[index]]$status, "failed"))
    if (.builder_project_text(progress_path)) {
      temporary <- paste0(progress_path, ".tmp")
      saveRDS(
        list(
          completed = index,
          total = length(jobs),
          failed = failed,
          last = results[[index]][intersect(
            c("id", "status", "error"),
            names(results[[index]])
          )]
        ),
        temporary,
        version = 3L
      )
      moved <- try(fs::file_move(temporary, progress_path), silent = TRUE)
      if (inherits(moved, "try-error")) {
        unlink(temporary, force = TRUE)
      }
    }
  }
  results
}

builder_project_apply_source_results <- function(manifest, results, root) {
  if (!is.list(manifest) || !is.list(manifest$datasets)) {
    stop("A Builder project manifest is required.", call. = FALSE)
  }
  root <- builder_project_normalize_root(root)
  result_map <- stats::setNames(
    results,
    vapply(results, function(result) as.character(result$id), character(1))
  )
  manifest$datasets <- lapply(manifest$datasets, function(record) {
    result <- result_map[[record$id]] %||% NULL
    if (is.null(result)) {
      return(record)
    }
    source <- record$source %||% list(kind = "managed")
    source$status <- result$status %||% "failed"
    source$error <- result$error %||% NULL
    if (identical(source$status, "ready")) {
      path <- normalizePath(result$path, winslash = "/", mustWork = TRUE)
      prefix <- paste0(root, "/")
      if (!startsWith(path, prefix)) {
        stop("A synchronized source escaped the Project folder.", call. = FALSE)
      }
      source$path <- substring(path, nchar(prefix) + 1L)
      source$fingerprint <- result$fingerprint
    }
    record$source <- source
    record
  })
  manifest
}

builder_project_source_context <- function(
  project,
  generation,
  active_ids = character()
) {
  if (
    !is.list(project) ||
      !is.list(project$manifest) ||
      !is.list(project$manifest$project)
  ) {
    stop(
      "A source synchronization requires a saved project identity.",
      call. = FALSE
    )
  }
  project_id <- project$manifest$project$id %||% NULL
  revision <- project$manifest$project$revision %||% NULL
  if (
    !.builder_project_identifier(project_id) ||
      !is.numeric(generation) ||
      length(generation) != 1L ||
      is.na(generation) ||
      !is.finite(generation) ||
      generation < 0 ||
      !is.numeric(revision) ||
      length(revision) != 1L ||
      is.na(revision) ||
      !is.finite(revision) ||
      revision < 0
  ) {
    stop(
      "A source synchronization requires a saved project identity.",
      call. = FALSE
    )
  }
  root <- builder_project_normalize_root(project$root)
  path <- normalizePath(
    project$path %||% builder_project_manifest_path(root),
    winslash = "/",
    mustWork = FALSE
  )
  active_ids <- unique(as.character(active_ids %||% character()))
  if (
    length(active_ids) &&
      any(
        !vapply(
          active_ids,
          .builder_project_identifier,
          logical(1)
        )
      )
  ) {
    stop("Source synchronization dataset ids are invalid.", call. = FALSE)
  }
  list(
    generation = as.double(generation),
    project_id = as.character(project_id),
    root = root,
    path = path,
    revision = as.integer(revision),
    active_ids = active_ids
  )
}

builder_project_source_context_matches <- function(
  context,
  project,
  generation
) {
  if (!is.list(context) || !is.list(project)) {
    return(FALSE)
  }
  current <- tryCatch(
    builder_project_source_context(project, generation),
    error = function(error) NULL
  )
  is.list(current) &&
    all(vapply(
      c("generation", "project_id", "root", "path", "revision"),
      function(field) identical(context[[field]], current[[field]]),
      logical(1)
    ))
}

.builder_project_lexical_path <- function(path) {
  value <- gsub("\\\\", "/", path.expand(as.character(path)[[1L]]))
  if (!grepl("^/|^[A-Za-z]:/", value)) {
    value <- file.path(getwd(), value)
  }
  gsub("/+", "/", value)
}

.builder_project_rebase_lexical_path <- function(path, root) {
  lexical_root <- .builder_project_lexical_path(root)
  normalized_root <- normalizePath(
    lexical_root,
    winslash = "/",
    mustWork = TRUE
  )
  lexical_path <- .builder_project_lexical_path(path)
  if (
    identical(lexical_path, lexical_root) ||
      startsWith(lexical_path, paste0(lexical_root, "/"))
  ) {
    suffix <- substring(lexical_path, nchar(lexical_root) + 1L)
    lexical_path <- paste0(normalized_root, suffix)
  }
  list(path = lexical_path, root = normalized_root)
}

.builder_project_path_has_link_within <- function(
  path,
  root,
  allow_missing_leaf = FALSE
) {
  if (!.builder_project_text(path) || !.builder_project_text(root)) {
    return(TRUE)
  }
  rebased <- .builder_project_rebase_lexical_path(path, root)
  root <- rebased$root
  path <- rebased$path
  if (
    !identical(path, root) &&
      (!startsWith(path, paste0(root, "/")) ||
        any(strsplit(path, "/", fixed = TRUE)[[1L]] %in% c(".", "..")))
  ) {
    return(TRUE)
  }
  relative <- if (identical(path, root)) {
    ""
  } else {
    substring(path, nchar(root) + 2L)
  }
  components <- if (nzchar(relative)) {
    strsplit(relative, "/", fixed = TRUE)[[1L]]
  } else {
    character()
  }
  current <- root
  for (index in seq_along(components)) {
    component <- components[[index]]
    current <- file.path(current, component)
    link <- tryCatch(
      Sys.readlink(current),
      error = function(error) NA_character_
    )
    if (is.na(link)) {
      missing_leaf <- isTRUE(allow_missing_leaf) &&
        identical(index, length(components)) &&
        !file.exists(current) &&
        !dir.exists(current)
      if (!missing_leaf) {
        return(TRUE)
      }
      next
    }
    if (nzchar(link)) {
      return(TRUE)
    }
  }
  FALSE
}

builder_project_release_session_source <- function(path, session_root) {
  if (
    !.builder_project_text(path) ||
      !.builder_project_text(session_root) ||
      !dir.exists(session_root) ||
      !file.exists(path) ||
      dir.exists(path)
  ) {
    return(FALSE)
  }
  if (
    nzchar(tryCatch(
      Sys.readlink(path.expand(session_root)),
      error = function(error) ""
    ))
  ) {
    return(FALSE)
  }
  rebased <- .builder_project_rebase_lexical_path(path, session_root)
  root <- rebased$root
  source_root <- file.path(root, "session-sources")
  if (!dir.exists(source_root) || nzchar(Sys.readlink(source_root))) {
    return(FALSE)
  }
  candidate <- rebased$path
  if (
    !startsWith(candidate, paste0(source_root, "/")) ||
      .builder_project_path_has_link_within(candidate, root)
  ) {
    return(FALSE)
  }
  unlink(candidate, force = TRUE)
  if (file.exists(candidate)) {
    return(FALSE)
  }
  dataset_dir <- dirname(candidate)
  if (
    dir.exists(dataset_dir) &&
      identical(dirname(dataset_dir), source_root) &&
      !length(list.files(dataset_dir, all.files = TRUE, no.. = TRUE))
  ) {
    unlink(dataset_dir, recursive = FALSE, force = TRUE)
  }
  TRUE
}

builder_project_cleanup_session_sources <- function(session_root) {
  if (!.builder_project_text(session_root) || !dir.exists(session_root)) {
    return(FALSE)
  }
  root <- normalizePath(session_root, winslash = "/", mustWork = TRUE)
  source_root <- file.path(root, "session-sources")
  if (!dir.exists(source_root)) {
    return(TRUE)
  }
  if (
    nzchar(Sys.readlink(source_root)) ||
      .builder_project_path_has_link_within(source_root, root)
  ) {
    return(FALSE)
  }
  dataset_dirs <- list.files(
    source_root,
    all.files = TRUE,
    no.. = TRUE,
    full.names = TRUE
  )
  for (dataset_dir in dataset_dirs) {
    if (nzchar(Sys.readlink(dataset_dir)) || !dir.exists(dataset_dir)) {
      return(FALSE)
    }
    members <- list.files(
      dataset_dir,
      all.files = TRUE,
      no.. = TRUE,
      full.names = TRUE
    )
    if (
      length(members) &&
        any(vapply(
          members,
          function(member) {
            nzchar(Sys.readlink(member)) || dir.exists(member)
          },
          logical(1)
        ))
    ) {
      return(FALSE)
    }
  }
  unlink(source_root, recursive = TRUE, force = TRUE)
  !dir.exists(source_root)
}

builder_project_commit_source_entries <- function(
  entries,
  manifest,
  results,
  root,
  session_root = NULL
) {
  records <- stats::setNames(
    manifest$datasets %||% list(),
    vapply(
      manifest$datasets %||% list(),
      function(record) {
        as.character(record$id)
      },
      character(1)
    )
  )
  ready <- Filter(function(result) identical(result$status, "ready"), results)
  ready <- stats::setNames(
    ready,
    vapply(
      ready,
      function(result) as.character(result$id),
      character(1)
    )
  )
  committed <- lapply(entries, function(entry) {
    result <- ready[[entry$id]] %||% NULL
    record <- records[[entry$id]] %||% NULL
    if (is.null(result) || !is.list(record$source)) {
      return(list(entry = entry, released = NULL))
    }
    managed <- builder_project_resolve_path(record$source$path, root, "managed")
    if (
      !file.exists(managed) ||
        dir.exists(managed) ||
        !identical(
          normalizePath(result$path, winslash = "/", mustWork = TRUE),
          normalizePath(managed, winslash = "/", mustWork = TRUE)
        )
    ) {
      return(list(entry = entry, released = NULL))
    }
    prior <- entry$path %||% NULL
    entry$path <- normalizePath(managed, winslash = "/", mustWork = TRUE)
    entry$source <- record$source
    released <- if (
      .builder_project_text(session_root %||% NULL) &&
        .builder_project_text(prior %||% NULL) &&
        is.list(entry$snapshot %||% NULL) &&
        builder_project_release_session_source(prior, session_root)
    ) {
      entry$id
    } else {
      NULL
    }
    list(entry = entry, released = released)
  })
  list(
    entries = lapply(committed, `[[`, "entry"),
    released = unique(as.character(unlist(lapply(
      committed,
      `[[`,
      "released"
    ))))
  )
}

builder_project_retain_session_source <- function(
  source,
  filename,
  root,
  dataset_id
) {
  target <- builder_project_session_source_path(
    source,
    filename,
    root,
    dataset_id
  )
  target_dir <- dirname(target)
  if (!dir.exists(target_dir) && !dir.create(target_dir, recursive = TRUE)) {
    stop("The session source folder could not be created.", call. = FALSE)
  }
  same <- identical(
    normalizePath(source, winslash = "/", mustWork = TRUE),
    normalizePath(target, winslash = "/", mustWork = FALSE)
  )
  if (
    !same &&
      !builder_project_copy_file(source, target)
  ) {
    stop("The uploaded dataset source could not be retained.", call. = FALSE)
  }
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

builder_project_session_source_path <- function(
  source,
  filename,
  root,
  dataset_id
) {
  if (
    !.builder_project_text(source) ||
      !file.exists(source) ||
      !.builder_project_text(root) ||
      !dir.exists(root) ||
      !.builder_project_identifier(dataset_id)
  ) {
    stop("A valid uploaded dataset source is required.", call. = FALSE)
  }
  retained_name <- if (.builder_project_text(filename)) {
    basename(filename)
  } else {
    basename(source)
  }
  if (!nzchar(retained_name) || retained_name %in% c(".", "..")) {
    retained_name <- paste0(dataset_id, ".rds")
  }
  file.path(
    normalizePath(root, winslash = "/", mustWork = TRUE),
    "session-sources",
    dataset_id,
    retained_name
  )
}

builder_project_load_retained_source <- function(
  id,
  source,
  progress,
  .adapter = function(path) builder_seurat_file_adapter(path),
  .register = function(adapter, id, progress) {
    .builder_register_adapter(adapter, id, progress)
  },
  .copy = file.copy
) {
  scalar_text <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }
  if (!is.list(source) || !scalar_text(source$retained_path)) {
    stop("A retained dataset path is required.", call. = FALSE)
  }
  target <- path.expand(source$retained_path)
  if (!file.exists(target)) {
    if (!scalar_text(source$source) || !file.exists(source$source)) {
      stop("The uploaded dataset source is no longer available.", call. = FALSE)
    }
    target_dir <- dirname(target)
    if (!dir.exists(target_dir) && !dir.create(target_dir, recursive = TRUE)) {
      stop("The session source folder could not be created.", call. = FALSE)
    }
    part <- tempfile(
      pattern = paste0(".", basename(target), "-"),
      tmpdir = target_dir,
      fileext = ".part"
    )
    on.exit(unlink(part, force = TRUE), add = TRUE)
    copied <- .copy(
      source$source,
      part,
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
    if (!isTRUE(copied) || !file.rename(part, target)) {
      stop("The uploaded dataset source could not be retained.", call. = FALSE)
    }
  }
  target <- normalizePath(target, winslash = "/", mustWork = TRUE)
  value <- .register(.adapter(target), id, progress)
  value$retained_path <- target
  value
}

builder_project_configuration_entry <- function(entry) {
  if (!is.list(entry) || !.builder_project_identifier(entry$id %||% NULL)) {
    stop("A dataset entry is required.", call. = FALSE)
  }
  list(
    id = as.character(entry$id),
    revision = as.integer(entry$revision %||% 0L),
    settings = entry$settings %||% list(),
    acknowledgements = entry$acknowledgements %||% character(),
    spatial_drafts = entry$spatial_drafts %||% list()
  )
}

builder_project_table_asset_jobs <- function(entries, root) {
  root <- builder_project_normalize_root(root)
  jobs <- list()
  seen <- character()
  for (entry in entries %||% list()) {
    if (!is.list(entry) || !.builder_project_identifier(entry$id %||% NULL)) {
      stop("A dataset entry is required.", call. = FALSE)
    }
    for (record in entry$settings$tables %||% list()) {
      source <- record$source_path %||% ""
      if (!.builder_project_text(source) && is.data.frame(record$table)) {
        next
      }
      if (
        !.builder_project_text(source) ||
          !file.exists(source) ||
          dir.exists(source)
      ) {
        stop("An attachment source is no longer available.", call. = FALSE)
      }
      source <- normalizePath(source, winslash = "/", mustWork = TRUE)
      asset <- record$project_asset %||% list()
      source_fingerprint <- builder_project_file_fingerprint(
        source,
        content = FALSE
      )
      managed <- tryCatch(
        builder_project_resolve_path(asset$path %||% "", root, "managed"),
        error = function(error) NULL
      )
      if (
        .builder_project_text(managed) &&
          identical(source, managed) &&
          builder_project_fingerprint_metadata_matches(
            asset$fingerprint,
            source_fingerprint
          )
      ) {
        next
      }
      key <- paste(entry$id, source, sep = "\r")
      if (key %in% seen) {
        next
      }
      seen <- c(seen, key)
      jobs[[length(jobs) + 1L]] <- list(
        entry_id = entry$id,
        source = source,
        filename = basename(record$file_name %||% source),
        source_fingerprint = source_fingerprint
      )
    }
  }
  jobs
}

builder_project_stage_table_asset_jobs <- function(jobs, root) {
  lapply(jobs %||% list(), function(job) {
    if (
      !is.list(job) ||
        !.builder_project_identifier(job$entry_id %||% NULL) ||
        !.builder_project_text(job$source %||% NULL) ||
        !file.exists(job$source) ||
        dir.exists(job$source)
    ) {
      stop("A table attachment job is invalid.", call. = FALSE)
    }
    if (
      !builder_project_fingerprint_metadata_matches(
        job$source_fingerprint,
        builder_project_file_fingerprint(job$source, content = FALSE)
      )
    ) {
      stop(
        "A table attachment changed while it was being saved.",
        call. = FALSE
      )
    }
    staged <- builder_project_stage_table_assets(
      list(
        id = job$entry_id,
        settings = list(
          tables = list(
            attachment = list(
              file_name = basename(job$filename %||% job$source),
              source_path = job$source
            )
          )
        )
      ),
      root
    )
    asset <- staged$settings$tables[[1L]]$project_asset
    if (
      !builder_project_fingerprint_metadata_matches(
        job$source_fingerprint,
        builder_project_file_fingerprint(job$source, content = FALSE)
      )
    ) {
      stop(
        "A table attachment changed while it was being saved.",
        call. = FALSE
      )
    }
    list(
      entry_id = job$entry_id,
      source = normalizePath(job$source, winslash = "/", mustWork = TRUE),
      asset = asset
    )
  })
}

builder_project_apply_table_asset_results <- function(entries, results, root) {
  result_keys <- vapply(
    results %||% list(),
    function(result) {
      paste(result$entry_id %||% "", result$source %||% "", sep = "\r")
    },
    character(1)
  )
  lapply(entries %||% list(), function(entry) {
    tables <- entry$settings$tables %||% list()
    for (key in names(tables)) {
      source <- tables[[key]]$source_path %||% ""
      if (!.builder_project_text(source) || !file.exists(source)) {
        next
      }
      source <- normalizePath(source, winslash = "/", mustWork = TRUE)
      index <- match(paste(entry$id, source, sep = "\r"), result_keys)
      if (is.na(index)) {
        next
      }
      asset <- results[[index]]$asset %||% NULL
      if (!is.list(asset)) {
        stop("A staged table attachment is invalid.", call. = FALSE)
      }
      tables[[key]]$project_asset <- asset
      tables[[key]]$source_path <- builder_project_resolve_path(
        asset$path,
        root,
        "managed"
      )
      tables[[key]]$table <- NULL
    }
    entry$settings$tables <- tables
    entry
  })
}

builder_project_table_asset_results_match <- function(jobs, results) {
  key <- function(value) {
    paste(value$entry_id %||% "", value$source %||% "", sep = "\r")
  }
  expected <- vapply(jobs %||% list(), key, character(1))
  actual <- vapply(results %||% list(), key, character(1))
  length(expected) == length(actual) &&
    !anyDuplicated(actual) &&
    setequal(expected, actual) &&
    all(vapply(
      results %||% list(),
      function(result) is.list(result$asset),
      logical(1)
    ))
}

builder_project_table_save_signature <- function(project, entries) {
  if (!is.list(project) || !is.list(project$manifest$project)) {
    stop("A Builder project is required.", call. = FALSE)
  }
  records <- lapply(entries %||% list(), function(entry) {
    list(
      id = entry$id,
      revision = as.integer(entry$revision %||% 0L),
      configuration = builder_project_configuration_digest(entry)
    )
  })
  ids <- vapply(records, `[[`, character(1), "id")
  list(
    project_id = project$manifest$project$id,
    root = builder_project_normalize_root(project$root),
    manifest_revision = as.integer(project$manifest$project$revision %||% 0L),
    entries = records[order(ids, method = "radix")]
  )
}

builder_project_stage_table_assets <- function(entry, root) {
  tables <- entry$settings$tables %||% list()
  if (!length(tables)) {
    return(entry)
  }
  root <- builder_project_normalize_root(root)
  assets <- list()
  for (key in names(tables)) {
    record <- tables[[key]]
    source <- record$source_path %||% ""
    if (!.builder_project_text(source) && is.data.frame(record$table)) {
      next
    }
    if (
      !.builder_project_text(source) ||
        !file.exists(source) ||
        dir.exists(source)
    ) {
      stop(
        paste0(
          "Attachment ",
          record$display_name %||% key,
          " is no longer available. Add it again before saving."
        ),
        call. = FALSE
      )
    }
    source <- normalizePath(source, winslash = "/", mustWork = TRUE)
    project_asset <- record$project_asset %||% list()
    project_path <- tryCatch(
      builder_project_resolve_path(
        project_asset$path %||% "",
        root,
        "managed"
      ),
      error = function(error) NULL
    )
    if (
      .builder_project_text(project_path) &&
        identical(source, project_path) &&
        builder_project_fingerprint_metadata_matches(
          project_asset$fingerprint,
          builder_project_file_fingerprint(source, content = FALSE)
        )
    ) {
      record$source_path <- NULL
      record$table <- NULL
      tables[[key]] <- record
      next
    }
    asset <- assets[[source]]
    if (is.null(asset)) {
      filename <- basename(record$file_name %||% source)
      source_fingerprint <- builder_project_file_fingerprint(
        source,
        content = TRUE
      )
      target <- builder_project_resolve_path(
        file.path(
          "attachments",
          entry$id,
          paste0(
            .builder_project_asset_segment(filename),
            "-",
            substr(source_fingerprint$md5, 1L, 10L)
          )
        ),
        root,
        "managed"
      )
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      if (
        !file.exists(target) ||
          !builder_project_content_fingerprint_matches(
            source_fingerprint,
            builder_project_file_fingerprint(target, content = TRUE)
          )
      ) {
        temporary <- tempfile(
          "attachment-",
          tmpdir = dirname(target),
          fileext = ".part"
        )
        on.exit(unlink(temporary, force = TRUE), add = TRUE)
        if (
          !builder_project_copy_file(source, temporary) ||
            !file.rename(temporary, target)
        ) {
          stop(
            paste0("Attachment ", filename, " could not be saved."),
            call. = FALSE
          )
        }
      }
      asset <- list(
        path = builder_project_relative_path(target, root),
        fingerprint = builder_project_file_fingerprint_with_md5(
          target,
          source_fingerprint$md5
        )
      )
      assets[[source]] <- asset
    }
    record$project_asset <- asset
    record$source_path <- NULL
    record$table <- NULL
    tables[[key]] <- record
  }
  entry$settings$tables <- tables
  entry
}

builder_project_adopt_table_assets <- function(entry, staged, root = NULL) {
  if (!is.list(entry) || !is.list(staged)) {
    return(entry)
  }
  tables <- entry$settings$tables %||% list()
  staged_tables <- staged$settings$tables %||% list()
  for (key in intersect(names(tables), names(staged_tables))) {
    asset <- staged_tables[[key]]$project_asset %||% NULL
    tables[[key]]$project_asset <- asset
    if (!is.null(root) && is.list(asset)) {
      tables[[key]]$source_path <- tryCatch(
        builder_project_resolve_path(asset$path %||% "", root, "managed"),
        error = function(error) NULL
      )
    }
  }
  entry$settings$tables <- tables
  entry
}

builder_project_restore_table_assets <- function(entry, root) {
  if (is.null(entry$settings$tables)) {
    return(entry)
  }
  tables <- entry$settings$tables %||% list()
  fingerprints <- list()
  for (key in names(tables)) {
    record <- tables[[key]]
    asset <- record$project_asset %||% list()
    if (is.null(asset$path) && is.data.frame(record$table)) {
      next
    }
    path <- tryCatch(
      builder_project_resolve_path(asset$path %||% "", root, "managed"),
      error = function(error) NULL
    )
    fingerprint <- fingerprints[[path %||% ""]]
    if (
      !is.null(path) &&
        is.null(fingerprint) &&
        file.exists(path) &&
        !dir.exists(path)
    ) {
      fingerprint <- builder_project_file_fingerprint(path, content = FALSE)
      fingerprints[[path]] <- fingerprint
    }
    matches <- builder_project_fingerprint_metadata_matches(
      asset$fingerprint,
      fingerprint
    )
    if (
      !isTRUE(matches) &&
        is.null(fingerprint$md5 %||% NULL) &&
        !is.null(path) &&
        file.exists(path)
    ) {
      fingerprint <- builder_project_file_fingerprint(path, content = TRUE)
      fingerprints[[path]] <- fingerprint
      matches <- builder_project_content_fingerprint_matches(
        asset$fingerprint,
        fingerprint
      )
    }
    if (
      is.null(path) ||
        !file.exists(path) ||
        dir.exists(path) ||
        !isTRUE(matches)
    ) {
      stop(
        paste0(
          "Attachment ",
          record$display_name %||% key,
          " is missing or has changed."
        ),
        call. = FALSE
      )
    }
    record$source_path <- path
    tables[[key]] <- record
  }
  entry$settings$tables <- tables
  entry
}

builder_project_dataset_config_path <- function(
  dataset_id,
  root,
  content_id
) {
  if (
    !.builder_project_identifier(dataset_id) ||
      !.builder_project_text(content_id)
  ) {
    stop("A safe dataset id and content id are required.", call. = FALSE)
  }
  relative <- paste(
    "datasets",
    dataset_id,
    "configs",
    paste0(content_id, ".json"),
    sep = "/"
  )
  builder_project_resolve_path(
    relative,
    root,
    "managed"
  )
}

builder_project_write_dataset_config <- function(entry, root) {
  root <- builder_project_normalize_root(root)
  config <- builder_project_configuration_entry(entry)
  target_dir <- builder_project_resolve_path(
    paste("datasets", config$id, "configs", sep = "/"),
    root,
    "managed"
  )
  if (
    !dir.exists(target_dir) &&
      !dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  ) {
    stop(
      "The dataset configuration directory could not be created.",
      call. = FALSE
    )
  }
  temporary <- tempfile(
    paste0(config$id, "-config-"),
    tmpdir = target_dir,
    fileext = ".part"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  writeLines(
    jsonlite::serializeJSON(config, digits = NA, pretty = TRUE),
    temporary,
    useBytes = TRUE
  )
  staged_fingerprint <- builder_project_file_fingerprint(
    temporary,
    content = TRUE
  )
  target <- builder_project_dataset_config_path(
    config$id,
    root,
    staged_fingerprint$md5
  )
  if (file.exists(target)) {
    current <- builder_project_file_fingerprint(target, content = TRUE)
    if (
      !builder_project_content_fingerprint_matches(staged_fingerprint, current)
    ) {
      stop(
        "An immutable Project configuration failed its integrity check.",
        call. = FALSE
      )
    }
  } else if (!file.rename(temporary, target)) {
    stop("A Project configuration could not be committed.", call. = FALSE)
  }
  fingerprint <- builder_project_file_fingerprint_with_md5(
    target,
    staged_fingerprint$md5
  )
  list(
    schema_version = .builder_project_config_schema_version,
    path = builder_project_relative_path(target, root),
    revision = config$revision,
    fingerprint = fingerprint
  )
}

builder_project_read_dataset_config <- function(record, root) {
  configuration <- record$configuration %||% list()
  path <- configuration$path %||% NULL
  if (.builder_project_text(path)) {
    resolved <- builder_project_resolve_path(path, root, "managed")
    if (!file.exists(resolved) || dir.exists(resolved)) {
      stop("A saved dataset configuration is missing.", call. = FALSE)
    }
    current <- builder_project_file_fingerprint(resolved, content = TRUE)
    if (
      is.list(configuration$fingerprint) &&
        !builder_project_content_fingerprint_matches(
          configuration$fingerprint,
          current
        )
    ) {
      stop(
        "A saved dataset configuration failed its integrity check.",
        call. = FALSE
      )
    }
    payload <- paste(readLines(resolved, warn = FALSE), collapse = "\n")
    config <- jsonlite::unserializeJSON(payload)
    if (
      !is.list(config) ||
        !identical(as.character(config$id %||% ""), as.character(record$id))
    ) {
      stop("A saved dataset configuration is invalid.", call. = FALSE)
    }
    return(config)
  }
  stop("A saved dataset configuration is missing.", call. = FALSE)
}

.builder_project_asset_segment <- function(value) {
  value <- as.character(value %||% "")[[1L]]
  readable <- gsub("[^A-Za-z0-9._-]+", "-", value)
  readable <- gsub("(^-+|-+$)", "", readable)
  if (!nzchar(readable)) {
    readable <- "item"
  }
  readable <- substr(readable, 1L, 60L)
  digest <- unclass(as.character(openssl::md5(charToRaw(enc2utf8(value)))))
  paste0(readable, "-", substr(digest, 1L, 10L))
}

.builder_project_decode_image_uri <- function(
  uri,
  max_encoded_bytes = .builder_project_inline_image_max_encoded_bytes
) {
  if (!.builder_project_text(uri)) {
    stop("A Spatial image payload is missing.", call. = FALSE)
  }
  if (
    !is.numeric(max_encoded_bytes) ||
      length(max_encoded_bytes) != 1L ||
      is.na(max_encoded_bytes) ||
      !is.finite(max_encoded_bytes) ||
      max_encoded_bytes < 1
  ) {
    stop("The Spatial image encoded size limit is invalid.", call. = FALSE)
  }
  separator <- regexpr(",", uri, fixed = TRUE)[[1L]]
  if (separator < 1L) {
    stop("A Spatial image payload is not a supported data URI.", call. = FALSE)
  }
  header <- substring(uri, 1L, separator)
  matched <- regexec("^data:([^;,]+);base64,$", header, perl = TRUE)
  parts <- regmatches(header, matched)[[1L]]
  if (length(parts) != 2L) {
    stop("A Spatial image payload is not a supported data URI.", call. = FALSE)
  }
  encoded_bytes <- nchar(uri, type = "bytes") - separator
  if (encoded_bytes > max_encoded_bytes) {
    stop(
      "A Spatial image payload exceeds its encoded size limit.",
      call. = FALSE
    )
  }
  payload <- substring(uri, separator + 1L)
  bytes <- tryCatch(
    base64enc::base64decode(payload),
    error = function(error) NULL
  )
  if (is.null(bytes) || !length(bytes)) {
    stop("A Spatial image payload could not be decoded.", call. = FALSE)
  }
  list(mime = parts[[2L]], bytes = bytes)
}

.builder_project_map_spatial_images <- function(entry, transform) {
  images <- entry$settings$images %||% list()
  if (!is.list(images) || !length(images)) {
    return(entry)
  }
  for (section in names(images)) {
    collection <- images[[section]]
    legacy_record <- is.list(collection) &&
      any(
        c("source_uri", "uri", "project_asset") %in% names(collection)
      )
    if (legacy_record) {
      label <- collection$source$name %||% "image"
      images[[section]] <- transform(collection, section, as.character(label))
      next
    }
    if (!is.list(collection) || !length(collection)) {
      next
    }
    for (label in names(collection)) {
      images[[section]][[label]] <- transform(
        collection[[label]],
        section,
        label
      )
    }
  }
  entry$settings$images <- images
  entry
}

builder_project_stage_spatial_assets <- function(entry, root) {
  if (!is.list(entry) || !.builder_project_identifier(entry$id)) {
    stop("A dataset entry is required.", call. = FALSE)
  }
  root <- builder_project_normalize_root(root)
  staged <- entry
  .builder_project_map_spatial_images(staged, function(record, section, label) {
    if (!is.list(record)) {
      return(record)
    }
    field_order <- names(record)
    payload <- record$source_uri %||% record$uri %||% NULL
    asset <- record$project_asset %||% NULL
    fail <- function(reason) {
      stop(
        paste0(
          "Spatial image asset for dataset “",
          entry$id,
          "”, FOV “",
          section,
          "”, image “",
          label,
          "” ",
          reason,
          "."
        ),
        call. = FALSE
      )
    }
    if (is.list(asset)) {
      path <- tryCatch(
        builder_project_resolve_path(asset$path %||% "", root, "managed"),
        error = function(error) NULL
      )
      if (
        !.builder_project_text(path) || !file.exists(path) || dir.exists(path)
      ) {
        fail("is missing")
      }
      if (!builder_project_managed_file_matches(asset$fingerprint, path)) {
        fail("failed its integrity check")
      }
      record$source_content_md5 <- asset$fingerprint$md5 %||%
        record$source_content_md5 %||%
        NULL
      record$source_uri <- NULL
      record$uri <- NULL
      return(record)
    }
    parsed <- .builder_project_decode_image_uri(
      payload
    )
    extension <- switch(
      tolower(parsed$mime),
      "image/jpeg" = "jpg",
      "image/webp" = "webp",
      "png"
    )
    asset_id <- unclass(as.character(openssl::md5(parsed$bytes)))
    target_dir <- builder_project_resolve_path(
      paste(
        "spatial-assets",
        entry$id,
        .builder_project_asset_segment(section),
        sep = "/"
      ),
      root,
      "managed"
    )
    if (
      !dir.exists(target_dir) &&
        !dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    ) {
      stop("The Spatial asset directory could not be created.", call. = FALSE)
    }
    target <- file.path(target_dir, paste0(asset_id, ".", extension))
    if (
      !file.exists(target) ||
        !identical(
          unname(as.character(tools::md5sum(target))),
          asset_id
        )
    ) {
      part <- tempfile(
        pattern = paste0(asset_id, "-"),
        tmpdir = target_dir,
        fileext = ".part"
      )
      on.exit(unlink(part, force = TRUE), add = TRUE)
      writeBin(parsed$bytes, part)
      if (
        !identical(unname(as.character(tools::md5sum(part))), asset_id) ||
          !file.rename(part, target)
      ) {
        stop(
          "A Spatial image asset could not be committed safely.",
          call. = FALSE
        )
      }
    }
    record$project_asset <- list(
      schema_version = 1L,
      path = builder_project_relative_path(target, root),
      mime = parsed$mime,
      fingerprint = builder_project_file_fingerprint_with_md5(target, asset_id),
      field_order = field_order
    )
    record$source_content_md5 <- asset_id
    record$source_uri <- NULL
    record$uri <- NULL
    record
  })
}

builder_project_adopt_spatial_assets <- function(entry, staged) {
  if (!is.list(entry) || !is.list(staged)) {
    return(entry)
  }
  staged_record <- function(section, label) {
    collection <- staged$settings$images[[section]] %||% NULL
    legacy <- is.list(collection) &&
      any(c("source_uri", "uri", "project_asset") %in% names(collection))
    if (legacy) collection else collection[[label]] %||% NULL
  }
  .builder_project_map_spatial_images(entry, function(record, section, label) {
    saved <- staged_record(section, label)
    if (
      is.list(record) &&
        is.list(saved) &&
        is.list(saved$project_asset %||% NULL)
    ) {
      record$project_asset <- saved$project_asset
    }
    record
  })
}

builder_project_restore_spatial_assets <- function(
  entry,
  root,
  validate = TRUE
) {
  dataset_id <- as.character(entry$id %||% "dataset")
  .builder_project_map_spatial_images(entry, function(record, section, label) {
    asset <- if (is.list(record)) record$project_asset %||% NULL else NULL
    if (is.null(asset)) {
      return(record)
    }
    fail <- function(reason) {
      stop(
        paste0(
          "Spatial image asset for dataset “",
          dataset_id,
          "”, FOV “",
          section,
          "”, image “",
          label,
          "” ",
          reason,
          "."
        ),
        call. = FALSE
      )
    }
    path <- tryCatch(
      builder_project_resolve_path(asset$path %||% "", root, "managed"),
      error = function(error) NULL
    )
    if (
      !.builder_project_text(path) || !file.exists(path) || dir.exists(path)
    ) {
      fail("is missing")
    }
    if (
      isTRUE(validate) &&
        !builder_project_content_fingerprint_matches(
          asset$fingerprint,
          builder_project_file_fingerprint(path, content = TRUE)
        )
    ) {
      fail("failed its integrity check")
    }
    mime <- as.character(asset$mime %||% "image/png")[[1L]]
    uri <- paste0(
      "data:",
      mime,
      ";base64,",
      base64enc::base64encode(path)
    )
    record$source_content_md5 <- asset$fingerprint$md5 %||%
      record$source_content_md5 %||%
      NULL
    record$source_uri <- uri
    record$uri <- uri
    field_order <- as.character(asset$field_order %||% character())
    if (length(field_order)) {
      record <- record[c(
        intersect(field_order, names(record)),
        setdiff(names(record), field_order)
      )]
    }
    record
  })
}

builder_project_checkpoint_entries <- function(entries) {
  lapply(entries, function(entry) {
    if (length(entry$settings$images %||% list())) {
      entry$settings$spatial_image_storage <- "embedded"
    }
    entry
  })
}

builder_project_plan_artifact_reusable <- function(item) {
  !length(setdiff(
    item$private_assets %||% character(),
    c(item$filename, item$sidecars %||% character())
  ))
}

builder_project_checkpoint_budget <- function(entries, root) {
  closure_bytes <- sum(vapply(
    entries,
    function(entry) {
      value <- suppressWarnings(as.double(entry$snapshot$closure_bytes %||% 0))
      if (length(value) == 1L && is.finite(value) && value >= 0) value else 0
    },
    numeric(1)
  ))
  required <- 2 * closure_bytes + 1024^3
  available <- tryCatch(
    .builder_snapshot_available_bytes(root),
    error = function(error) error
  )
  if (inherits(available, "condition")) {
    return(list(
      ok = FALSE,
      required = required,
      available = NA_real_,
      error = conditionMessage(available)
    ))
  }
  list(
    ok = is.finite(available) && available >= required,
    required = required,
    available = available,
    error = if (is.finite(available) && available >= required) {
      NULL
    } else {
      paste0(
        "The project volume does not have enough free space for the checkpoint ",
        "and verified artifact promotion. Free disk space and retry."
      )
    }
  )
}

builder_project_cleanup_checkpoint <- function(path, root) {
  if (!.builder_project_text(path) || !dir.exists(path)) {
    return(FALSE)
  }
  root <- builder_project_normalize_root(root)
  candidate <- .builder_project_lexical_path(path)
  lexical_root <- dirname(dirname(candidate))
  normalized_lexical_root <- tryCatch(
    normalizePath(lexical_root, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  normalized_candidate <- tryCatch(
    normalizePath(candidate, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (
    !identical(normalized_lexical_root, root) ||
      !identical(
        dirname(normalized_candidate %||% ""),
        file.path(root, "checkpoints")
      ) ||
      .builder_project_path_has_link_within(candidate, lexical_root)
  ) {
    return(FALSE)
  }
  unlink(candidate, recursive = TRUE, force = TRUE)
  if (dir.exists(candidate)) {
    return(FALSE)
  }
  if (exists("builder_release_cleanup_control", mode = "function")) {
    return(isTRUE(tryCatch(
      builder_release_cleanup_control(candidate),
      error = function(error) FALSE
    )))
  }
  TRUE
}

builder_project_cleanup_terminal_checkpoint <- function(
  saved,
  status,
  path,
  root
) {
  if (!isTRUE(saved) || !identical(status, "failed")) {
    return(FALSE)
  }
  builder_project_cleanup_checkpoint(path, root)
}

builder_project_configuration_digest <- function(entry) {
  digest_entry <- list(
    settings = entry$settings %||% list(),
    acknowledgements = entry$acknowledgements %||% character(),
    spatial_drafts = entry$spatial_drafts %||% list()
  )
  tables <- digest_entry$settings$tables %||% list()
  if (is.list(tables) && length(tables)) {
    digest_entry$settings$tables <- lapply(tables, function(record) {
      if (is.list(record)) {
        record$source_path <- NULL
        record$table <- NULL
        record$project_asset <- NULL
      }
      record
    })
  }
  digest_entry <- .builder_project_map_spatial_images(
    digest_entry,
    function(record, section, label) {
      if (is.list(record)) {
        source_md5 <- record$source_content_md5 %||%
          record$project_asset$fingerprint$md5 %||%
          NULL
        if (
          !.builder_project_text(source_md5) ||
            !grepl("^[[:xdigit:]]{32}$", source_md5)
        ) {
          source_md5 <- tryCatch(
            {
              parsed <- .builder_project_decode_image_uri(
                record$source_uri %||% record$uri %||% NULL
              )
              unclass(as.character(openssl::md5(parsed$bytes)))
            },
            error = function(error) NULL
          )
        }
        # Digest-only content identity; never serialized as project UI state.
        record$source_content_md5 <- source_md5
        record$source_uri <- NULL
        record$uri <- NULL
        record$project_asset <- NULL
      }
      record
    }
  )
  # `levels` is source-derived and immutable after import. Source fingerprinting,
  # rather than the editable-configuration digest, invalidates it on reload.
  value <- list(
    contract_version = .builder_project_configuration_contract_version,
    settings = digest_entry$settings %||% list(),
    acknowledgements = digest_entry$acknowledgements %||% character(),
    spatial_drafts = digest_entry$spatial_drafts %||% list()
  )
  unclass(as.character(openssl::md5(
    serialize(value, NULL, version = 3L)
  )))
}

builder_project_cached_configuration_digest <- function(
  entry,
  cache,
  digest = builder_project_configuration_digest,
  variant = "live"
) {
  if (
    !is.environment(cache) || !.builder_project_identifier(entry$id %||% NULL)
  ) {
    return(digest(entry))
  }
  key <- paste(
    entry$id,
    as.integer(entry$revision %||% 0L),
    as.character(variant %||% "live"),
    sep = "::"
  )
  cached <- get0(key, envir = cache, inherits = FALSE)
  if (.builder_project_text(cached %||% NULL)) {
    return(cached)
  }
  value <- digest(entry)
  assign(key, value, envir = cache)
  id_prefix <- paste0(entry$id, "::")
  revision_prefix <- paste0(
    entry$id,
    "::",
    as.integer(entry$revision %||% 0L),
    "::"
  )
  keys <- ls(cache)
  stale <- keys[
    startsWith(keys, id_prefix) & !startsWith(keys, revision_prefix)
  ]
  if (length(stale)) {
    rm(list = stale, envir = cache)
  }
  revision_keys <- keys[startsWith(keys, revision_prefix)]
  overflow <- setdiff(revision_keys, key)
  if (length(revision_keys) > 8L && length(overflow)) {
    rm(
      list = head(sort(overflow, method = "radix"), length(revision_keys) - 8L),
      envir = cache
    )
  }
  value
}

builder_project_configuration_cache_clear <- function(cache) {
  if (!is.environment(cache)) {
    return(invisible(FALSE))
  }
  keys <- ls(cache)
  if (length(keys)) {
    rm(list = keys, envir = cache)
  }
  invisible(TRUE)
}

builder_project_configuration_cache_retain_datasets <- function(cache, ids) {
  if (!is.environment(cache)) {
    return(invisible(FALSE))
  }
  keys <- ls(cache)
  cached_ids <- sub("::.*$", "", keys)
  drop <- keys[!cached_ids %in% as.character(ids %||% character())]
  if (length(drop)) {
    rm(list = drop, envir = cache)
  }
  invisible(TRUE)
}

builder_project_dataset_record <- function(
  entry,
  source,
  checked = FALSE,
  artifact = NULL,
  order = 1L,
  payload_entry = entry,
  root,
  configuration_digest = NULL
) {
  profile <- entry$profile %||% list()
  spatial <- entry$dataset_profile$content$spatial %||%
    entry$profile$viewer_content$spatial %||%
    list()
  sections <- spatial$sections %||% spatial$fovs %||% character()
  digest <- configuration_digest %||%
    builder_project_configuration_digest(payload_entry)
  configuration <- utils::modifyList(
    builder_project_write_dataset_config(payload_entry, root),
    list(
      digest = digest,
      checked = isTRUE(checked),
      checked_digest = if (isTRUE(checked)) digest else NULL,
      contract_version = .builder_project_configuration_contract_version
    )
  )
  list(
    id = entry$id,
    name = entry$settings$name %||% entry$id,
    order = as.integer(order),
    format = entry$format %||% NULL,
    source = source,
    inspection = list(
      cells = as.integer(profile$n_cells %||% 0L),
      genes = as.integer(profile$n_genes %||% 0L),
      assays = as.character(names(profile$assay_profiles %||% list())),
      projections = as.character(entry$settings$reductions %||% character()),
      fovs = as.character(names(sections) %||% sections %||% character())
    ),
    configuration = configuration,
    artifact = artifact,
    release = list(included = TRUE)
  )
}

builder_project_restore_entry <- function(
  record,
  root,
  hydrate_spatial_assets = TRUE,
  status = NULL
) {
  trusted_status <- is.list(status) &&
    builder_project_status_snapshot_fresh(status, record, root) &&
    is.list(status$configuration_entry %||% NULL)
  entry <- if (trusted_status) {
    status$configuration_entry
  } else {
    builder_project_read_dataset_config(record, root)
  }
  entry$id <- as.character(record$id)
  entry$source_id <- entry$id
  entry$output_id <- entry$id
  entry$selector_value <- entry$id
  entry$revision <- as.integer(
    entry$revision %||% record$configuration$revision %||% 0L
  )
  entry$settings <- entry$settings %||% list(name = record$name %||% entry$id)
  inspection <- record$inspection %||% list()
  entry$profile <- entry$profile %||%
    list(
      n_cells = as.integer(inspection$cells %||% 0L),
      n_genes = as.integer(inspection$genes %||% 0L),
      assays = as.character(inspection$assays %||% character()),
      reductions = as.character(inspection$projections %||% character())
    )
  entry$dataset_profile <- entry$dataset_profile %||% list()
  entry$levels <- entry$levels %||% list()
  entry$format <- entry$format %||% record$format %||% NULL
  entry$load_state <- "reload_required"
  if (isTRUE(hydrate_spatial_assets)) {
    entry <- builder_project_restore_spatial_assets(
      entry,
      root,
      validate = !trusted_status
    )
  }
  entry <- builder_project_restore_table_assets(entry, root)
  source <- record$source %||% list()
  entry$source_origin <- source$origin %||%
    if (identical(source$kind, "example")) "example" else "upload"
  if (identical(source$kind, "example")) {
    entry$example <- source$example
    entry$path <- NULL
  } else {
    entry$example <- if (identical(entry$source_origin, "example")) {
      source$example %||% NULL
    } else {
      NULL
    }
    entry$path <- builder_project_resolve_path(
      source$path %||% "",
      root,
      source$kind %||% "managed"
    )
  }
  entry
}

builder_project_hydrate_loaded_entry <- function(loaded, record, root) {
  if (!is.list(loaded) || !.builder_project_identifier(loaded$id)) {
    stop("A loaded dataset entry is required.", call. = FALSE)
  }
  if (!is.list(record) || !identical(as.character(record$id), loaded$id)) {
    stop(
      "The saved dataset configuration does not match the loaded entry.",
      call. = FALSE
    )
  }
  status <- builder_project_dataset_status(record, root)
  if (!isTRUE(status$restorable)) {
    stop(
      "The saved dataset inputs changed during Project restore.",
      call. = FALSE
    )
  }
  saved <- builder_project_restore_entry(
    record,
    root,
    status = status
  )
  hydrated <- loaded
  hydrated$settings <- saved$settings
  hydrated$acknowledgements <- saved$acknowledgements %||% NULL
  hydrated$spatial_drafts <- saved$spatial_drafts %||% NULL
  hydrated$revision <- max(
    as.integer(loaded$revision %||% 0L),
    as.integer(saved$revision %||% 0L)
  ) +
    1L
  hydrated$project_hydration <- list(
    schema_version = 1L,
    configuration_digest = as.character(
      record$configuration$digest %||% ""
    )
  )
  hydrated
}

builder_project_entry_hydrated_from <- function(entry, record) {
  is.list(entry) &&
    is.list(record) &&
    is.list(entry$project_hydration) &&
    .builder_project_text(record$configuration$digest %||% NULL) &&
    identical(
      as.character(entry$project_hydration$configuration_digest %||% ""),
      as.character(record$configuration$digest)
    )
}

builder_project_invalidate_entry_hydration <- function(entry) {
  if (is.list(entry)) {
    entry$project_hydration <- NULL
  }
  entry
}

builder_project_hydrate_pending_entries <- function(entries, pending, root) {
  if (!is.list(entries) || !is.list(pending)) {
    stop("Project restore state is invalid.", call. = FALSE)
  }
  keep <- rep(TRUE, length(entries))
  restored <- list()
  failures <- list()
  for (index in seq_along(entries)) {
    entry <- entries[[index]]
    id <- as.character(entry$id %||% "")
    record <- pending[[id]] %||% NULL
    if (
      is.null(record) ||
        !identical(entry$load_state %||% "loaded", "loaded")
    ) {
      next
    }
    hydrated <- tryCatch(
      if (builder_project_entry_hydrated_from(entry, record)) {
        entry
      } else {
        builder_project_hydrate_loaded_entry(entry, record, root)
      },
      error = identity
    )
    pending[[id]] <- NULL
    if (inherits(hydrated, "condition")) {
      keep[[index]] <- FALSE
      failures[[id]] <- list(
        entry = entry,
        record = record,
        message = paste0(
          "The saved project configuration for ",
          id,
          " could not be restored: ",
          conditionMessage(hydrated)
        )
      )
      next
    }
    entries[[index]] <- hydrated
    restored[[id]] <- list(entry = hydrated, record = record)
  }
  list(
    entries = entries[keep],
    pending = pending,
    restored = restored,
    failures = failures
  )
}

builder_project_store_artifact_bundle <- function(
  built,
  sidecars = character(),
  dataset_id,
  root,
  promote = FALSE
) {
  if (
    !.builder_project_identifier(dataset_id) ||
      !.builder_project_text(built) ||
      !file.exists(built) ||
      dir.exists(built)
  ) {
    stop("A completed dataset artifact is required.", call. = FALSE)
  }
  root <- builder_project_normalize_root(root)
  built <- normalizePath(built, winslash = "/", mustWork = TRUE)
  build_root <- normalizePath(dirname(built), winslash = "/", mustWork = TRUE)
  sidecars <- unique(as.character(sidecars %||% character()))
  safe_member <- function(target) {
    normalized_target <- gsub("\\\\", "/", target)
    if (
      !.builder_project_text(target) ||
        grepl("^/", target) ||
        grepl("^[A-Za-z]:[/\\\\]", target) ||
        any(
          strsplit(normalized_target, "/", fixed = TRUE)[[1L]] %in%
            c("", ".", "..")
        ) ||
        identical(normalized_target, basename(built))
    ) {
      stop("An artifact member path is unsafe.", call. = FALSE)
    }
    source <- normalizePath(
      file.path(build_root, target),
      winslash = "/",
      mustWork = TRUE
    )
    if (!startsWith(source, paste0(build_root, "/")) || dir.exists(source)) {
      stop("An artifact member escaped the build folder.", call. = FALSE)
    }
    list(target = normalized_target, source = source)
  }
  members <- lapply(sidecars, safe_member)
  normalized_members <- vapply(members, `[[`, character(1), "target")
  if (anyDuplicated(normalized_members)) {
    stop("Artifact member paths must be unique.", call. = FALSE)
  }
  primary_fingerprint <- builder_project_file_fingerprint(built, content = TRUE)
  member_fingerprints <- lapply(members, function(member) {
    builder_project_file_fingerprint(member$source, content = TRUE)
  })
  identity <- list(
    primary = list(name = basename(built), md5 = primary_fingerprint$md5),
    members = lapply(seq_along(members), function(index) {
      list(
        target = members[[index]]$target,
        md5 = member_fingerprints[[index]]$md5
      )
    })
  )
  bundle_id <- unclass(as.character(openssl::md5(
    serialize(identity, NULL, version = 3L)
  )))
  relative_dir <- paste(
    "artifacts",
    dataset_id,
    "generations",
    bundle_id,
    sep = "/"
  )
  generation_dir <- builder_project_resolve_path(relative_dir, root, "managed")
  generation_parent <- dirname(generation_dir)
  if (
    !dir.exists(generation_parent) &&
      !dir.create(generation_parent, recursive = TRUE, showWarnings = FALSE)
  ) {
    stop("The artifact generation folder could not be created.", call. = FALSE)
  }
  staging <- tempfile(
    pattern = paste0(".", dataset_id, "-artifact-"),
    tmpdir = generation_parent
  )
  promoted <- list()
  generation_committed <- FALSE
  on.exit(
    {
      if (!generation_committed && length(promoted)) {
        for (move in rev(promoted)) {
          if (file.exists(move$staged) && !file.exists(move$source)) {
            dir.create(
              dirname(move$source),
              recursive = TRUE,
              showWarnings = FALSE
            )
            file.rename(move$staged, move$source)
          }
        }
      }
      unlink(staging, recursive = TRUE, force = TRUE)
    },
    add = TRUE
  )
  read_existing_generation <- function() {
    primary_path <- file.path(generation_dir, basename(built))
    stored_members <- lapply(seq_along(members), function(index) {
      member <- members[[index]]
      path <- file.path(generation_dir, member$target)
      if (!file.exists(path) || dir.exists(path)) {
        stop("A stored artifact member is missing.", call. = FALSE)
      }
      list(
        target = member$target,
        path = builder_project_relative_path(path, root),
        fingerprint = builder_project_file_fingerprint(path, content = TRUE)
      )
    })
    if (!file.exists(primary_path) || dir.exists(primary_path)) {
      stop("The stored dataset artifact is missing.", call. = FALSE)
    }
    stored_primary <- builder_project_file_fingerprint(
      primary_path,
      content = TRUE
    )
    if (
      !builder_project_content_fingerprint_matches(
        primary_fingerprint,
        stored_primary
      ) ||
        any(
          !vapply(
            seq_along(stored_members),
            function(index) {
              builder_project_content_fingerprint_matches(
                member_fingerprints[[index]],
                stored_members[[index]]$fingerprint
              )
            },
            logical(1)
          )
        )
    ) {
      stop(
        "The stored artifact generation failed its integrity check.",
        call. = FALSE
      )
    }
    list(
      path = builder_project_relative_path(primary_path, root),
      fingerprint = stored_primary,
      members = stored_members
    )
  }
  if (!dir.exists(generation_dir)) {
    if (!dir.create(staging, showWarnings = FALSE)) {
      stop("The artifact staging folder could not be created.", call. = FALSE)
    }
    primary_target <- file.path(staging, basename(built))
    stage_file <- function(source, target) {
      moved <- isTRUE(promote) && isTRUE(file.rename(source, target))
      if (moved) {
        promoted[[length(promoted) + 1L]] <<- list(
          source = source,
          staged = target
        )
        return(TRUE)
      }
      builder_project_copy_file(source, target)
    }
    if (!stage_file(built, primary_target)) {
      stop("The dataset artifact could not be staged.", call. = FALSE)
    }
    for (index in seq_along(members)) {
      member_target <- file.path(staging, members[[index]]$target)
      if (
        !dir.exists(dirname(member_target)) &&
          !dir.create(
            dirname(member_target),
            recursive = TRUE,
            showWarnings = FALSE
          )
      ) {
        stop("An artifact member folder could not be staged.", call. = FALSE)
      }
      if (!stage_file(members[[index]]$source, member_target)) {
        stop("An artifact member could not be staged.", call. = FALSE)
      }
    }
    staged_paths <- c(
      primary_target,
      vapply(
        members,
        function(member) {
          file.path(staging, member$target)
        },
        character(1)
      )
    )
    expected <- c(
      primary_fingerprint$md5,
      vapply(member_fingerprints, `[[`, character(1), "md5")
    )
    staged_md5 <- unname(as.character(tools::md5sum(staged_paths)))
    if (!identical(staged_md5, expected)) {
      stop(
        "The artifact generation could not be committed safely.",
        call. = FALSE
      )
    }
    staged_fingerprints <- lapply(seq_along(staged_paths), function(index) {
      builder_project_file_fingerprint_with_md5(
        staged_paths[[index]],
        staged_md5[[index]]
      )
    })
    if (any(vapply(staged_fingerprints, is.null, logical(1)))) {
      stop("The staged artifact generation is incomplete.", call. = FALSE)
    }
    stored_primary <- staged_fingerprints[[1L]]
    stored_members <- lapply(seq_along(members), function(index) {
      list(
        target = members[[index]]$target,
        path = paste(relative_dir, members[[index]]$target, sep = "/"),
        fingerprint = staged_fingerprints[[index + 1L]]
      )
    })
    if (!file.rename(staging, generation_dir)) {
      if (dir.exists(generation_dir)) {
        return(read_existing_generation())
      }
      stop(
        "The artifact generation could not be committed safely.",
        call. = FALSE
      )
    }
    generation_committed <- TRUE
    return(list(
      path = paste(relative_dir, basename(built), sep = "/"),
      fingerprint = stored_primary,
      members = stored_members
    ))
  }
  read_existing_generation()
}

builder_project_artifact_available <- function(artifact, root) {
  if (
    !is.list(artifact) ||
      !identical(artifact$status, "ready") ||
      isFALSE(artifact$reusable %||% TRUE)
  ) {
    return(FALSE)
  }
  path <- tryCatch(
    builder_project_resolve_path(artifact$path %||% "", root, "managed"),
    error = function(error) NULL
  )
  if (!.builder_project_text(path) || !file.exists(path) || dir.exists(path)) {
    return(FALSE)
  }
  if (!builder_project_managed_file_matches(artifact$fingerprint, path)) {
    return(FALSE)
  }
  members <- artifact$members %||% list()
  if (!is.list(members)) {
    return(FALSE)
  }
  all(vapply(
    members,
    function(member) {
      member_path <- tryCatch(
        builder_project_resolve_path(member$path %||% "", root, "managed"),
        error = function(error) NULL
      )
      if (
        !.builder_project_text(member_path) ||
          !file.exists(member_path) ||
          dir.exists(member_path)
      ) {
        return(FALSE)
      }
      builder_project_managed_file_matches(member$fingerprint, member_path)
    },
    logical(1)
  ))
}

builder_project_entries_requiring_crb <- function(entries, artifacts, root) {
  Filter(
    function(entry) {
      artifact <- artifacts[[entry$id]] %||% NULL
      !is.list(artifact) ||
        !builder_project_artifact_available(artifact, root) ||
        !identical(
          as.character(artifact$built_from_configuration %||% ""),
          builder_project_configuration_digest(entry)
        )
    },
    entries
  )
}

builder_project_spatial_assets_status <- function(record, root) {
  entry <- tryCatch(
    builder_project_read_dataset_config(record, root),
    error = identity
  )
  if (inherits(entry, "condition")) {
    return(list(ready = FALSE, error = conditionMessage(entry), entry = NULL))
  }
  entry$id <- as.character(record$id %||% entry$id %||% "dataset")
  validated <- tryCatch(
    .builder_project_map_spatial_images(entry, function(image, section, label) {
      asset <- if (is.list(image)) image$project_asset %||% NULL else NULL
      if (is.null(asset)) {
        return(image)
      }
      path <- builder_project_resolve_path(asset$path %||% "", root, "managed")
      if (
        !.builder_project_text(path) || !file.exists(path) || dir.exists(path)
      ) {
        stop(
          paste0(
            "Spatial image asset for dataset “",
            entry$id,
            "”, FOV “",
            section,
            "”, image “",
            label,
            "” is missing."
          ),
          call. = FALSE
        )
      }
      if (!builder_project_managed_file_matches(asset$fingerprint, path)) {
        stop(
          paste0(
            "Spatial image asset for dataset “",
            entry$id,
            "”, FOV “",
            section,
            "”, image “",
            label,
            "” failed its integrity check."
          ),
          call. = FALSE
        )
      }
      image
    }),
    error = identity
  )
  if (inherits(validated, "condition")) {
    return(list(
      ready = FALSE,
      error = conditionMessage(validated),
      entry = NULL
    ))
  }
  list(ready = TRUE, error = NULL, entry = entry)
}

builder_project_record_status_identity <- function(record) {
  if (!is.list(record)) {
    return("")
  }
  descriptor <- record
  descriptor$runtime_restore_status <- NULL
  unclass(as.character(openssl::md5(
    serialize(descriptor, NULL, version = 3L)
  )))
}

builder_project_status_stat_signature <- function(record, root, entry = NULL) {
  references <- list()
  add <- function(path, kind = "managed") {
    if (.builder_project_text(path %||% NULL)) {
      references[[length(references) + 1L]] <<- list(path = path, kind = kind)
    }
  }
  configuration <- record$configuration %||% list()
  add(configuration$path %||% NULL)
  source <- record$source %||% list()
  if (!identical(source$kind %||% "managed", "example")) {
    add(source$path %||% NULL, source$kind %||% "managed")
  }
  artifact <- record$artifact %||% list()
  add(artifact$path %||% NULL)
  for (member in artifact$members %||% list()) {
    add(member$path %||% NULL)
  }
  if (is.list(entry)) {
    .builder_project_map_spatial_images(entry, function(image, section, label) {
      if (is.list(image)) {
        add(image$project_asset$path %||% NULL)
      }
      image
    })
  }
  resolved <- vapply(
    references,
    function(reference) {
      resolved_path <- tryCatch(
        builder_project_resolve_path(reference$path, root, reference$kind),
        error = function(error) NA_character_
      )
      if (.builder_project_text(resolved_path)) {
        as.character(resolved_path)
      } else {
        NA_character_
      }
    },
    character(1)
  )
  paths <- sort(unique(resolved[!is.na(resolved)]), method = "radix")
  stats::setNames(
    lapply(paths, function(path) {
      info <- file.info(path)
      list(
        size = suppressWarnings(as.numeric(info$size[[1L]])),
        mtime = suppressWarnings(as.numeric(info$mtime[[1L]])),
        ctime = suppressWarnings(as.numeric(info$ctime[[1L]]))
      )
    }),
    paths
  )
}

builder_project_status_snapshot_fresh <- function(status, record, root) {
  is.list(status) &&
    identical(
      as.character(status$record_identity %||% ""),
      builder_project_record_status_identity(record)
    ) &&
    is.list(status$stat_signature %||% NULL) &&
    identical(
      status$stat_signature,
      builder_project_status_stat_signature(
        record,
        root,
        status$configuration_entry %||% NULL
      )
    )
}

builder_project_record_configuration_confirmed <- function(record) {
  is.list(record) &&
    is.list(record$configuration) &&
    isTRUE(record$configuration$checked) &&
    identical(
      as.character(
        record$configuration$checked_digest %||%
          record$configuration$digest %||%
          ""
      ),
      as.character(record$configuration$digest %||% "")
    )
}

builder_project_dataset_status <- function(record, root) {
  if (
    is.list(record$runtime_restore_status %||% NULL) &&
      builder_project_status_snapshot_fresh(
        record$runtime_restore_status,
        record,
        root
      )
  ) {
    return(record$runtime_restore_status)
  }
  artifact_ready <- builder_project_artifact_available(record$artifact, root)
  spatial_assets <- builder_project_spatial_assets_status(record, root)
  source <- record$source %||% list()
  source_path <- if (identical(source$kind, "example")) {
    source$example %||% NULL
  } else {
    tryCatch(
      builder_project_resolve_path(
        source$path %||% "",
        root,
        source$kind %||% "managed"
      ),
      error = function(error) NULL
    )
  }
  source_ready <- identical(source$kind, "example") ||
    (.builder_project_text(source_path) && file.exists(source_path))
  recorded_fingerprint <- source$fingerprint %||% NULL
  current_metadata <- if (source_ready && !identical(source$kind, "example")) {
    builder_project_file_fingerprint(source_path, content = FALSE)
  } else {
    NULL
  }
  managed_source_unchanged <- identical(source$kind, "managed") &&
    builder_project_content_addressed_source(source$path, record$id) &&
    builder_project_fingerprint_metadata_matches(
      recorded_fingerprint,
      current_metadata
    )
  current_fingerprint <- if (
    source_ready &&
      !identical(source$kind, "example") &&
      !managed_source_unchanged
  ) {
    builder_project_file_fingerprint(source_path, content = TRUE)
  } else {
    current_metadata
  }
  source_matches <- identical(source$kind, "example") ||
    is.null(recorded_fingerprint) ||
    managed_source_unchanged ||
    builder_project_content_fingerprint_matches(
      recorded_fingerprint,
      current_fingerprint
    )
  checked <- isTRUE(spatial_assets$ready) &&
    builder_project_record_configuration_confirmed(record) &&
    source_matches
  list(
    source_ready = source_ready,
    source_matches = source_matches,
    spatial_assets_ready = isTRUE(spatial_assets$ready),
    spatial_assets_error = spatial_assets$error,
    restorable = source_ready && isTRUE(spatial_assets$ready),
    artifact_ready = artifact_ready,
    record_identity = builder_project_record_status_identity(record),
    configuration_entry = spatial_assets$entry,
    stat_signature = builder_project_status_stat_signature(
      record,
      root,
      spatial_assets$entry
    ),
    checked = checked,
    label = if (!isTRUE(spatial_assets$ready)) {
      "Needs check · spatial image missing"
    } else if (checked && artifact_ready) {
      "Checked · CRB ready"
    } else if (checked) {
      "Checked · source reload required"
    } else if (source_ready && !source_matches) {
      "Needs check · source changed"
    } else if (source_ready) {
      "Needs check · load source"
    } else {
      "Needs check · source missing"
    }
  )
}

builder_project_status_snapshot <- function(manifest, root) {
  records <- manifest$datasets %||% list()
  if (!length(records)) {
    return(list())
  }
  statuses <- lapply(records, function(record) {
    record$runtime_restore_status <- NULL
    builder_project_dataset_status(record, root)
  })
  names(statuses) <- vapply(
    records,
    function(record) {
      as.character(record$id)
    },
    character(1)
  )
  statuses
}

builder_project_validate_external_sources <- function(manifest, roots) {
  if (!is.list(manifest)) {
    stop("A Builder Project manifest is required.", call. = FALSE)
  }
  manifest$datasets <- lapply(manifest$datasets %||% list(), function(record) {
    source <- record$source %||% list()
    if (!identical(source$kind, "external")) {
      return(record)
    }
    label <- as.character(
      record$name %||% source$filename %||% record$id %||% "dataset"
    )
    resolved <- tryCatch(
      builder_server_path_resolve(source$path, type = "file", roots = roots),
      error = identity
    )
    if (inherits(resolved, "condition")) {
      stop(
        "External source for ",
        label,
        " is not allowed: ",
        conditionMessage(resolved),
        call. = FALSE
      )
    }
    record$source$path <- resolved
    record
  })
  manifest
}

builder_project_open_snapshot <- function(path, external_roots = NULL) {
  manifest <- builder_project_read(path)
  if (!is.null(external_roots)) {
    manifest <- builder_project_validate_external_sources(
      manifest,
      roots = external_roots
    )
  }
  root <- dirname(path)
  statuses <- builder_project_status_snapshot(manifest, root)
  manifest$datasets <- lapply(
    manifest$datasets %||% list(),
    function(record) {
      record$runtime_restore_status <- statuses[[record$id]] %||% NULL
      record
    }
  )
  list(
    manifest = manifest,
    root = root,
    path = path
  )
}

builder_project_new_manifest <- function(root, name = basename(root)) {
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  list(
    schema_version = .builder_project_schema_version,
    project = list(
      id = builder_project_id(),
      name = as.character(name),
      revision = 0L,
      created_at = now,
      updated_at = now,
      builder_version = as.character(
        tryCatch(
          utils::packageVersion("CerebroNexus"),
          error = function(error) "development"
        )
      )
    ),
    datasets = list(),
    configuration = builder_project_configuration(),
    pending_build = NULL,
    releases = list(),
    last_ui = list(stage = "upload", selected_dataset = NULL)
  )
}

builder_project_configuration <- function(
  review_options = NULL,
  build_mode = FALSE,
  auth_enabled = FALSE,
  initial_dataset = NULL
) {
  scalar_flag <- function(value) {
    is.logical(value) && length(value) == 1L && !is.na(value)
  }
  if (!scalar_flag(build_mode) || !scalar_flag(auth_enabled)) {
    stop("Builder project preferences are invalid.", call. = FALSE)
  }
  if (!is.null(initial_dataset)) {
    initial_dataset <- as.character(initial_dataset)
    if (
      length(initial_dataset) != 1L ||
        is.na(initial_dataset) ||
        !.builder_project_identifier(initial_dataset)
    ) {
      stop("The Builder starting dataset preference is invalid.", call. = FALSE)
    }
  }
  allowed <- c(
    "welcome_message",
    "initial_page",
    "point_size",
    "variable_to_compare",
    "host",
    "port",
    "max_request_size",
    "display_mode",
    "launch_browser",
    "show_upload_ui"
  )
  saved_options <- if (is.null(review_options)) {
    NULL
  } else {
    if (!is.list(review_options)) {
      stop("Builder Review preferences are invalid.", call. = FALSE)
    }
    candidate <- unclass(review_options)[
      intersect(allowed, names(review_options))
    ]
    required <- setdiff(allowed, names(candidate))
    if (length(required)) {
      stop("Builder Review preferences are incomplete.", call. = FALSE)
    }
    valid_text <- function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(value)
    }
    valid_number <- function(value, lower, upper = Inf, whole = FALSE) {
      is.numeric(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        is.finite(value) &&
        value >= lower &&
        value <= upper &&
        (!whole || value == floor(value))
    }
    valid_flag <- function(value) {
      is.logical(value) && length(value) == 1L && !is.na(value)
    }
    if (
      !valid_text(candidate$welcome_message) ||
        !valid_text(candidate$initial_page) ||
        !valid_number(candidate$point_size, 0, 20) ||
        !valid_flag(candidate$variable_to_compare) ||
        !valid_text(candidate$host) ||
        !valid_number(candidate$port, 1, 65535, whole = TRUE) ||
        !valid_number(candidate$max_request_size, .Machine$double.eps) ||
        !valid_text(candidate$display_mode) ||
        !candidate$display_mode %in% c("auto", "normal", "showcase") ||
        !valid_flag(candidate$launch_browser) ||
        !valid_flag(candidate$show_upload_ui)
    ) {
      stop("Builder Review preferences are invalid.", call. = FALSE)
    }
    candidate$point_size <- as.double(candidate$point_size)
    candidate$port <- as.integer(candidate$port)
    candidate$max_request_size <- as.double(candidate$max_request_size)
    candidate
  }
  list(
    review_options = saved_options,
    build_mode = isTRUE(build_mode),
    auth_enabled = isTRUE(auth_enabled),
    initial_dataset = initial_dataset
  )
}

builder_project_migrate_manifest <- function(manifest, root) {
  version <- .builder_project_integer(manifest$schema_version)
  if (!version %in% .builder_project_supported_schema_versions) {
    stop("This is not a supported Builder project.", call. = FALSE)
  }
  if (version < .builder_project_schema_version) {
    manifest$migrated_from_schema <- version
    if (identical(version, 1L)) {
      manifest$configuration <- builder_project_configuration()
    }
    manifest$datasets <- lapply(manifest$datasets, function(record) {
      payload <- record$configuration$payload %||% NULL
      if (!.builder_project_text(payload)) {
        return(record)
      }
      entry <- tryCatch(jsonlite::unserializeJSON(payload), error = identity)
      if (inherits(entry, "condition")) {
        return(record)
      }
      entry$id <- as.character(record$id %||% entry$id %||% "")
      old_digest <- as.character(record$configuration$digest %||% "")
      new_digest <- tryCatch(
        builder_project_configuration_digest(entry),
        error = function(error) NULL
      )
      if (!.builder_project_text(new_digest)) {
        return(record)
      }
      config_entry <- builder_project_configuration_entry(entry)
      config_entry <- builder_project_stage_spatial_assets(config_entry, root)
      descriptor <- builder_project_write_dataset_config(config_entry, root)
      record$configuration <- utils::modifyList(
        descriptor,
        list(
          digest = new_digest,
          checked = isTRUE(record$configuration$checked),
          checked_digest = if (isTRUE(record$configuration$checked)) {
            new_digest
          } else {
            NULL
          },
          contract_version = .builder_project_configuration_contract_version
        )
      )
      record$cache <- NULL
      if (
        is.list(record$artifact) &&
          identical(
            as.character(
              record$artifact$built_from_configuration %||% ""
            ),
            old_digest
          )
      ) {
        record$artifact$built_from_configuration <- new_digest
      }
      record
    })
    manifest$schema_version <- .builder_project_schema_version
  }
  configuration <- manifest$configuration %||% list()
  manifest$configuration <- builder_project_configuration(
    review_options = configuration$review_options %||% NULL,
    build_mode = configuration$build_mode %||% FALSE,
    auth_enabled = configuration$auth_enabled %||% FALSE,
    initial_dataset = configuration$initial_dataset %||% NULL
  )
  manifest$datasets <- lapply(manifest$datasets %||% list(), function(record) {
    record$cache <- NULL
    record$runtime_restore_status <- NULL
    record
  })
  last_ui <- manifest$last_ui %||% list()
  stage <- as.character(last_ui$stage %||% "configure")
  if (
    length(stage) != 1L ||
      is.na(stage) ||
      !stage %in% c("upload", "configure", "review", "build")
  ) {
    stage <- "configure"
  }
  selected <- last_ui$selected_dataset %||% NULL
  if (!is.null(selected)) {
    selected <- as.character(selected)
    if (length(selected) != 1L || is.na(selected) || !nzchar(selected)) {
      selected <- NULL
    }
  }
  manifest$last_ui <- list(
    stage = stage,
    selected_dataset = selected,
    spatial = builder_project_last_ui_spatial(
      last_ui$spatial %||% NULL,
      selected_dataset = selected
    )
  )
  manifest
}

builder_project_last_ui_spatial <- function(
  spatial,
  selected_dataset = NULL
) {
  if (is.null(spatial)) {
    return(NULL)
  }
  scalar_text <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }
  if (
    !is.list(spatial) ||
      !scalar_text(spatial$dataset) ||
      !scalar_text(spatial$section) ||
      (!is.null(spatial$image) && !scalar_text(spatial$image)) ||
      (!is.null(selected_dataset) &&
        !identical(spatial$dataset, selected_dataset))
  ) {
    return(NULL)
  }
  list(
    dataset = spatial$dataset,
    section = spatial$section,
    image = spatial$image %||% NULL
  )
}

builder_project_last_ui_target <- function(
  last_ui,
  available_ids
) {
  available_ids <- unique(as.character(available_ids %||% character()))
  saved_dataset <- if (is.list(last_ui)) last_ui$selected_dataset else NULL
  selected_dataset <- if (
    length(available_ids) &&
      is.character(saved_dataset) &&
      length(saved_dataset) == 1L &&
      !is.na(saved_dataset) &&
      saved_dataset %in% available_ids
  ) {
    saved_dataset
  } else if (length(available_ids)) {
    available_ids[[1L]]
  } else {
    NULL
  }
  list(
    selected_dataset = selected_dataset,
    stage = if (length(available_ids)) "configure" else "upload",
    spatial = builder_project_last_ui_spatial(
      if (is.list(last_ui)) last_ui$spatial %||% NULL else NULL,
      selected_dataset = selected_dataset
    )
  )
}

builder_project_read <- function(path) {
  if (!.builder_project_text(path) || !file.exists(path) || dir.exists(path)) {
    stop("Choose a Builder project JSON file.", call. = FALSE)
  }
  input_bytes <- suppressWarnings(as.double(file.info(path)$size[[1L]]))
  if (
    !is.finite(input_bytes) ||
      input_bytes < 1 ||
      input_bytes > .builder_project_manifest_max_bytes
  ) {
    stop("The Builder project exceeds its input size limit.", call. = FALSE)
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  payload <- readBin(
    connection,
    what = "raw",
    n = as.integer(.builder_project_manifest_max_bytes + 1L)
  )
  if (length(payload) > .builder_project_manifest_max_bytes) {
    stop("The Builder project exceeds its input size limit.", call. = FALSE)
  }
  manifest <- jsonlite::fromJSON(
    rawToChar(payload),
    simplifyVector = FALSE
  )
  if (
    !is.list(manifest) ||
      !.builder_project_integer(manifest$schema_version) %in%
        .builder_project_supported_schema_versions ||
      !is.list(manifest$project) ||
      !.builder_project_identifier(manifest$project$id) ||
      !is.list(manifest$datasets)
  ) {
    stop("This is not a supported Builder project.", call. = FALSE)
  }
  dataset_ids <- vapply(
    manifest$datasets,
    function(record) {
      if (!is.list(record) || !.builder_project_identifier(record$id)) {
        return(NA_character_)
      }
      as.character(record$id)
    },
    character(1)
  )
  if (anyNA(dataset_ids) || anyDuplicated(dataset_ids)) {
    stop(
      "The Builder project contains unsafe or duplicate dataset ids.",
      call. = FALSE
    )
  }
  manifest$project$revision <- .builder_project_integer(
    manifest$project$revision
  )
  builder_project_migrate_manifest(manifest, root = dirname(path))
}

builder_project_write <- function(manifest, root, expected_revision = NULL) {
  root <- builder_project_normalize_root(root)
  manifest$datasets <- lapply(manifest$datasets %||% list(), function(record) {
    record$runtime_restore_status <- NULL
    record
  })
  target <- builder_project_manifest_path(root)
  write_lock <- builder_project_acquire_manifest_lock(root)
  on.exit(builder_project_release_manifest_lock(write_lock), add = TRUE)
  disk <- if (file.exists(target)) builder_project_read(target) else NULL
  disk_revision <- if (is.null(disk)) 0L else disk$project$revision
  if (
    !is.null(expected_revision) &&
      !identical(as.integer(expected_revision), as.integer(disk_revision))
  ) {
    stop(
      "This project was updated by another Builder window. Reopen it before saving.",
      call. = FALSE
    )
  }
  if (!is.list(manifest$project)) {
    stop("The Builder project header is missing.", call. = FALSE)
  }
  inline_payloads <- vapply(
    manifest$datasets %||% list(),
    function(record) {
      configuration <- record$configuration %||% list()
      .builder_project_text(configuration$payload %||% NULL) ||
        .builder_project_text(configuration$legacy_payload %||% NULL)
    },
    logical(1)
  )
  if (any(inline_payloads)) {
    stop(
      "Schema v3 cannot write inline dataset payloads.",
      call. = FALSE
    )
  }
  manifest$schema_version <- .builder_project_schema_version
  manifest$migrated_from_schema <- NULL
  manifest$configuration <- builder_project_configuration(
    review_options = manifest$configuration$review_options %||% NULL,
    build_mode = manifest$configuration$build_mode %||% FALSE,
    auth_enabled = manifest$configuration$auth_enabled %||% FALSE,
    initial_dataset = manifest$configuration$initial_dataset %||% NULL
  )
  manifest$project$revision <- as.integer(disk_revision) + 1L
  manifest$project$updated_at <- format(
    Sys.time(),
    "%Y-%m-%dT%H:%M:%OS3Z",
    tz = "UTC"
  )
  temporary <- tempfile("builder-project-", tmpdir = root, fileext = ".json")
  backup <- paste0(target, ".bak")
  on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
  jsonlite::write_json(
    manifest,
    temporary,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    pretty = TRUE,
    digits = NA
  )
  manifest_bytes <- suppressWarnings(as.double(file.info(temporary)$size[[1L]]))
  if (
    !is.finite(manifest_bytes) ||
      manifest_bytes > .builder_project_manifest_max_bytes
  ) {
    stop(
      "The Project manifest exceeded the schema v3 size limit.",
      call. = FALSE
    )
  }
  if (file.exists(target)) {
    if (file.exists(backup)) {
      unlink(backup)
    }
    if (!file.rename(target, backup)) {
      stop(
        "The previous project manifest could not be backed up.",
        call. = FALSE
      )
    }
  }
  if (!file.rename(temporary, target)) {
    if (file.exists(backup)) {
      file.rename(backup, target)
    }
    stop("The project manifest could not be committed.", call. = FALSE)
  }
  list(manifest = manifest, path = target)
}

builder_project_artifact_entry <- function(
  entry,
  artifact,
  root,
  status = NULL,
  record = NULL
) {
  trusted_status <- is.list(status) &&
    is.list(record) &&
    builder_project_status_snapshot_fresh(status, record, root)
  artifact_ready <- if (trusted_status) {
    isTRUE(status$artifact_ready) && identical(artifact, record$artifact)
  } else {
    builder_project_artifact_available(artifact, root)
  }
  if (!artifact_ready) {
    stop("The saved CRB is no longer available.", call. = FALSE)
  }
  entry$load_state <- "artifact_ready"
  entry$snapshot <- NULL
  if (.builder_project_text(artifact$plan_payload %||% "")) {
    artifact$plan_item <- jsonlite::unserializeJSON(artifact$plan_payload)
  }
  artifact$resolved_path <- builder_project_resolve_path(
    artifact$path,
    root,
    "managed"
  )
  if (length(artifact$members %||% list())) {
    artifact$members <- lapply(artifact$members, function(member) {
      member$resolved_path <- builder_project_resolve_path(
        member$path,
        root,
        "managed"
      )
      member
    })
  }
  entry$project_artifact <- artifact
  entry
}

builder_project_prepare_open_selection <- function(manifest, root, actions) {
  records <- manifest$datasets %||% list()
  ids <- vapply(
    records,
    function(record) {
      as.character(record$id)
    },
    character(1)
  )
  names(records) <- ids
  reusable_entries <- list()
  pending_entries <- list()
  artifacts <- list()
  marks <- character()
  skipped_ids <- character()
  for (record in records) {
    action <- actions[[record$id]] %||% "skip"
    status <- builder_project_dataset_status(record, root)
    if (identical(action, "reuse") && isTRUE(status$artifact_ready)) {
      entry <- builder_project_restore_entry(
        record,
        root,
        hydrate_spatial_assets = FALSE,
        status = status
      )
      entry <- builder_project_artifact_entry(
        entry,
        record$artifact,
        root,
        status = status,
        record = record
      )
      reusable_entries[[length(reusable_entries) + 1L]] <- entry
      artifacts[[record$id]] <- record$artifact
      restored_mark <- builder_project_restored_check_identity(
        record,
        entry,
        status,
        root
      )
      if (!is.null(restored_mark)) {
        marks[[record$id]] <- restored_mark
      }
    } else if (identical(action, "resume") && isTRUE(status$restorable)) {
      record$runtime_restore_status <- status
      pending_entries[[record$id]] <- record
    } else {
      skipped_ids <- c(skipped_ids, record$id)
    }
  }
  list(
    reusable_entries = reusable_entries,
    pending_entries = pending_entries,
    artifacts = artifacts,
    marks = marks,
    skipped_ids = skipped_ids
  )
}

builder_project_check_identity <- function(
  entry,
  identity_cache = NULL,
  variant = "live"
) {
  if (
    is.list(entry) &&
      identical(entry$load_state %||% "loaded", "artifact_ready") &&
      is.list(entry$project_artifact)
  ) {
    value <- entry$project_artifact$fingerprint$md5 %||%
      entry$project_artifact$path %||%
      entry$id
    return(paste0("artifact:", value))
  }
  paste0(
    "configuration:",
    builder_project_cached_configuration_digest(
      entry,
      identity_cache,
      variant = variant
    )
  )
}

builder_project_effective_check_identity <- function(
  entry,
  coordinate_drafts = list(),
  identity_cache = NULL
) {
  records <- if (
    is.list(entry) &&
      .builder_project_text(entry$id %||% "") &&
      is.list(coordinate_drafts)
  ) {
    coordinate_drafts[[entry$id]] %||% list()
  } else {
    list()
  }
  if (length(records)) {
    variant <- unclass(as.character(openssl::md5(
      serialize(records, NULL, version = 3L)
    )))
    snapshot_identity <- tryCatch(
      .builder_worker_identity(entry$snapshot),
      error = function(error) NULL
    )
    applied <- if (is.null(snapshot_identity)) {
      NULL
    } else {
      tryCatch(
        builder_coordinate_drafts_apply_entry(
          entry,
          records,
          snapshot_identity = snapshot_identity
        ),
        error = function(error) NULL
      )
    }
    if (!is.list(applied) || !is.list(applied$entry)) {
      return(NA_character_)
    }
    entry <- applied$entry
  } else {
    variant <- "live"
  }
  builder_project_check_identity(entry, identity_cache, variant)
}

builder_project_checked_ids <- function(
  entries,
  marks,
  coordinate_drafts = list(),
  identity_cache = NULL
) {
  ids <- vapply(entries, `[[`, character(1), "id")
  identities <- vapply(
    entries,
    builder_project_effective_check_identity,
    character(1),
    coordinate_drafts = coordinate_drafts,
    identity_cache = identity_cache
  )
  matched <- !is.na(identities) & ids %in% names(marks)
  matched[matched] <- unname(marks[ids[matched]]) == identities[matched]
  ids[matched]
}

builder_project_restored_check_identity <- function(
  record,
  entry,
  status,
  root
) {
  if (
    !is.list(record) ||
      !is.list(entry) ||
      !is.list(status) ||
      !isTRUE(status$checked) ||
      !isTRUE(status$source_matches) ||
      !builder_project_status_snapshot_fresh(status, record, root)
  ) {
    return(NULL)
  }
  builder_project_check_identity(entry)
}
