loading_path <- builder_profile_inst_path("builder", "loading.R")
if (file.exists(loading_path)) {
  sys.source(loading_path, envir = globalenv())
}

builder_loading_call <- function(name, ...) {
  fun <- get0(name, mode = "function", inherits = TRUE)
  if (is.null(fun)) {
    return(NULL)
  }
  fun(...)
}

test_that("new imports enter the typed queue without loading data", {
  source <- list(
    kind = "file",
    staged_path = "/private/session/upload.rds",
    fingerprint = "20:fixture",
    display_name = "upload.rds"
  )
  entry <- builder_loading_call(
    "builder_import_entry",
    id = "ds1",
    label = "upload",
    source = source,
    filename = "upload.rds",
    file_type = "RDS",
    size = 20
  )

  expect_s3_class(entry, "builder_import_entry")
  expect_identical(entry$load_state, "queued")
  expect_identical(entry$progress_label, "Waiting to load")
  expect_identical(entry$generation, 1L)
  expect_null(entry$error)
  expect_null(entry$profile)
  expect_null(entry$settings)
})

test_that("ready import target follows watched and current dataset state", {
  expect_identical(
    names(formals(builder_import_ready_target)),
    c("watched", "current_id", "loaded_id")
  )
  expect_identical(
    builder_import_ready_target(
      watched = TRUE,
      current_id = "ds1",
      loaded_id = "ds2"
    ),
    "ds2"
  )
  expect_identical(
    builder_import_ready_target(
      watched = FALSE,
      current_id = "ds1",
      loaded_id = "ds2"
    ),
    "ds1"
  )
  expect_identical(
    builder_import_ready_target(
      watched = FALSE,
      current_id = NULL,
      loaded_id = "ds2"
    ),
    "ds2"
  )
})

test_that("new imports do not replace an existing dataset or import focus", {
  expect_identical(builder_import_auto_focus(NULL, NULL, "ds-new"), "ds-new")
  expect_identical(
    builder_import_auto_focus("ds-ready", NULL, "ds-new"),
    NULL
  )
  expect_identical(
    builder_import_auto_focus(NULL, "ds-watched", "ds-new"),
    "ds-watched"
  )
})

test_that("public import errors scrub mounted and network paths", {
  mounted <- builder_import_public_error(
    "Failed to open /Volumes/Lab/patient.rds"
  )
  network <- builder_import_public_error(
    "Failed to open \\\\server\\share\\patient.rds"
  )

  expect_false(grepl("/Volumes", mounted, fixed = TRUE))
  expect_false(grepl("server", network, fixed = TRUE))
  expect_match(mounted, "selected file", fixed = TRUE)
  expect_match(network, "selected file", fixed = TRUE)
})

test_that("imports follow the bounded loading state machine", {
  entry <- builder_loading_call(
    "builder_import_entry",
    "ds1",
    "PBMC",
    list(kind = "example", example = "all_content")
  )
  queue <- builder_loading_call("builder_import_queue", max_active = 1L)
  queue <- builder_loading_call("builder_import_add", queue, entry)

  expect_s3_class(queue, "builder_import_queue")
  expect_identical(builder_import_find(queue, "ds1")$load_state, "queued")

  for (state in c("reading", "inspecting", "validating", "preparing")) {
    queue <- builder_import_transition(queue, "ds1", state, generation = 1L)
    expect_identical(builder_import_find(queue, "ds1")$load_state, state)
  }
  queue <- builder_import_transition(queue, "ds1", "ready", generation = 1L)
  expect_identical(builder_import_find(queue, "ds1")$load_state, "ready")
  expect_length(builder_import_active_ids(queue), 0L)
})

test_that("invalid transitions fail and stale completions are ignored", {
  queue <- builder_import_queue()
  queue <- builder_import_add(
    queue,
    builder_import_entry(
      "ds1",
      "PBMC",
      list(kind = "example", example = "all_content")
    )
  )

  expect_error(
    builder_import_transition(queue, "ds1", "ready", generation = 1L),
    class = "builder_import_state_error"
  )

  queued <- builder_import_transition(
    queue,
    "ds1",
    "reading",
    generation = 99L
  )
  expect_identical(queued, queue)
  expect_identical(builder_import_find(queued, "ds1")$load_state, "queued")
})

test_that("error retry uses a new generation and cannot be overwritten", {
  queue <- builder_import_queue()
  queue <- builder_import_add(
    queue,
    builder_import_entry(
      "ds1",
      "PBMC",
      list(kind = "example", example = "all_content")
    )
  )
  queue <- builder_import_transition(queue, "ds1", "reading", 1L)
  queue <- builder_import_transition(
    queue,
    "ds1",
    "error",
    1L,
    error = "/private/session/object.rds failed with a stack trace"
  )

  failed <- builder_import_find(queue, "ds1")
  expect_identical(failed$load_state, "error")
  expect_identical(failed$progress_label, "Could not load dataset")
  expect_false(grepl("/private/session", failed$error, fixed = TRUE))

  retried <- builder_import_retry(queue, "ds1")
  expect_identical(builder_import_find(retried, "ds1")$generation, 2L)
  expect_identical(builder_import_find(retried, "ds1")$load_state, "queued")

  stale <- builder_import_transition(
    retried,
    "ds1",
    "ready",
    generation = 1L
  )
  expect_identical(stale, retried)
})

test_that("failed imports freeze their elapsed time and retries reset it", {
  queue <- builder_import_queue()
  queue <- builder_import_add(
    queue,
    builder_import_entry(
      "ds1",
      "PBMC",
      list(kind = "example", example = "all_content")
    )
  )
  queue <- builder_import_transition(queue, "ds1", "reading", 1L)
  queue <- builder_import_transition(
    queue,
    "ds1",
    "error",
    1L,
    error = "unreadable object"
  )

  failed <- builder_import_find(queue, "ds1")
  expect_identical(failed$load_state, "error")
  expect_true(is.finite(failed$import_elapsed_ms))
  expect_gte(failed$import_elapsed_ms, 0)

  retried <- builder_import_retry(queue, "ds1")
  expect_null(builder_import_find(retried, "ds1")$import_elapsed_ms)
  expect_true(is.finite(builder_import_find(retried, "ds1")$started_at_ms))
})

test_that("the queue enforces one active importer and skips removed work", {
  queue <- builder_import_queue(max_active = 1L)
  for (index in seq_len(10L)) {
    queue <- builder_import_add(
      queue,
      builder_import_entry(
        paste0("ds", index),
        paste("Dataset", index),
        list(kind = "file", staged_path = paste0("/private/", index))
      )
    )
  }

  queue <- builder_import_transition(queue, "ds1", "reading", 1L)
  expect_identical(builder_import_active_ids(queue), "ds1")
  expect_error(
    builder_import_transition(queue, "ds2", "reading", 1L),
    class = "builder_import_state_error"
  )

  queue <- builder_import_remove(queue, "ds3")
  expect_identical(names(queue$entries), paste0("ds", c(1L, 2L, 4L:10L)))
})

test_that("import focus follows the first non-terminal entry in FIFO order", {
  queue <- builder_import_queue(max_active = 1L)
  for (id in paste0("ds", 1:4)) {
    queue <- builder_import_add(
      queue,
      builder_import_entry(
        id,
        id,
        list(kind = "example", example = id)
      )
    )
  }
  queue <- builder_import_transition(queue, "ds1", "reading", 1L)
  queue <- builder_import_transition(queue, "ds1", "preparing", 1L)
  queue <- builder_import_transition(queue, "ds1", "ready", 1L)
  queue <- builder_import_transition(queue, "ds2", "reading", 1L)
  queue <- builder_import_transition(
    queue,
    "ds2",
    "error",
    1L,
    error = "fixture failure"
  )
  queue <- builder_import_transition(queue, "ds3", "cancelled", 1L)

  expect_identical(builder_import_focus_id(queue), "ds4")

  queue <- builder_import_transition(queue, "ds4", "reading", 1L)
  expect_identical(builder_import_focus_id(queue), "ds4")
  expect_lte(length(builder_import_active_ids(queue)), 1L)
})

test_that("import focus is empty when every entry is terminal", {
  queue <- builder_import_queue()
  queue <- builder_import_add(
    queue,
    builder_import_entry(
      "ds1",
      "Ready",
      list(kind = "example", example = "ready")
    )
  )
  queue <- builder_import_transition(queue, "ds1", "reading", 1L)
  queue <- builder_import_transition(queue, "ds1", "preparing", 1L)
  queue <- builder_import_transition(queue, "ds1", "ready", 1L)

  expect_null(builder_import_focus_id(queue))
})
