loading_path <- builder_profile_inst_path("builder", "loading.R")
rail_path <- builder_profile_inst_path("builder", "ui", "dataset_rail.R")
if (file.exists(loading_path)) {
  sys.source(loading_path, envir = globalenv())
}
if (file.exists(rail_path)) {
  sys.source(rail_path, envir = globalenv())
}

test_that("examples can be queued repeatedly while files remain deduplicated", {
  first <- builder_source_reserve(list(), character(), "example", "all_content")
  second <- builder_source_reserve(
    list(),
    first$pending,
    "example",
    "all_content"
  )

  expect_true(first$ok)
  expect_true(second$ok)
  expect_length(second$pending, 0L)

  path <- tempfile(fileext = ".rds")
  file_first <- builder_source_reserve(list(), character(), "file", path)
  file_second <- builder_source_reserve(
    list(),
    file_first$pending,
    "file",
    path
  )
  expect_true(file_first$ok)
  expect_false(file_second$ok)
})

test_that("the dataset rail keeps both add-data routes available", {
  app <- paste(
    readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  js <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(app, "builder-rail-add-browser", fixed = TRUE)
  expect_match(app, "builder-rail-add-local", fixed = TRUE)
  expect_match(js, "openDatasetPicker();", fixed = TRUE)
  expect_match(js, 'send("choose_local_datasets", Date.now())', fixed = TRUE)
})

test_that("import rail patches preserve unchanged sibling rows", {
  first <- builder_import_entry(
    "ds1",
    "Patient one",
    list(kind = "file", staged_path = "/private/ds1.rds"),
    filename = "ds1.rds",
    file_type = "RDS",
    size = 2048
  )
  second <- builder_import_entry(
    "ds2",
    "Patient two",
    list(kind = "file", staged_path = "/private/ds2.rds"),
    filename = "ds2.rds",
    file_type = "RDS",
    size = 4096
  )
  entries <- list(ds1 = first, ds2 = second)
  before <- builder_import_rail_patch(entries, current = "ds1", offset = 3L)

  entries$ds1$load_state <- "reading"
  entries$ds1$progress_label <- "Reading Seurat object…"
  after <- builder_import_rail_patch(entries, current = "ds1", offset = 3L)

  expect_null(names(before$rows))
  expect_identical(
    unname(vapply(before$rows, `[[`, character(1), "id")),
    c("ds1", "ds2")
  )
  expect_false(identical(
    before$rows[[1L]]$fingerprint,
    after$rows[[1L]]$fingerprint
  ))
  expect_identical(before$rows[[2L]]$fingerprint, after$rows[[2L]]$fingerprint)
  expect_match(after$rows[[1L]]$html, 'data-import-id="ds1"', fixed = TRUE)
  expect_match(after$rows[[1L]]$html, 'data-import-fingerprint="', fixed = TRUE)
  expect_match(after$rows[[1L]]$html, 'class="ds-idx">4<', fixed = TRUE)
  expect_match(after$rows[[2L]]$html, 'class="ds-idx">5<', fixed = TRUE)
})

test_that("loading rail rows expose safe status and real actions", {
  entry <- builder_import_entry(
    "ds1",
    "patient-one",
    list(
      kind = "file",
      staged_path = "/private/session/upload-123/object.rds"
    ),
    filename = "patient-one.rds",
    file_type = "RDS",
    size = 2048
  )
  html <- builder_import_rail_patch(
    list(ds1 = entry),
    current = "ds1"
  )$rows[[1L]]$html

  expect_match(html, "patient-one", fixed = TRUE)
  expect_match(html, "Waiting to load", fixed = TRUE)
  expect_match(html, "builder-pick-import", fixed = TRUE)
  expect_match(html, "builder-remove-import", fixed = TRUE)
  expect_match(html, "ds ds--import is-active is-importing", fixed = TRUE)
  expect_match(html, 'data-load-state="queued"', fixed = TRUE)
  expect_match(html, 'aria-current="true"', fixed = TRUE)
  expect_match(
    html,
    'aria-label="Remove queued import patient-one"',
    fixed = TRUE
  )
  expect_match(html, "Remove from queue", fixed = TRUE)
  expect_match(html, "ds-state-dot", fixed = TRUE)
  expect_match(html, 'data-started-at-ms="', fixed = TRUE)
  expect_false(grepl("data-elapsed-ms", html, fixed = TRUE))
  expect_false(grepl("/private/session", html, fixed = TRUE))
})

test_that("active imports offer the established server cancellation action", {
  entry <- builder_import_entry(
    "ds1",
    "patient-one",
    list(kind = "file", staged_path = "/private/session/object.rds")
  )
  queue <- builder_import_add(builder_import_queue(), entry)
  queue <- builder_import_transition(queue, "ds1", "reading", 1L)

  rail_html <- builder_import_rail_patch(
    queue$entries,
    current = "ds1"
  )$rows[[1L]]$html
  expect_match(rail_html, "builder-remove-import", fixed = TRUE)
  expect_match(rail_html, "Cancel active import patient-one", fixed = TRUE)
  expect_match(rail_html, ">Cancel<", fixed = TRUE)
  expect_match(
    rail_html,
    paste(
      "ds-del btn btn-remove-soft builder-cancel-import",
      "builder-remove-import"
    ),
    fixed = TRUE
  )
  expect_false(grepl("Remove from queue", rail_html, fixed = TRUE))
})

test_that("error rows offer Retry and Remove without internal details", {
  entry <- builder_import_entry(
    "ds1",
    "broken",
    list(kind = "file", staged_path = "/private/session/broken.rds")
  )
  queue <- builder_import_add(builder_import_queue(), entry)
  queue <- builder_import_transition(queue, "ds1", "reading", 1L)
  queue <- builder_import_transition(
    queue,
    "ds1",
    "error",
    1L,
    error = "/private/session/broken.rds: invalid object"
  )
  html <- builder_import_rail_patch(
    queue$entries,
    current = "ds1"
  )$rows[[1L]]$html

  expect_match(html, "Could not load dataset", fixed = TRUE)
  expect_match(html, "builder-retry-import", fixed = TRUE)
  expect_match(html, "builder-remove-import", fixed = TRUE)
  expect_match(html, 'data-load-state="error"', fixed = TRUE)
  expect_match(html, "is-error", fixed = TRUE)
  expect_match(html, 'aria-label="Remove failed import broken"', fixed = TRUE)
  expect_match(html, 'data-elapsed-ms="', fixed = TRUE)
  expect_false(grepl("data-started-at-ms", html, fixed = TRUE))
  expect_false(grepl("/private/session", html, fixed = TRUE))
  expect_false(grepl("stack", html, ignore.case = TRUE))
})
