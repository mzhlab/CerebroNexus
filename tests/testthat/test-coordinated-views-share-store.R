# test-coordinated-views-share-store.R — persistent linked-view share records.

inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE),
  normalizePath(system.file(package = "CerebroNexus"), mustWork = FALSE)
)
share_inst <- inst_candidates[file.exists(file.path(
  inst_candidates,
  "viewer"
))][1]
share_file <- if (is.na(share_inst)) {
  ""
} else {
  file.path(
    share_inst,
    "viewer/coordinated_views/share_store.R"
  )
}
share_env <- new.env(parent = baseenv())
if (nzchar(share_file) && file.exists(share_file)) {
  sys.source(share_file, envir = share_env)
}

test_that("a share record is opaque and expires after ninety days", {
  expect_true(exists(
    "cv_share_store_open",
    envir = share_env,
    inherits = FALSE
  ))
  store <- share_env$cv_share_store_open(tempfile(fileext = ".sqlite"))
  withr::defer(DBI::dbDisconnect(store$con))
  created_at <- as.POSIXct("2026-08-20 12:00:00", tz = "UTC")
  created <- share_env$cv_share_store_create(
    store,
    '{"schema":"test"}',
    "fingerprint-a",
    now = created_at
  )
  expect_match(created$token, "^[A-Za-z0-9_-]{43}$")
  expect_equal(
    share_env$cv_share_store_fetch(
      store,
      created$token,
      "fingerprint-a",
      now = created_at
    )$json,
    '{"schema":"test"}'
  )
  expect_error(
    share_env$cv_share_store_fetch(
      store,
      created$token,
      "fingerprint-b",
      now = created_at
    ),
    class = "cv_share_error"
  )
  expired <- share_env$cv_share_store_create(
    store,
    '{"schema":"test"}',
    "fingerprint-a",
    now = created_at
  )
  expect_error(
    share_env$cv_share_store_fetch(
      store,
      expired$token,
      "fingerprint-a",
      now = created_at + 90 * 24 * 60 * 60 + 1
    ),
    class = "cv_share_error"
  )
})

test_that("a browser may supply a cryptographically generated public token", {
  store <- share_env$cv_share_store_open(tempfile(fileext = ".sqlite"))
  withr::defer(DBI::dbDisconnect(store$con))
  token <- paste(rep("A", 43L), collapse = "")
  created <- share_env$cv_share_store_create(
    store,
    '{"schema":"test"}',
    "fingerprint-a",
    token = token
  )

  expect_identical(created$token, token)
  expect_error(
    share_env$cv_share_store_create(
      store,
      '{"schema":"test"}',
      "fingerprint-a",
      token = token
    ),
    class = "cv_share_error"
  )
})

test_that("share creation records administrator audit fields", {
  store <- share_env$cv_share_store_open(tempfile(fileext = ".sqlite"))
  withr::defer(DBI::dbDisconnect(store$con))
  token <- paste(rep("C", 43L), collapse = "")
  created <- share_env$cv_share_store_create(
    store,
    '{"schema":"test"}',
    "fingerprint-a",
    token = token,
    creator = "alice",
    dataset_label = "PBMC"
  )

  expect_identical(created$token, token)
  rows <- share_env$cv_share_store_list(store)
  expect_identical(rows$creator[[1L]], "alice")
  expect_identical(rows$dataset_label[[1L]], "PBMC")
})

test_that("administrators can inventory and revoke independent links", {
  store <- share_env$cv_share_store_open(tempfile(fileext = ".sqlite"))
  withr::defer(DBI::dbDisconnect(store$con))
  now <- as.POSIXct("2026-08-20 12:00:00", tz = "UTC")
  first <- share_env$cv_share_store_create(
    store,
    '{"schema":"first"}',
    "fingerprint-a",
    now = now,
    creator = "alice",
    dataset_label = "PBMC"
  )
  second <- share_env$cv_share_store_create(
    store,
    '{"schema":"second"}',
    "fingerprint-a",
    now = now + 1,
    creator = "alice",
    dataset_label = "PBMC"
  )

  rows <- share_env$cv_share_store_list(store, now = now + 2)
  expect_setequal(rows$token, c(first$token, second$token))
  expect_true(all(rows$creator == "alice"))
  expect_true(all(rows$dataset_label == "PBMC"))
  expect_false("json" %in% names(rows))
  expect_false("receipt_hash" %in% names(rows))

  share_env$cv_share_store_revoke_admin(
    store,
    first$token,
    now = now + 3
  )
  expect_error(
    share_env$cv_share_store_fetch(
      store,
      first$token,
      "fingerprint-a",
      now = now + 3
    ),
    class = "cv_share_error"
  )
  expect_identical(
    share_env$cv_share_store_fetch(
      store,
      second$token,
      "fingerprint-a",
      now = now + 3
    )$json,
    '{"schema":"second"}'
  )
})

test_that("a new share database contains only the admin-managed schema", {
  path <- tempfile(fileext = ".sqlite")
  store <- share_env$cv_share_store_open(path)
  withr::defer(DBI::dbDisconnect(store$con))
  columns <- DBI::dbGetQuery(
    store$con,
    "PRAGMA table_info(linked_view_shares)"
  )$name
  expect_setequal(
    columns,
    c(
      "token",
      "json",
      "fingerprint",
      "created_at",
      "expires_at",
      "revoked_at",
      "creator",
      "dataset_label"
    )
  )
  expect_false(exists(
    "cv_share_store_revoke",
    envir = share_env,
    inherits = FALSE
  ))
})

test_that("the pre-admin receipt schema migrates without losing links", {
  path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE linked_view_shares (",
      "token TEXT PRIMARY KEY, receipt_hash TEXT NOT NULL, json TEXT NOT NULL,",
      "fingerprint TEXT NOT NULL, created_at TEXT NOT NULL, expires_at TEXT NOT NULL,",
      "revoked_at TEXT, creator TEXT, dataset_label TEXT)"
    )
  )
  token <- paste(rep("L", 43L), collapse = "")
  DBI::dbExecute(
    con,
    "INSERT INTO linked_view_shares VALUES (?, ?, ?, ?, ?, ?, NULL, ?, ?)",
    params = list(
      token,
      paste(rep("0", 64L), collapse = ""),
      '{"schema":"legacy"}',
      "fingerprint-a",
      "2026-08-20T12:00:00Z",
      "2026-11-18T12:00:00Z",
      "alice",
      "PBMC"
    )
  )
  DBI::dbDisconnect(con)

  store <- share_env$cv_share_store_open(path)
  withr::defer(DBI::dbDisconnect(store$con))
  columns <- DBI::dbGetQuery(
    store$con,
    "PRAGMA table_info(linked_view_shares)"
  )$name
  expect_false("receipt_hash" %in% columns)
  expect_identical(
    share_env$cv_share_store_fetch(
      store,
      token,
      "fingerprint-a",
      as.POSIXct("2026-08-21", tz = "UTC")
    )$json,
    '{"schema":"legacy"}'
  )
})
