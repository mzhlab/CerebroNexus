# test-viewer-admin-ui.R — authenticated Viewer administration shell.

viewer_admin_inst <- normalizePath(
  testthat::test_path("../../inst"),
  mustWork = TRUE
)
viewer_admin_core <- new.env(parent = globalenv())
sys.source(
  file.path(viewer_admin_inst, "viewer/admin/core.R"),
  envir = viewer_admin_core
)

test_that("only the exact Admin path is rewritten", {
  expect_true(viewer_admin_core$viewer_admin_route("/admin"))
  expect_true(viewer_admin_core$viewer_admin_route("/admin/"))
  expect_true(viewer_admin_core$viewer_admin_route("/apps/cerebro/admin"))
  expect_false(viewer_admin_core$viewer_admin_route("/administrator"))
  expect_false(viewer_admin_core$viewer_admin_route("/admin/shares"))
})

test_that("session authorization defaults closed", {
  session <- list(userData = new.env(parent = emptyenv()))
  expect_identical(
    viewer_admin_core$viewer_auth_context(session),
    list(authenticated = FALSE, user = NULL, is_admin = FALSE)
  )
  session$userData$viewer_auth <- list(
    authenticated = TRUE,
    user = "alice",
    is_admin = TRUE
  )
  expect_true(viewer_admin_core$viewer_is_admin(session))
  session$userData$viewer_auth$is_admin <- FALSE
  expect_false(viewer_admin_core$viewer_is_admin(session))
})

test_that("Admin inventory strips private share data", {
  rows <- data.frame(
    token = paste(rep("A", 43L), collapse = ""),
    fingerprint = "fingerprint-a",
    dataset_label = "PBMC",
    creator = "alice",
    created_at = "2026-08-20T12:00:00Z",
    expires_at = "2026-11-18T12:00:00Z",
    revoked_at = NA_character_,
    stringsAsFactors = FALSE
  )
  safe <- viewer_admin_core$viewer_admin_records(rows)
  expect_length(safe, 1L)
  expect_identical(
    names(safe[[1L]]),
    c(
      "token",
      "fingerprint",
      "dataset_label",
      "creator",
      "created_at",
      "expires_at"
    )
  )
})

test_that("Admin UI and assets expose one coherent management page", {
  ui <- paste(
    readLines(
      file.path(viewer_admin_inst, "viewer/admin/UI.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  server <- paste(
    readLines(
      file.path(viewer_admin_inst, "viewer/admin/server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  script <- paste(
    readLines(
      file.path(viewer_admin_inst, "viewer/www/admin.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  clipboard <- paste(
    readLines(
      file.path(viewer_admin_inst, "viewer/www/viewer-clipboard.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(ui, 'tabName = "admin"', fixed = TRUE)
  expect_match(ui, "Shared views", fixed = TRUE)
  expect_match(server, "viewer_admin_request", fixed = TRUE)
  expect_match(server, "viewer_is_admin(session)", fixed = TRUE)
  expect_match(server, 'session$clientData$url_pathname', fixed = TRUE)
  expect_match(script, "Copied ✓", fixed = TRUE)
  expect_match(script, "shiny:connected", fixed = TRUE)
  expect_match(script, "cerebro:share-created", fixed = TRUE)
  expect_match(
    script,
    "JSON.stringify(nextRecords) !== JSON.stringify(records)",
    fixed = TRUE
  )
  expect_match(
    clipboard,
    "root.setTimeout(function () { finish(false); }, 500)",
    fixed = TRUE
  )
  expect_match(script, "window.cerebroClipboard.copyText", fixed = TRUE)
  expect_match(server, "invalidateLater(2000, session)", fixed = TRUE)
})

test_that("Admin deep links switch tabs only after the menu is inserted", {
  server <- paste(
    readLines(
      file.path(viewer_admin_inst, "viewer/admin/server.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  admin_start <- regexpr(
    "if (viewer_is_admin(session)) {",
    server,
    fixed = TRUE
  )[[1L]]
  viewer_start <- regexpr(
    "} else {",
    server,
    fixed = TRUE
  )[[1L]]
  expect_gt(admin_start, 0L)
  expect_gt(viewer_start, admin_start)
  admin_flush <- substr(server, admin_start, viewer_start - 1L)
  insert_at <- regexpr("insertUI(", admin_flush, fixed = TRUE)[[1L]]
  select_at <- regexpr(
    'updateTabItems(session, "sidebar", selected = "admin")',
    admin_flush,
    fixed = TRUE
  )[[1L]]
  expect_gt(insert_at, 0L)
  expect_gt(select_at, insert_at)
})
