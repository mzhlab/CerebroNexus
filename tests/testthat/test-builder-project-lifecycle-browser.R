library(shinytest2)

builder_project_lifecycle_browser_require <- function() {
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("openssl")
}

builder_project_lifecycle_manifest <- function(project_dir) {
  jsonlite::read_json(
    file.path(project_dir, "builder-project.json"),
    simplifyVector = FALSE
  )
}

builder_project_lifecycle_wait_for_manifest <- function(
  app,
  project_dir,
  revision = 1L,
  timeout = 60000
) {
  manifest_path <- file.path(project_dir, "builder-project.json")
  started <- Sys.time()
  repeat {
    if (file.exists(manifest_path)) {
      manifest <- tryCatch(
        builder_project_lifecycle_manifest(project_dir),
        error = function(error) NULL
      )
      if (
        is.list(manifest) &&
          as.integer(manifest$project$revision %||% 0L) >= revision
      ) {
        app$wait_for_idle(timeout = 10000)
        return(manifest)
      }
    }
    if (
      as.numeric(difftime(Sys.time(), started, units = "secs")) > timeout / 1000
    ) {
      browser_state <- tryCatch(
        app$get_js(paste0(
          "(() => ({",
          "modal: document.querySelector('#shiny-modal .modal-title')?.textContent.trim() || null,",
          "status: document.querySelector('.builder-project-status')?.textContent.trim() || null,",
          "notifications: Array.from(document.querySelectorAll('.shiny-notification-content'))",
          ".map(node => node.textContent.trim()),",
          "body: document.body.innerText.slice(0, 1200)",
          "}))()"
        )),
        error = function(error) list(browser_error = conditionMessage(error))
      )
      stop(
        paste0(
          "Builder Project manifest did not reach revision ",
          revision,
          ". Directory entries: ",
          paste(
            list.files(project_dir, all.files = TRUE, no.. = TRUE),
            collapse = ", "
          ),
          ". Browser state: ",
          jsonlite::toJSON(browser_state, auto_unbox = TRUE)
        ),
        call. = FALSE
      )
    }
    Sys.sleep(0.1)
  }
}

builder_project_lifecycle_record <- function(manifest, id = NULL) {
  records <- manifest$datasets %||% list()
  if (is.null(id)) {
    stopifnot(length(records) == 1L)
    return(records[[1L]])
  }
  ids <- vapply(records, `[[`, character(1), "id")
  records[[match(id, ids)]]
}

builder_project_lifecycle_close_result <- function(app) {
  app$wait_for_js(
    paste0(
      "document.body.classList.contains('builder-project-result-open') && ",
      "document.querySelector('#builder-operation-overlay-actions button') !== null"
    ),
    timeout = 30000
  )
  app$click(selector = "#builder-operation-overlay-actions button:first-child")
  app$wait_for_js(
    "!document.body.classList.contains('builder-project-result-open')",
    timeout = 10000
  )
  invisible(TRUE)
}

builder_project_lifecycle_save <- function(app, project_dir) {
  before <- builder_project_lifecycle_manifest(project_dir)
  revision <- as.integer(before$project$revision) + 1L
  app$click("save_builder_project")
  manifest <- builder_project_lifecycle_wait_for_manifest(
    app,
    project_dir,
    revision = revision
  )
  app$wait_for_js(
    paste0(
      "document.body.classList.contains('builder-project-result-open') && ",
      "document.querySelector('#builder-operation-overlay-actions button') !== null"
    ),
    timeout = 60000
  )
  manifest
}

builder_project_lifecycle_check_current <- function(app) {
  id <- app$get_js(
    "document.querySelector('#ds_ready_list .builder-pick[aria-current=true]').dataset.ds"
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('complete_dataset_check') !== null && ",
      "document.getElementById('complete_dataset_check').disabled === false"
    ),
    timeout = 10000
  )
  app$click("complete_dataset_check")
  app$wait_for_js(
    sprintf(
      paste0(
        "document.querySelector('#ds_ready_list .ds[data-ds=%s] ",
        ".ds-ready-summary.is-checked') !== null"
      ),
      jsonlite::toJSON(id, auto_unbox = TRUE)
    ),
    timeout = 30000
  )
  invisible(TRUE)
}

builder_project_lifecycle_prepare_crb <- function(
  app,
  project_dir,
  timeout = 120000
) {
  before <- builder_project_lifecycle_manifest(project_dir)
  app$wait_for_js(
    paste0(
      "(function(){var title=document.getElementById(",
      "'builder-operation-overlay-title');return !!(title && ",
      "title.textContent.trim());})()"
    ),
    timeout = 30000
  )
  app$wait_for_js(
    paste0(
      "(function(){var title=document.getElementById(",
      "'builder-operation-overlay-title');return !!(title && (",
      "title.textContent.trim() === 'Project and CRBs saved' || ",
      "title.textContent.trim() === 'CRB preparation failed'));})()"
    ),
    timeout = timeout
  )
  expect_identical(
    app$get_js(
      "document.getElementById('builder-operation-overlay-title').textContent.trim()"
    ),
    "Project and CRBs saved",
    info = app$get_js(
      "document.getElementById('builder-operation-overlay').textContent.trim()"
    )
  )
  builder_project_lifecycle_wait_for_manifest(
    app,
    project_dir,
    revision = as.integer(before$project$revision) + 1L,
    timeout = timeout
  )
}

test_that("Check Save and Prepare CRB transitions stay isolated across datasets", {
  builder_project_lifecycle_browser_require()
  app_dir <- builder_browser_current_contract_app(
    builder_profile_inst_path("builder"),
    .local_envir = environment()
  )
  local_app_support(app_dir)
  project_dir <- file.path(withr::local_tempdir(), "unchecked-project")
  builder_browser_project_folder_picker(project_dir)

  app <- AppDriver$new(
    app_dir,
    name = "builder_project_unchecked_save",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)

  builder_browser_wait_for_example_ready(app)
  app$click(selector = ".example-btn[data-ex=all_content]")
  app$wait_for_js(
    "document.getElementById('complete_dataset_check') !== null",
    timeout = 60000
  )
  builder_browser_dismiss_project_offer(app)
  app$click("save_builder_project")
  app$wait_for_js(
    "document.getElementById('choose_builder_project_folder') !== null",
    timeout = 10000
  )
  app$click("choose_builder_project_folder")

  manifest <- builder_project_lifecycle_wait_for_manifest(app, project_dir)
  expect_length(manifest$datasets, 1L)
  expect_false(isTRUE(manifest$datasets[[1L]]$configuration$checked))
  expect_null(manifest$datasets[[1L]]$artifact)

  app$wait_for_js(
    "document.querySelector('.builder-project-status').textContent.includes('fully saved')",
    timeout = 60000
  )
  builder_project_lifecycle_check_current(app)
  checked_manifest <- builder_project_lifecycle_save(app, project_dir)
  checked_record <- builder_project_lifecycle_record(checked_manifest)
  expect_true(isTRUE(checked_record$configuration$checked))
  expect_null(checked_record$artifact)

  ready_manifest <- builder_project_lifecycle_prepare_crb(app, project_dir)
  ready_record <- builder_project_lifecycle_record(ready_manifest)
  expect_true(isTRUE(ready_record$configuration$checked))
  expect_identical(ready_record$artifact$status, "ready")
  expect_true(isTRUE(ready_record$artifact$reusable))
  expect_true(file.exists(file.path(project_dir, ready_record$artifact$path)))
  first_artifact_path <- ready_record$artifact$path
  builder_project_lifecycle_close_result(app)

  app$set_inputs(`core-name` = "All content edited after CRB")
  app$wait_for_js(
    paste0(
      "document.getElementById('complete_dataset_check') !== null && ",
      "document.getElementById('complete_dataset_check').disabled === false && ",
      "document.getElementById('complete_dataset_check').textContent.includes('Finish checking')"
    ),
    timeout = 30000
  )
  stale_manifest <- builder_project_lifecycle_save(app, project_dir)
  stale_record <- builder_project_lifecycle_record(stale_manifest)
  expect_false(isTRUE(stale_record$configuration$checked))
  expect_identical(stale_record$artifact$status, "stale")
  expect_false(app$get_js(paste0(
    "Array.from(document.querySelectorAll('#builder-operation-overlay-actions button'))",
    ".some(button => button.textContent.includes('Prepare checked CRBs'))"
  )))
  revision_before_rejected_prepare <- stale_manifest$project$revision
  generation_dirs_before <- list.dirs(
    file.path(project_dir, "artifacts", stale_record$id, "generations"),
    recursive = FALSE,
    full.names = FALSE
  )
  app$run_js(paste0(
    "Shiny.setInputValue('prepare_builder_project_crbs', ",
    "{request_id:'unchecked-direct-request',nonce:Date.now()},",
    "{priority:'event'});"
  ))
  app$wait_for_idle(timeout = 10000)
  Sys.sleep(1)
  rejected_manifest <- builder_project_lifecycle_manifest(project_dir)
  expect_identical(
    rejected_manifest$project$revision,
    revision_before_rejected_prepare
  )
  expect_identical(
    list.dirs(
      file.path(project_dir, "artifacts", stale_record$id, "generations"),
      recursive = FALSE,
      full.names = FALSE
    ),
    generation_dirs_before
  )
  builder_project_lifecycle_close_result(app)

  builder_project_lifecycle_check_current(app)
  rechecked_manifest <- builder_project_lifecycle_save(app, project_dir)
  expect_true(isTRUE(
    builder_project_lifecycle_record(rechecked_manifest)$configuration$checked
  ))
  rebuilt_manifest <- builder_project_lifecycle_prepare_crb(app, project_dir)
  rebuilt_record <- builder_project_lifecycle_record(rebuilt_manifest)
  expect_identical(rebuilt_record$artifact$status, "ready")
  expect_true(isTRUE(rebuilt_record$artifact$reusable))
  expect_false(identical(rebuilt_record$artifact$path, first_artifact_path))
  expect_true(file.exists(file.path(project_dir, rebuilt_record$artifact$path)))
  checkpoint_dirs <- list.dirs(
    file.path(project_dir, "checkpoints"),
    recursive = FALSE
  )
  expect_identical(
    checkpoint_dirs,
    character(),
    info = paste(checkpoint_dirs, collapse = "\n")
  )
  builder_project_lifecycle_close_result(app)

  first_id <- rebuilt_record$id
  upload_path <- file.path(app_dir, "fixtures", "all_content.rds")
  dispatch <- list(
    client_id = "client-import-project-mixed-state",
    name = basename(upload_path),
    size = unname(file.info(upload_path)$size),
    nonce = 1
  )
  app$run_js(sprintf(
    "Shiny.setInputValue('builder_client_import_dispatch', %s, {priority:'event'});",
    jsonlite::toJSON(dispatch, auto_unbox = TRUE)
  ))
  app$upload_file(dataset_files = upload_path)
  app$wait_for_js(
    paste0(
      "document.querySelectorAll('#ds_ready_list .ds[data-ds]').length === 2 && ",
      "document.querySelectorAll('.ds--import').length === 0"
    ),
    timeout = 60000
  )
  app$wait_for_idle(timeout = 30000)
  second_id <- app$get_js(
    "document.querySelectorAll('#ds_ready_list .builder-pick')[1].dataset.ds"
  )
  app$click(selector = "#ds_ready_list .ds:nth-child(2) .builder-pick")
  app$wait_for_js(
    sprintf(
      paste0(
        "document.getElementById('core-rendered_for').value === %s && ",
        "document.querySelector('.builder-dataset-switch-veil') === null"
      ),
      jsonlite::toJSON(second_id, auto_unbox = TRUE)
    ),
    timeout = 30000
  )
  expect_false(identical(second_id, first_id))
  builder_project_lifecycle_check_current(app)
  app$wait_for_idle(timeout = 10000)
  second_checked <- sprintf(
    "document.querySelector('#ds_ready_list .ds[data-ds=%s] .ds-ready-summary.is-checked') !== null",
    jsonlite::toJSON(second_id, auto_unbox = TRUE)
  )
  expect_true(
    app$get_js(second_checked),
    info = "after checking second dataset"
  )

  first_selector <- sprintf(
    "#ds_ready_list .builder-pick[data-ds=%s]",
    jsonlite::toJSON(first_id, auto_unbox = TRUE)
  )
  app$click(selector = first_selector)
  app$wait_for_js(
    sprintf(
      "document.getElementById('core-rendered_for').value === %s",
      jsonlite::toJSON(first_id, auto_unbox = TRUE)
    ),
    timeout = 30000
  )
  expect_true(
    app$get_js(second_checked),
    info = "after switching to first dataset"
  )
  app$set_inputs(`core-name` = "First dataset unchecked in mixed save")
  app$wait_for_js(
    paste0(
      "document.getElementById('complete_dataset_check') !== null && ",
      "document.getElementById('complete_dataset_check').disabled === false"
    ),
    timeout = 30000
  )
  expect_true(app$get_js(second_checked), info = "after editing first dataset")

  mixed_manifest <- builder_project_lifecycle_save(app, project_dir)
  mixed_first <- builder_project_lifecycle_record(mixed_manifest, first_id)
  mixed_second <- builder_project_lifecycle_record(mixed_manifest, second_id)
  expect_false(isTRUE(mixed_first$configuration$checked))
  expect_identical(mixed_first$artifact$status, "stale")
  expect_true(isTRUE(mixed_second$configuration$checked))
  expect_null(mixed_second$artifact)

  mixed_ready_manifest <- builder_project_lifecycle_prepare_crb(
    app,
    project_dir
  )
  mixed_ready_first <- builder_project_lifecycle_record(
    mixed_ready_manifest,
    first_id
  )
  mixed_ready_second <- builder_project_lifecycle_record(
    mixed_ready_manifest,
    second_id
  )
  expect_false(isTRUE(mixed_ready_first$configuration$checked))
  expect_identical(mixed_ready_first$artifact$status, "stale")
  expect_true(isTRUE(mixed_ready_second$configuration$checked))
  expect_identical(mixed_ready_second$artifact$status, "ready")
  expect_true(isTRUE(mixed_ready_second$artifact$reusable))
  expect_true(file.exists(file.path(
    project_dir,
    mixed_ready_second$artifact$path
  )))
  expect_identical(
    list.dirs(file.path(project_dir, "checkpoints"), recursive = FALSE),
    character()
  )
  builder_expect_clean_browser_logs(app)
})
