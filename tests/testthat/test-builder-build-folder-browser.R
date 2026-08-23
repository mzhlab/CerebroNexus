library(shinytest2)

builder_build_folder_picker <- function(
  output_dir,
  .local_envir = parent.frame()
) {
  skip_on_os("windows")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  fake_bin <- withr::local_tempdir(.local_envir = .local_envir)
  picker_name <- if (identical(Sys.info()[["sysname"]], "Darwin")) {
    "osascript"
  } else {
    "zenity"
  }
  picker <- file.path(fake_bin, picker_name)
  writeLines(
    c(
      "#!/bin/sh",
      "sleep 1",
      sprintf(
        "printf '%%s\\n' %s",
        shQuote(normalizePath(output_dir, winslash = "/", mustWork = TRUE))
      )
    ),
    picker
  )
  Sys.chmod(picker, mode = "0755")
  withr::local_envvar(
    PATH = paste(fake_bin, Sys.getenv("PATH"), sep = .Platform$path.sep),
    .local_envir = .local_envir
  )
  invisible(output_dir)
}

builder_build_folder_open_stage <- function(app) {
  builder_browser_wait_for_example_ready(app)
  app$click(selector = ".example-btn[data-ex=all_content]")
  app$wait_for_js(
    paste0(
      "document.querySelector('.ds-pick[aria-current=true]') !== null && ",
      "document.getElementById('complete_dataset_check') !== null"
    ),
    timeout = 60000
  )
  builder_browser_dismiss_project_offer(app)
  app$wait_for_idle(timeout = 30000)
  builder_browser_check_all_datasets(app)
  app$click("continue_to_review")
  app$wait_for_js(
    paste0(
      "document.getElementById('review-stage') !== null && ",
      "document.getElementById('confirm_review') !== null"
    ),
    timeout = 30000
  )
  app$click("confirm_review")
  app$wait_for_js(
    paste0(
      "document.querySelector('[data-workflow-stage=build]') !== null && ",
      "document.querySelectorAll('#build-stage-status').length === 1 && ",
      "document.getElementById('choose_output_folder') !== null && ",
      "document.getElementById('build') !== null && ",
      "document.getElementById('build').disabled"
    ),
    timeout = 30000
  )
}

test_that("confirmed Build waits for a separately selected output folder", {
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)
  output_dir <- file.path(
    withr::local_tempdir(),
    "builder-native-folder-output"
  )
  builder_build_folder_picker(output_dir)

  app <- AppDriver$new(
    app_dir,
    name = "builder_native_folder_queue",
    width = 1280,
    height = 900,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 30000)
  builder_build_folder_open_stage(app)

  expect_true(app$get_js(
    paste0(
      "document.querySelector('#build-stage-status .builder-build-status-section') === null && ",
      "document.querySelector('#build_stage_footer #build') !== null"
    )
  ))
  app$get_js(
    "window.__builderStableBuildHost = document.getElementById('build-stage-status'); true"
  )

  app$click("choose_output_folder")
  app$wait_for_js(
    paste0(
      "document.getElementById('build-stage-status') === window.__builderStableBuildHost && ",
      "document.querySelector('#build-stage-status .builder-build-status-section') === null && ",
      "document.querySelector('#build_stage_footer #build') !== null && ",
      "!document.getElementById('build').disabled && ",
      "document.querySelector('.builder-selected-output').textContent.includes(",
      "'builder-native-folder-output')"
    ),
    timeout = 30000
  )
  app$wait_for_idle(timeout = 10000)

  expect_false(app$get_js(
    "document.querySelector('#build-stage-status .result-card') !== null"
  ))

  app$click("build")
  app$wait_for_js(
    paste0(
      "document.querySelectorAll('#build-stage-status').length === 1 && ",
      "document.getElementById('build-stage-status') === window.__builderStableBuildHost && ",
      "document.querySelector('#build-stage-status ",
      ".builder-build-status-section .builder-build-pipeline') !== null && ",
      "document.getElementById('dataset_files').disabled && ",
      "Array.from(document.querySelectorAll('.builder-reorder, .builder-drop')).every(control => control.disabled) && ",
      "document.querySelector('.topbar .builder-build-pipeline') === null"
    ),
    timeout = 30000
  )
  app$wait_for_js(
    paste0(
      "document.querySelectorAll('#build-stage-status').length === 1 && ",
      "document.getElementById('build-stage-status') === window.__builderStableBuildHost && ",
      "document.querySelector('#build-stage-status ",
      ".builder-build-status-section .result-card') !== null"
    ),
    timeout = 120000
  )

  expect_true(app$get_js(paste0(
    "!document.getElementById('dataset_files').disabled && ",
    "Array.from(document.querySelectorAll('.builder-drop')).every(control => !control.disabled)"
  )))
  app$click("choose_output_folder")
  app$wait_for_js(
    paste0(
      "document.getElementById('build-stage-status') === window.__builderStableBuildHost && ",
      "document.querySelector('#build-stage-status .builder-build-status-section') === null && ",
      "document.querySelector('#build_stage_footer #build') !== null && ",
      "!document.getElementById('build').disabled"
    ),
    timeout = 30000
  )

  app$click("build")
  conflict_ready <- paste0(
    "document.querySelector('.builder-build-dialog-backdrop.is-visible') !== null && ",
    "document.querySelector('.builder-build-dialog') !== null && ",
    "document.querySelector('.builder-build-dialog').getClientRects().length === 1 && ",
    "document.querySelector('.builder-build-dialog .btn-replace') !== null"
  )
  tryCatch(
    app$wait_for_js(conflict_ready, timeout = 30000),
    error = function(error) {
      stop(
        conditionMessage(error),
        "\nBuild UI: ",
        app$get_js(
          "document.getElementById('build-stage-status').textContent.trim()"
        ),
        call. = FALSE
      )
    }
  )
  app$click(selector = ".builder-build-dialog button:first-child")
  app$wait_for_js(
    paste0(
      "document.querySelector('.builder-build-dialog-backdrop') === null && ",
      "!document.getElementById('build').disabled"
    ),
    timeout = 10000
  )
})
