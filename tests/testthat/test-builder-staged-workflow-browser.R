library(shinytest2)

builder_expect_no_horizontal_overflow <- function(app) {
  geometry <- app$get_js(paste0(
    "({documentWidth: document.documentElement.scrollWidth, ",
    "clientWidth: document.documentElement.clientWidth})"
  ))
  expect_lte(geometry$documentWidth, geometry$clientWidth + 1)
}

builder_wait_for_visible_stage_focus <- function(app, stage) {
  selector <- sprintf("[data-workflow-stage=%s] h2", stage)
  app$wait_for_js(
    sprintf(
      paste0(
        "(() => { const heading = document.querySelector('%s'); ",
        "const topbar = document.querySelector('.topbar'); ",
        "return heading && topbar && document.activeElement === heading && ",
        "heading.getBoundingClientRect().top >= ",
        "topbar.getBoundingClientRect().bottom - 2; })()"
      ),
      selector
    ),
    timeout = 10000
  )
}

test_that("staged workflow remains focused and overflow-free", {
  skip_if_not(identical(Sys.getenv("CEREBRO_RUN_BROWSER_TESTS"), "true"))
  app_dir <- builder_profile_inst_path("builder")
  local_app_support(app_dir)

  for (viewport in list(c(1920L, 1080L), c(768L, 1024L), c(390L, 844L))) {
    app <- AppDriver$new(
      app_dir,
      name = paste0("builder_staged_", viewport[[1]]),
      width = viewport[[1]],
      height = viewport[[2]],
      load_timeout = 60000
    )
    on.exit(app$stop(), add = TRUE)
    app$wait_for_idle(timeout = 30000)
    expect_identical(
      app$get_js(
        "document.querySelectorAll('[data-workflow-stage=upload]').length"
      ),
      1L
    )
    expect_false(app$get_js(
      "!!document.querySelector('#continue_to_review, .actionbar, [data-workflow-stage=build]')"
    ))
    builder_browser_wait_for_example_ready(app)
    app$click(selector = ".example-btn[data-ex=all_content]")
    app$wait_for_js(
      paste0(
        "document.querySelector('.ds--import') !== null && ",
        "document.querySelector('[data-workflow-stage=upload]') !== null && ",
        "document.querySelector('#continue_to_review, #confirm_review, ",
        ".actionbar, [data-workflow-stage=build], #make_app') === null"
      ),
      timeout = 10000
    )
    builder_expect_no_horizontal_overflow(app)
    app$wait_for_js(
      "document.getElementById('complete_dataset_check') !== null",
      timeout = 60000
    )
    builder_browser_dismiss_project_offer(app)
    expect_identical(
      app$get_js(
        "document.querySelectorAll('.builder-workflow-stage-link').length"
      ),
      1L
    )
    expect_identical(
      app$get_js(
        "document.querySelectorAll('.builder-workflow-progress .is-unavailable').length"
      ),
      2L
    )
    app$wait_for_js(
      "document.querySelector('[data-workflow-stage=configure]') !== null",
      timeout = 10000
    )
    builder_expect_no_horizontal_overflow(app)

    builder_browser_check_all_datasets(app)
    expect_identical(
      app$get_js("document.querySelectorAll('#continue_to_review').length"),
      1L
    )
    app$click("continue_to_review")
    builder_wait_for_visible_stage_focus(app, "review")
    expect_identical(
      app$get_js("document.querySelectorAll('#confirm_review').length"),
      1L
    )
    expect_identical(
      app$get_js("document.querySelectorAll('#back_to_settings').length"),
      1L
    )
    expect_false(app$get_js(
      "!!document.querySelector('[data-workflow-stage=review] input:not([type=hidden]), [data-workflow-stage=review] select, [data-workflow-stage=review] textarea')"
    ))
    initial_revision <- app$get_js(paste0(
      "document.querySelector('.review-plan-revision').textContent.trim()",
      ".match(/(\\d+)$/)[1]"
    ))
    builder_expect_no_horizontal_overflow(app)

    app$click("confirm_review")
    builder_wait_for_visible_stage_focus(app, "build")
    expect_identical(
      app$get_js("document.querySelectorAll('#build-stage-status').length"),
      1L
    )
    expect_identical(
      app$get_js(
        "document.querySelector('.builder-workflow-progress').dataset.workflowConfirmed"
      ),
      "true"
    )
    expect_identical(
      app$get_js(paste0(
        "document.querySelector('.confirmed-plan-revision').textContent.trim()",
        ".match(/(\\d+)$/)[1]"
      )),
      initial_revision
    )
    builder_expect_no_horizontal_overflow(app)

    app$wait_for_js(
      "document.getElementById('workflow_stage_review') !== null",
      timeout = 10000
    )
    app$click("workflow_stage_review")
    builder_wait_for_visible_stage_focus(app, "review")
    expect_identical(
      app$get_js(
        "document.querySelector('.builder-workflow-progress').dataset.workflowConfirmed"
      ),
      "true"
    )
    app$click("workflow_stage_configure")
    builder_wait_for_visible_stage_focus(app, "configure")
    expect_identical(
      app$get_js(
        "document.querySelector('.builder-workflow-progress').dataset.workflowConfirmed"
      ),
      "true"
    )
    app$set_inputs(`core-name` = paste0("Accepted ", viewport[[1]]))
    app$wait_for_js(
      "document.querySelector('.builder-workflow-progress').dataset.workflowConfirmed === 'false'",
      timeout = 10000
    )
    expect_false(app$get_js(
      "!!document.querySelector('[data-workflow-stage=build]')"
    ))
    expect_identical(
      app$get_js("document.getElementById('core-name').value"),
      paste0("Accepted ", viewport[[1]])
    )
    builder_browser_check_all_datasets(app)
    app$click("continue_to_review")
    app$wait_for_js(
      "document.getElementById('confirm_review') !== null",
      timeout = 10000
    )
    expect_true(app$get_js(sprintf(
      "Array.from(document.querySelectorAll('.review-dataset-card > summary strong')).some(node => node.textContent.trim() === %s)",
      jsonlite::toJSON(paste0("Accepted ", viewport[[1]]), auto_unbox = TRUE)
    )))
    expect_identical(
      app$get_js(
        "document.querySelector('.builder-workflow-progress').dataset.workflowConfirmed"
      ),
      "false"
    )
    revised_revision <- app$get_js(paste0(
      "document.querySelector('.review-plan-revision').textContent.trim()",
      ".match(/(\\d+)$/)[1]"
    ))
    expect_gt(as.integer(revised_revision), as.integer(initial_revision))
    app$click("confirm_review")
    builder_wait_for_visible_stage_focus(app, "build")
    expect_identical(
      app$get_js("document.querySelectorAll('#build-stage-status').length"),
      1L
    )
    expect_identical(
      app$get_js(
        "document.querySelector('.builder-workflow-progress').dataset.workflowConfirmed"
      ),
      "true"
    )
    expect_identical(
      app$get_js(paste0(
        "document.querySelector('.confirmed-plan-revision').textContent.trim()",
        ".match(/(\\d+)$/)[1]"
      )),
      revised_revision
    )
    builder_expect_no_horizontal_overflow(app)
    builder_expect_clean_browser_logs(app)
    app$stop()
  }
})
