# test-hla-tcr-main-case-browser.R — real Viewer acceptance for the fixture.

library(shinytest2)

main_case_browser_inst <- system.file(package = "CerebroNexus")
if (
  !nzchar(main_case_browser_inst) ||
    !file.exists(file.path(main_case_browser_inst, "app.R"))
) {
  main_case_browser_inst <- testthat::test_path("../../inst")
}

main_case_browser_config <- file.path(
  main_case_browser_inst,
  "extdata/examples/demo_hla_tcr_main_case.linked-view.json"
)

test_that("the real Viewer restores the HLA/TCR main-case selection", {
  local_app_support(main_case_browser_inst)
  app <- AppDriver$new(
    main_case_browser_inst,
    name = "hla_tcr_main_case_browser",
    height = 900,
    width = 1440
  )
  on.exit(app$stop(), add = TRUE)

  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    "document.getElementById('crb_file_selector') !== null",
    timeout = 30000
  )
  app$set_inputs(
    crb_file_selector = "extdata/examples/demo_hla_tcr_dextramer.crb",
    wait_ = FALSE
  )
  app$wait_for_js(
    "document.body.textContent.indexOf('12,000') >= 0",
    timeout = 60000
  )
  app$run_js(paste0(
    "document.querySelector(",
    "'a[href=\"#shiny-tab-coordinated_views\"]'",
    ").click();"
  ))
  app$wait_for_js(
    paste0(
      "window.cerebroLinkedViewsState && ",
      "window.cerebroLinkedViewsState.ready()"
    ),
    timeout = 60000
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('12,000 cells') >= 0",
    timeout = 30000
  )

  app$upload_file(coordviews_config_upload = main_case_browser_config)
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-config-status').textContent.indexOf(",
      "'Restored 293 selected cells and view settings.') >= 0"
    ),
    timeout = 60000
  )

  restored <- app$get_js(paste0(
    "(function(){var state=window.cerebroLinkedViewsState.capture();",
    "return {selected:state.selection.cells.length,",
    "colour:state.view.colour.mode,",
    "projections:state.view.projections,",
    "cloneLayout:state.view.display.clone_layout,",
    "source:state.selection.source,",
    "first:state.selection.cells[0],",
    "last:state.selection.cells[state.selection.cells.length-1]};})()"
  ))

  expect_identical(as.integer(restored$selected), 293L)
  expect_identical(restored$colour, "sample")
  expect_identical(
    unlist(restored$projections, use.names = FALSE),
    "umap"
  )
  expect_identical(restored$cloneLayout, "stack")
  expect_identical(restored$source, "Clonal expansion (TCR)")
  expect_identical(restored$first, "donor1_AAACCTGGTCCGAAGA-29")
  expect_identical(restored$last, "donor1_TTGGAACAGTGCCAGA-14")
})
