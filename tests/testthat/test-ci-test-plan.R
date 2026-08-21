ci_test_plan_runner <- test_path("..", "..", "scripts", "run-test-shard.R")
ci_test_plan_available <- file.exists(ci_test_plan_runner)
ci_test_plan_api <- new.env(parent = globalenv())
if (ci_test_plan_available) {
  sys.source(ci_test_plan_runner, envir = ci_test_plan_api)
}

test_that("the Builder CI plan classifies every present test", {
  skip_if_not(ci_test_plan_available)
  plan <- ci_test_plan_api$ci_test_plan(test_path())
  discovered <- sort(list.files(
    test_path(),
    pattern = "^test-[[:alnum:]_.-]+[.]R$",
    full.names = FALSE
  ))

  expect_identical(plan$process_sensitive, "test-builder-worker.R")
  expect_true(all(
    c(
      "test-coordinated-views-config-browser.R",
      "test-hla-tcr-main-case-browser.R"
    ) %in%
      plan$browser
  ))
  classified <- c(plan$logic, plan$process_sensitive, plan$browser)
  expect_setequal(classified, discovered)
  expect_false(anyDuplicated(classified) > 0L)
})

test_that("round-robin sharding remains deterministic and lossless", {
  skip_if_not(ci_test_plan_available)
  files <- paste0("test-", letters[1:4], ".R")

  assigned <- ci_test_plan_api$ci_test_shards(
    files,
    2L
  )

  expect_identical(
    assigned,
    list(c("test-a.R", "test-c.R"), c("test-b.R", "test-d.R"))
  )
  expect_setequal(unlist(assigned, use.names = FALSE), files)
  expect_error(
    ci_test_plan_api$ci_test_shard_files(
      list(
        logic = files,
        process_sensitive = character(),
        browser = character()
      ),
      "logic",
      shard = 1.5,
      shards = 2L
    ),
    "shard"
  )
})

test_that("the shard runner rejects retired weighted options", {
  skip_if_not(ci_test_plan_available)
  expect_error(
    ci_test_plan_api$ci_parse_args(c("--strategy", "weighted")),
    "Unknown argument"
  )
})

test_that("browser references match the explicit browser group", {
  skip_if_not(ci_test_plan_available)
  files <- list.files(
    test_path(),
    pattern = "^test-[[:alnum:]_.-]+[.]R$",
    full.names = TRUE
  )
  files <- files[basename(files) != "test-ci-test-plan.R"]
  browser_references <- basename(files[vapply(
    files,
    function(file) {
      code <- sub("#.*$", "", readLines(file, warn = FALSE))
      any(grepl("shinytest2|AppDriver", code))
    },
    logical(1)
  )])

  expect_true(all(
    browser_references %in% ci_test_plan_api$ci_browser_test_files()
  ))
})

test_that("precheck only checks formatting", {
  skip_if_not(ci_test_plan_available)
  precheck <- paste(
    readLines(test_path("..", "..", "scripts", "precheck.sh"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(precheck, "air format --check [.]", perl = TRUE)
  expect_false(grepl("air format [.]($|\n)", precheck, perl = TRUE))
  expect_false(grepl("run-local-validation", precheck, fixed = TRUE))
  expect_match(precheck, "run_group process-sensitive", fixed = TRUE)
})

test_that("the CI workflow includes the Builder process-sensitive group", {
  skip_if_not(ci_test_plan_available)
  workflow <- readLines(
    test_path("..", "..", ".github", "workflows", "R-tests.yaml"),
    warn = FALSE
  )
  text <- paste(workflow, collapse = "\n")

  expect_true(any(grepl("^  process_sensitive:$", workflow)))
  expect_match(text, "needs: [logic, process_sensitive, browser]", fixed = TRUE)
})

test_that("manual pkgdown validation never deploys the site", {
  skip_if_not(ci_test_plan_available)
  workflow <- paste(
    readLines(
      test_path("..", "..", ".github", "workflows", "pkgdown.yaml"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    workflow,
    "if: github.event_name == 'push' && github.ref == 'refs/heads/master'",
    fixed = TRUE
  )
})
