builder_plan_contract_source_runtime(environment())

test_that("rail Review and BuildPlan share manifest readiness", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    defaults <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      make_app = TRUE
    )
    expect_identical(
      defaults$app_options$point_size,
      list(overview_projection_point_size = 5)
    )
    expect_identical(defaults$app_options$variable_to_compare, FALSE)

    blocking <- builder_task6_entry(status = "blocking")
    state <- builder_dataset_state(blocking)
    failed <- builder_make_plan(list(blocking), tempdir(), make_app = FALSE)

    expect_identical(state$readiness, "blocked")
    expect_identical(state$blocking_ids, "expression")
    expect_identical(failed$error_code, "blocking_capability")
    expect_match(failed$error, "blocking capability", ignore.case = TRUE)

    ready <- builder_task6_entry()
    ready_state <- builder_dataset_state(ready)
    plan <- builder_freeze_plan(list(ready), tempdir(), make_app = FALSE)
    expect_null(plan$error)
    expect_identical(plan$items[[1L]]$readiness, ready_state$readiness)
    expect_identical(plan$readiness, ready_state$readiness)
  })
})

test_that("legacy alignment saved flags do not affect freezing", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$settings$images$fov <- list(
      uri = "data:image/png;base64,AAAA",
      bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      saved = FALSE,
      outside = 0L
    )
    ready <- builder_freeze_plan(list(entry), tempdir(), make_app = FALSE)
    expect_null(ready$error)

    entry$settings$images$fov$outside <- 1L
    blocked <- builder_freeze_plan(list(entry), tempdir(), make_app = FALSE)
    expect_identical(blocked$error_code, "spatial_alignment_outside")
    expect_match(blocked$error, "fov")
    expect_match(blocked$error, "outside")

    entry$settings$images$fov$outside <- 0L
    ready <- builder_freeze_plan(list(entry), tempdir(), make_app = FALSE)
    expect_null(ready$error)

    for (invalid in list("invalid", NA_integer_, c(0L, 1L), -1L, 1.5)) {
      entry$settings$images$fov$outside <- invalid
      blocked <- builder_freeze_plan(list(entry), tempdir(), make_app = FALSE)
      expect_identical(
        blocked$error_code,
        "invalid_spatial_alignment_diagnostics"
      )
    }

    entry$settings$images$fov <- "corrupt image record"
    blocked <- builder_freeze_plan(list(entry), tempdir(), make_app = FALSE)
    expect_identical(
      blocked$error_code,
      "invalid_spatial_alignment_diagnostics"
    )
    expect_true(is.na(.builder_plan_alignment_outside_count(
      "corrupt image record"
    )))
  })
})

test_that("spatial image storage and nested image counts freeze exactly", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    record <- list(
      source = list(name = "H&E.png", type = "image/png", size = 4),
      source_uri = "data:image/png;base64,AAAA",
      uri = "data:image/png;base64,AAAA",
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      dx = 0,
      dy = 0,
      scale = 1,
      rotation = 0,
      flip_x = FALSE,
      flip_y = FALSE,
      image_opacity = 0.8,
      point_opacity = 0.85,
      point_size = 5,
      section_id = "fov",
      section_kind = "spatial"
    )
    record$outside <- 0L
    entry$dataset_profile$spatial <- list(sections = "fov")
    entry$settings$images <- list(fov = list(`H&E` = record))
    expect_false(builder_plan_requires_app(list(entry)))

    entry$settings$spatial_image_storage <- "external"
    expect_true(builder_plan_requires_app(list(entry)))

    entry$settings$images <- list()
    expect_false(builder_plan_requires_app(list(entry)))

    trekker_record <- record
    trekker_record$section_id <- "trekker"
    trekker_record$section_kind <- "trekker"
    entry$settings$images <- list(trekker = trekker_record)
    expect_true(builder_plan_requires_app(list(entry)))

    entry$settings$images <- list(fov = list(`H&E` = record))

    blocked <- builder_freeze_plan(list(entry), tempdir(), make_app = FALSE)
    expect_identical(blocked$error_code, "external_images_require_app")

    entry$settings$spatial_image_storage <- "invalid"
    blocked <- builder_freeze_plan(list(entry), tempdir(), make_app = TRUE)
    expect_identical(blocked$error_code, "invalid_spatial_image_storage")

    entry$settings$spatial_image_storage <- "external"
    ready <- builder_freeze_plan(list(entry), tempdir(), make_app = TRUE)
    expect_null(ready$error)
    expect_identical(ready$items[[1L]]$spatial_image_storage, "external")
    expect_identical(ready$items[[1L]]$spatial_alignment$image_count, 1L)
    expect_false(
      "saved_count" %in%
        names(
          ready$items[[1L]]$spatial_alignment
        )
    )

    expect_error(
      builder_plan_requires_app("not a list"),
      "Builder plan entries must be a list.",
      fixed = TRUE
    )

    valid_external_entry <- entry
    duplicate <- structure(list(record, record), names = c("H&E", "H&E"))
    entry$settings$images <- list(fov = duplicate)
    expect_error(builder_plan_requires_app(list(entry)), "unique")
    expect_error(
      builder_plan_requires_app(list(valid_external_entry, entry)),
      "unique"
    )

    invalid_image_entry <- valid_external_entry
    invalid_image_entry$settings$images <- list(fov = "corrupt image record")
    expect_error(
      builder_plan_requires_app(list(invalid_image_entry)),
      "invalid for atomic vectors",
      fixed = TRUE
    )
    expect_error(
      builder_plan_requires_app(list(
        valid_external_entry,
        invalid_image_entry
      )),
      "invalid for atomic vectors",
      fixed = TRUE
    )

    blocked <- builder_freeze_plan(list(entry), tempdir(), make_app = TRUE)
    expect_identical(blocked$error_code, "duplicate_spatial_image_label")
  })
})

test_that("artifact entries bypass editable Spatial image validation", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$load_state <- "artifact_ready"
    entry$snapshot <- NULL
    entry$settings$spatial_image_storage <- "external"
    entry$settings$images <- list(
      fov = list(
        `H&E` = list(
          project_asset = list(path = "spatial-assets/ds1/fov/image.png"),
          bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10)
        )
      )
    )

    expect_false(builder_plan_requires_app(list(entry)))
    preflight <- .builder_plan_preflight_entries(list(entry), make_app = FALSE)
    expect_true(is.list(preflight))
    expect_null(preflight$error_code)
    expect_identical(preflight$labels, "Dataset A")
  })
})

test_that("artifact preflight reuses saved identity without loading source", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    saved <- builder_freeze_plan(list(entry), tempdir(), make_app = FALSE)
    expect_null(saved$error)

    entry$load_state <- "artifact_ready"
    entry$snapshot <- NULL
    entry$dataset_profile$identity <- NULL
    entry$project_artifact <- list(plan_item = saved$items[[1L]])

    preflight <- .builder_plan_preflight_entries(list(entry), make_app = FALSE)

    expect_null(preflight$error_code)
    expect_identical(preflight$labels, "Dataset A")
  })
})

test_that("final included sets own their default values", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$levels$batch <- c("one", "two")
    entry$dataset_profile$metadata$columns$batch <- list(
      name = "batch",
      class = "factor",
      supported = TRUE,
      non_missing = 100L,
      unique_non_missing = 2L
    )
    metadata <- builder_recommend_metadata(
      entry$dataset_profile,
      required = c("nCount_RNA", "nFeature_RNA"),
      dependency_ids = list(
        nCount_RNA = "core.qc.nUMI",
        nFeature_RNA = "core.qc.nGene"
      )
    )
    entry$settings$recommendations$metadata <- metadata
    entry$settings$metadata_policy <- builder_task6_final_metadata_policy(
      metadata,
      list(
        nCount_RNA = "included",
        nFeature_RNA = "included",
        donor_id = "excluded"
      )
    )
    entry$settings$groups <- c("cluster", "batch")
    entry$settings$default_group <- "batch"
    entry$settings$recommendations$groups$included <- c("cluster", "batch")
    plan <- builder_make_plan(list(entry), tempdir(), FALSE)

    expect_null(plan$error)
    expect_true(plan$items[[1L]]$default_group %in% plan$items[[1L]]$groups)
    expect_true(
      plan$items[[1L]]$default_projection %in%
        plan$items[[1L]]$reductions
    )

    invalid_group <- entry
    invalid_group$settings$default_group <- "removed_group"
    expect_identical(
      builder_make_plan(list(invalid_group), tempdir(), FALSE)$error_code,
      "invalid_default_group"
    )

    invalid_projection <- entry
    invalid_projection$settings$default_projection <- "removed_projection"
    expect_identical(
      builder_make_plan(list(invalid_projection), tempdir(), FALSE)$error_code,
      "invalid_default_projection"
    )

    invalid_selection <- entry
    invalid_selection$settings$groups <- c("cluster", "batch", "removed")
    expect_identical(
      builder_make_plan(list(invalid_selection), tempdir(), FALSE)$error_code,
      "invalid_group_selection"
    )
  })
})

test_that("modern drafts fail closed before planning", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    missing_manifest <- builder_task6_entry()
    missing_manifest$dataset_profile$manifest <- NULL
    expect_identical(
      builder_make_plan(list(missing_manifest), tempdir(), FALSE)$error_code,
      "missing_manifest"
    )

    loading <- builder_task6_entry()
    loading$load_state <- "loading"
    expect_identical(
      builder_make_plan(list(loading), tempdir(), FALSE)$error_code,
      "dataset_loading"
    )

    reload <- builder_task6_entry()
    reload$load_state <- "reload_required"
    expect_identical(
      builder_make_plan(list(reload), tempdir(), FALSE)$error_code,
      "dataset_reload_required"
    )
  })
})

test_that("Builder runtimes load dataset state before planning", {
  app <- readLines(
    builder_profile_inst_path("builder", "app.R"),
    warn = FALSE
  )
  session <- readLines(
    builder_profile_inst_path("builder", "worker.R"),
    warn = FALSE
  )

  app_state <- grep('source("state.R", local = TRUE)', app, fixed = TRUE)
  app_plan <- grep('source("plan.R", local = TRUE)', app, fixed = TRUE)
  worker_state <- grep(
    'source(file.path(dir, "state.R"))',
    session,
    fixed = TRUE
  )
  worker_plan <- grep(
    'source(file.path(dir, "plan.R"))',
    session,
    fixed = TRUE
  )

  expect_length(app_state, 1L)
  expect_length(app_plan, 1L)
  expect_lt(app_state, app_plan)
  expect_length(worker_state, 1L)
  expect_length(worker_plan, 1L)
  expect_lt(worker_state, worker_plan)
})

test_that("frozen plans own every reviewed value", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    draft <- builder_task6_entry()
    app_options <- list(show_upload_ui = FALSE)
    prior <- list(entries = list(list(path = "old.crb", md5 = "old-md5")))
    plan <- builder_freeze_plan(
      list(draft),
      tempdir(),
      FALSE,
      revision = 8L,
      app_options = app_options,
      expected_prior_identity = prior
    )

    expect_null(plan$error)
    expect_s3_class(plan, "builder_build_plan")
    expect_true(all(
      c(
        "revision",
        "dataset_order",
        "source_snapshot_identities",
        "metadata_policy",
        "backend_sidecars",
        "analysis_dependency_graph",
        "viewer_page_expectations",
        "viewer_bundle_assets",
        "private_assets",
        "viewer_bundle_asset_claims",
        "private_asset_claims",
        "acknowledgements",
        "app_options",
        "output_release",
        "expected_prior_identity"
      ) %in%
        names(plan)
    ))
    expect_identical(plan$revision, 8L)
    expect_identical(plan$dataset_order, "dataset-a")
    expect_false(
      plan$source_snapshot_identities[["dataset-a"]]$available
    )
    frozen_snapshot_decision <-
      plan$source_snapshot_identities[["dataset-a"]]
    expect_true(plan$items[[1L]]$filename %in% plan$private_assets)
    expect_false(
      plan$items[[1L]]$filename %in% plan$viewer_bundle_assets
    )

    draft$settings$name <- "Changed"
    draft$settings$metadata_policy$included <- "changed"
    draft$snapshot_identity <- builder_task6_snapshot_identity()
    prior$entries[[1L]]$md5 <- "changed"

    expect_identical(plan$items[[1L]]$name, "Dataset A")
    expect_identical(
      plan$metadata_policy[["dataset-a"]]$included,
      c(
        "cell_barcode",
        "cluster",
        "nCount_RNA",
        "nFeature_RNA"
      )
    )
    expect_identical(
      plan$source_snapshot_identities[["dataset-a"]],
      frozen_snapshot_decision
    )
    expect_identical(
      plan$expected_prior_identity$entries[[1L]]$md5,
      "old-md5"
    )
    expect_identical(
      plan$viewer_page_expectations[["dataset-a"]],
      builder_viewer_page_contract(plan$manifests[["dataset-a"]])
    )

    spoofed_app <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE,
      app_options = list(enabled = TRUE)
    )
    expect_identical(spoofed_app$error_code, "invalid_app_options")
    inert_welcome <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE,
      app_options = list(welcome = "Hello")
    )
    expect_identical(inert_welcome$error_code, "invalid_app_options")

    invalid_initial_dataset <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE,
      app_options = list(initial_dataset = "missing")
    )
    expect_identical(
      invalid_initial_dataset$error_code,
      "invalid_app_options"
    )

    automatic <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE
    )
    expect_identical(automatic$app_options$initial_dataset, "dataset-a")
    expect_identical(automatic$app_options$initial_page, "data_info")
    expect_identical(
      automatic$app_options$initial_dataset_mode,
      "automatic"
    )
    explicit_first <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE,
      app_options = list(initial_dataset = "dataset-a")
    )
    expect_identical(
      explicit_first$app_options$initial_dataset_mode,
      "explicit"
    )

    unsafe <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE,
      app_options = list(callback = new.env(parent = emptyenv()))
    )
    expect_identical(unsafe$error_code, "unsafe_reference")

    attributed_reference <- structure(
      list(entries = list()),
      mutable = new.env(parent = emptyenv())
    )
    unsafe_attribute <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      FALSE,
      expected_prior_identity = attributed_reference
    )
    expect_identical(unsafe_attribute$error_code, "unsafe_reference")
  })
})

test_that("final plan validation does not serialize the full frozen plan", {
  freeze <- paste(
    readLines(
      builder_profile_inst_path("builder", "plan", "freeze.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl(".builder_plan_deep_copy(plan)", freeze, fixed = TRUE))
  expect_match(freeze, ".builder_plan_has_reference(plan)", fixed = TRUE)
})

test_that("Review App options are typed, range checked, and frozen", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    options <- list(
      show_upload_ui = TRUE,
      initial_dataset = "dataset-a",
      initial_page = "data_info",
      welcome_message = "Welcome, team!",
      point_size = list(overview_projection_point_size = 6),
      variable_to_compare = FALSE,
      host = "0.0.0.0",
      port = 4242L,
      max_request_size = 512,
      display_mode = "showcase",
      launch_browser = FALSE
    )
    plan <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      make_app = TRUE,
      app_options = options
    )

    expect_null(plan$error)
    expect_identical(
      plan$app_options[names(options)],
      options
    )
    options$welcome_message <- "mutated"
    options$point_size$overview_projection_point_size <- 20
    expect_identical(plan$app_options$welcome_message, "Welcome, team!")
    expect_identical(
      plan$app_options$point_size$overview_projection_point_size,
      6
    )
    wrapped <- builder_make_plan(
      list(builder_task6_entry()),
      tempdir(),
      make_app = TRUE,
      app_options = options
    )
    expect_null(wrapped$error)
    expect_identical(wrapped$app_options$welcome_message, "mutated")

    invalid <- list(
      welcome_message = list(NA_character_, c("a", "b"), 1),
      point_size = list(
        list(overview_projection_point_size = -1),
        list(overview_projection_point_size = 21),
        list(overview_projection_point_size = NULL),
        list(hidden = 4),
        4
      ),
      variable_to_compare = list(NULL, NA, c(TRUE, FALSE), "cluster"),
      host = list("", NA_character_, c("a", "b"), 1),
      port = list(0, 65536, 1.5, NA, "8080"),
      max_request_size = list(0, Inf, NA, "8000"),
      display_mode = list("fullscreen", NA_character_, c("auto", "normal")),
      launch_browser = list(NA, c(TRUE, FALSE), 1),
      initial_page = list("missing", NA_character_, c("data_info", "groups"))
    )
    for (field in names(invalid)) {
      for (value in invalid[[field]]) {
        failure <- builder_freeze_plan(
          list(builder_task6_entry()),
          tempdir(),
          make_app = TRUE,
          app_options = stats::setNames(list(value), field)
        )
        expect_identical(
          failure$error_code,
          "invalid_app_options",
          info = field
        )
      }
    }

    unknown <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      make_app = TRUE,
      app_options = list(hidden_future_option = TRUE)
    )
    expect_identical(unknown$error_code, "invalid_app_options")
  })
})

test_that("Starting page must exist on the initial dataset", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    conditional <- builder_task6_entry(
      full_ir_ready = TRUE,
      hla_tcr_ready = FALSE
    )
    allowed <- builder_freeze_plan(
      list(conditional),
      tempdir(),
      make_app = TRUE,
      app_options = list(initial_page = "immune_repertoire")
    )
    expect_null(allowed$error)
    expect_identical(allowed$app_options$initial_page, "immune_repertoire")

    unavailable <- builder_freeze_plan(
      list(builder_task6_entry()),
      tempdir(),
      make_app = TRUE,
      app_options = list(initial_page = "immune_repertoire")
    )
    expect_identical(unavailable$error_code, "invalid_app_options")
    expect_match(unavailable$error, "starting Viewer page", fixed = TRUE)
  })
})

test_that("BuildPlan freezes exact artifact identities for read-back", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$dataset_profile$identity <- list(
      cells = list(
        ids = c("cell-b", "cell-a"),
        canonical_ids = c("cell-b", "cell-a"),
        count = 2L
      ),
      features = list(
        ids = c("Gene2", "Gene1"),
        canonical_ids = c("Gene2", "Gene1"),
        count = 2L
      )
    )
    entry$levels$cluster <- c("B", "A")
    entry$settings$analyses <- "percent_mt_ribo"
    entry$dataset_profile$spatial <- list(
      section_count = 2L,
      sections = c("slice-b", "slice-a"),
      sections_truncated = FALSE,
      section_names_truncated = FALSE
    )

    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(plan$error)
    expectation <- plan$items[[1L]]$artifact_identity
    expect_identical(expectation$schema_version, 3L)
    expect_identical(
      expectation$cells,
      builder_axis_identity(c("cell-b", "cell-a"))
    )
    expect_identical(
      expectation$features,
      builder_axis_identity(c("Gene2", "Gene1"))
    )
    expect_identical(expectation$group_levels$cluster, c("B", "A"))
    expect_identical(expectation$projections, "umap")
    expect_identical(
      expectation$metadata,
      c(
        "cell_barcode",
        "cluster",
        "nUMI",
        "nGene",
        "percent_mt",
        "percent_ribo"
      )
    )
    expect_identical(expectation$spatial_sections, c("slice-b", "slice-a"))

    entry$dataset_profile$identity$cells$canonical_ids[[1L]] <- "changed"
    entry$levels$cluster[[1L]] <- "changed"
    expect_identical(
      expectation$cells,
      builder_axis_identity(c("cell-b", "cell-a"))
    )
    expect_identical(expectation$group_levels$cluster, c("B", "A"))
  })
})

test_that("BuildPlan reuses compact workspace axis identities", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    cells <- builder_axis_identity(c("cell-b", "cell-a"))
    features <- builder_axis_identity(c("Gene2", "Gene1"))
    entry$dataset_profile$identity <- list(
      cells = list(count = 2L, valid = TRUE, axis_identity = cells),
      features = list(count = 2L, valid = TRUE, axis_identity = features)
    )

    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)

    expect_null(plan$error)
    expect_identical(plan$items[[1L]]$artifact_identity$cells, cells)
    expect_identical(plan$items[[1L]]$artifact_identity$features, features)
  })
})

test_that("BuildPlan rejects missing axis identity data", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$dataset_profile$identity$cells <- list(count = 2L, valid = TRUE)
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)

    expect_identical(plan$error_code, "invalid_artifact_identity")
  })
})

test_that("release manifests select a coherent visible dataset entry", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    absent <- builder_task6_entry(immune_detected = FALSE)
    visible <- builder_task6_entry(full_ir_ready = TRUE)
    visible$id <- "dataset-b"
    visible$settings$name <- "Dataset B"
    plan <- builder_freeze_plan(list(absent, visible), tempdir(), FALSE)

    expect_null(plan$error)
    expect_identical(
      plan$manifest[["immune_repertoire"]]$status,
      "valid"
    )
    expect_identical(
      plan$manifest[["immune_repertoire"]]$disposition,
      "preserved"
    )
    expect_contains(
      plan$manifest[["immune_repertoire"]]$pages,
      "immune_repertoire"
    )
    expect_s3_class(
      builder_content_manifest(unname(plan$manifest)),
      "builder_content_manifest"
    )

    filtered <- builder_task6_entry(full_ir_ready = TRUE)
    filtered$settings$content_dispositions <- list(
      immune_repertoire = "filtered"
    )
    visible_after_filtered <- builder_task6_entry(full_ir_ready = TRUE)
    visible_after_filtered$id <- "dataset-b"
    visible_after_filtered$settings$name <- "Dataset B"
    filtered_plan <- builder_freeze_plan(
      list(filtered, visible_after_filtered),
      tempdir(),
      FALSE
    )
    expect_identical(
      filtered_plan$manifest[["immune_repertoire"]]$disposition,
      "preserved"
    )
    expect_contains(
      filtered_plan$manifest[["immune_repertoire"]]$pages,
      "immune_repertoire"
    )
  })
})
