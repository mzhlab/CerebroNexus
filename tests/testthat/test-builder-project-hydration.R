builder_project_hydration_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  for (file in c("io.R", "project.R", "build.R")) {
    sys.source(
      testthat::test_path("..", "..", "inst", "builder", file),
      envir = runtime
    )
  }
  runtime
}

builder_stage_contract_source_runtime(environment())

test_that("loaded source entries are hydrated before their first store attachment", {
  runtime <- builder_project_hydration_runtime()
  root <- withr::local_tempdir()
  loaded <- list(
    id = "ds1",
    source_id = "ds1",
    output_id = "ds1",
    selector_value = "ds1",
    snapshot = list(path = "/runtime/snapshot", owner_token = "owner"),
    revision = 1L,
    profile = list(),
    settings = list(
      name = "Source default",
      organism = "hg",
      assay = "RNA",
      layer = "data",
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA",
      expression_backend = "embedded",
      included_projections = "umap",
      default_projection = "umap",
      overview_point_size = 5,
      overview_percentage_cells_to_show = 100,
      analyses = character(),
      images = list()
    )
  )
  saved <- loaded
  saved$settings$name <- "Saved project name"
  saved$settings$organism <- "mm"
  saved$settings$included_projections <- c("umap", "pca")
  saved$settings$default_projection <- "pca"
  saved$settings$overview_point_size <- 8
  saved$settings$overview_percentage_cells_to_show <- 60
  saved$settings$group_color_overrides <- list(
    cluster = c(B = "#E76F51")
  )
  saved$settings$included_groups <- c("cluster", "sample")
  saved$settings$default_group <- "sample"
  saved$settings$metadata_policy <- list(
    retained = c("cluster", "sample", "batch"),
    groups = c("cluster", "sample")
  )
  saved$settings$included_trajectories <- list(
    slingshot = list(method = "slingshot", name = "lineage-2")
  )
  saved$settings$default_trajectory <- list(
    method = "slingshot",
    name = "lineage-2"
  )
  saved$settings$palette <- "okabe_ito"
  saved$settings$content_sources <- list(immune_repertoire = "vdj")
  saved$settings$analyses <- c("most_expressed", "marker_genes")
  saved$acknowledgements <- "metadata"
  record <- runtime$builder_project_dataset_record(
    saved,
    source = list(kind = "example", example = "all_content"),
    checked = TRUE,
    root = root
  )

  hydrated <- runtime$builder_project_hydrate_loaded_entry(loaded, record, root)

  expect_identical(hydrated$settings, saved$settings)
  expect_identical(hydrated$acknowledgements, "metadata")
  expect_identical(hydrated$snapshot, loaded$snapshot)
  expect_identical(hydrated$source_id, "ds1")
  expect_gt(hydrated$revision, loaded$revision)
  expect_true(runtime$builder_project_entry_hydrated_from(hydrated, record))

  another_record <- record
  another_record$configuration$digest <- "another-configuration"
  expect_false(runtime$builder_project_entry_hydrated_from(
    hydrated,
    another_record
  ))

  changed <- runtime$builder_project_invalidate_entry_hydration(hydrated)
  expect_null(changed$project_hydration)
  expect_false(runtime$builder_project_entry_hydrated_from(changed, record))
})

test_that("pending project hydration isolates a corrupt dataset", {
  runtime <- builder_project_hydration_runtime()
  root <- withr::local_tempdir()
  loaded <- function(id) {
    list(
      id = id,
      load_state = "loaded",
      revision = 1L,
      snapshot = list(path = paste0("/runtime/", id)),
      settings = list(name = paste("Source", id))
    )
  }
  saved <- loaded("good")
  saved$settings$name <- "Saved good"
  good <- runtime$builder_project_dataset_record(
    saved,
    source = list(kind = "example", example = "all_content"),
    checked = TRUE,
    root = root
  )
  corrupt <- runtime$builder_project_dataset_record(
    loaded("corrupt"),
    source = list(kind = "example", example = "all_content"),
    root = root
  )
  writeLines(
    "not serialized JSON",
    runtime$builder_project_resolve_path(
      corrupt$configuration$path,
      root,
      "managed"
    )
  )

  result <- runtime$builder_project_hydrate_pending_entries(
    entries = list(loaded("good"), loaded("corrupt")),
    pending = list(good = good, corrupt = corrupt),
    root = root
  )

  expect_identical(
    vapply(result$entries, `[[`, character(1), "id"),
    "good"
  )
  expect_identical(result$entries[[1L]]$settings$name, "Saved good")
  expect_identical(names(result$restored), "good")
  expect_identical(names(result$failures), "corrupt")
  expect_match(result$failures$corrupt$message, "configuration")
  expect_identical(result$failures$corrupt$entry$id, "corrupt")
  expect_length(result$pending, 0L)
})

test_that("configuration identity is stable across Spatial asset hydration", {
  runtime <- builder_project_hydration_runtime()
  source_uri <- paste0(
    "data:image/png;base64,",
    base64enc::base64encode(charToRaw("same-source-image"))
  )
  transformed_uri <- paste0(
    "data:image/png;base64,",
    base64enc::base64encode(charToRaw("derived-rotated-image"))
  )
  image <- list(
    source = list(name = "H&E"),
    source_uri = source_uri,
    uri = transformed_uri,
    base_bounds = list(xmin = 0, xmax = 100, ymin = 0, ymax = 80),
    bounds = list(xmin = 306, xmax = 406, ymin = -25, ymax = 55),
    dx = 306,
    dy = -25,
    scale = 1.42,
    rotation = -17,
    flip_x = TRUE,
    flip_y = TRUE,
    image_opacity = 0.55,
    point_opacity = 0.65,
    point_size = 8,
    section_id = "fov-a",
    section_kind = "spatial"
  )
  before <- list(
    id = "ds1",
    settings = list(
      images = list(`fov-a` = list(`H&E` = image))
    )
  )
  after <- before
  after$settings$images[["fov-a"]][["H&E"]]$uri <- source_uri

  expect_identical(
    runtime$builder_project_configuration_digest(before),
    runtime$builder_project_configuration_digest(after)
  )

  changed <- after
  changed$settings$images[["fov-a"]][["H&E"]]$source_uri <- paste0(
    "data:image/png;base64,",
    base64enc::base64encode(charToRaw("different-source-image"))
  )
  expect_false(identical(
    runtime$builder_project_configuration_digest(before),
    runtime$builder_project_configuration_digest(changed)
  ))
})

test_that("Spatial UI model carries every saved image control", {
  image <- list(
    source = list(name = "H&E"),
    source_uri = "data:image/png;base64,AA==",
    uri = "data:image/png;base64,AA==",
    base_bounds = list(xmin = 0, xmax = 100, ymin = 0, ymax = 80),
    bounds = list(xmin = 306, xmax = 448, ymin = -25, ymax = 89),
    dx = 306,
    dy = -25,
    scale = 1.42,
    rotation = -17,
    flip_x = TRUE,
    flip_y = TRUE,
    image_opacity = 0.55,
    point_opacity = 0.65,
    point_size = 8,
    section_id = "fov-a",
    section_kind = "spatial"
  )
  dapi <- image
  dapi$source$name <- "DAPI"
  dapi$rotation <- 23
  model <- builder_enhance_model(
    id = "ds1",
    profile = list(images = "fov-a", extras = list()),
    state = list(manifest = list()),
    settings = list(
      tables = list(),
      images = list(`fov-a` = list(`H&E` = image, DAPI = dapi)),
      spatial_coordinate_transforms = list(
        `fov-a` = list(rotation_degrees = 32.8, scale = 1)
      ),
      spatial_image_storage = "external"
    ),
    modules = list(),
    active_section = "fov-a",
    active_image = "DAPI"
  )
  controls <- model$attachments$histology$controls

  expect_identical(model$attachments$histology$active_image, "DAPI")
  expect_identical(controls$dx, 306)
  expect_identical(controls$dy, -25)
  expect_identical(controls$scale, 1.42)
  expect_identical(controls$rotation, 23)
  expect_true(controls$flip_x)
  expect_true(controls$flip_y)
  expect_identical(controls$image_opacity, 0.55)
  expect_identical(controls$point_opacity, 0.65)
  expect_identical(controls$point_size, 8)
  expect_lte(controls$dx_min, 306)
  expect_gte(controls$dx_max, 306)
  expect_lte(controls$dy_min, -25)
  expect_gte(controls$dy_max, -25)

  html <- paste(
    as.character(builder_spatial_alignment_ui(
      "enhance",
      model$attachments$histology
    )),
    collapse = ""
  )
  expect_match(html, 'id="enhance-img_dx"[^>]+data-from="306"', perl = TRUE)
  expect_match(html, 'id="enhance-img_dy"[^>]+data-from="-25"', perl = TRUE)
  expect_match(html, 'id="enhance-img_scale"[^>]+data-from="1.42"', perl = TRUE)
  expect_match(html, 'id="enhance-img_rotate"[^>]+data-from="23"', perl = TRUE)
  expect_match(html, 'id="enhance-image_flip_x"[^>]+checked', perl = TRUE)
  expect_match(html, 'id="enhance-image_flip_y"[^>]+checked', perl = TRUE)
})

test_that("Spatial control ranges retain saved offsets before preview bounds arrive", {
  record <- list(
    base_bounds = list(xmin = 0, xmax = 100, ymin = 0, ymax = 80),
    bounds = list(xmin = 306, xmax = 448, ymin = -25, ymax = 89),
    dx = 306,
    dy = -25
  )

  pending <- builder_alignment_control_ranges(record, bounds = NULL)
  expect_lte(pending$dx$min, 306)
  expect_gte(pending$dx$max, 306)
  expect_lte(pending$dy$min, -25)
  expect_gte(pending$dy$max, -25)

  ready <- builder_alignment_control_ranges(
    record,
    bounds = list(xmin = 0, xmax = 500, ymin = -100, ymax = 100)
  )
  expect_identical(ready$dx$min, -500)
  expect_identical(ready$dx$max, 500)
  expect_identical(ready$dy$min, -200)
  expect_identical(ready$dy$max, 200)
})

test_that("schema v1 projects migrate durable project preferences explicitly", {
  runtime <- builder_project_hydration_runtime()
  path <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(
    list(
      schema_version = 1L,
      project = list(id = "project-safe", revision = 2L),
      datasets = list(),
      last_ui = list(stage = "configure", selected_dataset = "ds1")
    ),
    path,
    auto_unbox = TRUE,
    null = "null"
  )

  migrated <- runtime$builder_project_read(path)

  expect_identical(migrated$schema_version, 3L)
  expect_identical(migrated$migrated_from_schema, 1L)
  expect_false(migrated$configuration$build_mode)
  expect_false(migrated$configuration$auth_enabled)
  expect_null(migrated$configuration$review_options)
  expect_identical(migrated$last_ui$stage, "configure")
})

test_that("schema v1 migration canonicalizes record identity without dirtying", {
  runtime <- builder_project_hydration_runtime()
  path <- withr::local_tempfile(fileext = ".json")
  entry <- builder_minimal_entry("ds1", "Saved dataset")
  canonical <- runtime$builder_project_configuration_digest(entry)
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(kind = "example", example = "all_content"),
    checked = TRUE,
    root = dirname(path)
  )
  record$configuration <- list(
    revision = as.integer(entry$revision %||% 0L),
    digest = "legacy-digest",
    checked = TRUE,
    payload = jsonlite::serializeJSON(entry, digits = NA, pretty = FALSE)
  )
  record$configuration$digest <- "legacy-digest"
  record$artifact <- list(
    status = "ready",
    built_from_configuration = "legacy-digest"
  )
  jsonlite::write_json(
    list(
      schema_version = 1L,
      project = list(id = "project-safe", revision = 2L),
      datasets = list(record),
      last_ui = list(stage = "configure", selected_dataset = "ds1")
    ),
    path,
    auto_unbox = TRUE,
    null = "null"
  )

  migrated <- runtime$builder_project_read(path)

  migrated_record <- migrated$datasets[[1L]]
  expect_identical(migrated$datasets[[1L]]$configuration$digest, canonical)
  expect_null(migrated_record$configuration$payload)
  expect_true(file.exists(file.path(
    dirname(path),
    migrated_record$configuration$path
  )))
  expect_null(migrated_record$cache)
  expect_identical(
    migrated_record$artifact$built_from_configuration,
    canonical
  )
  expect_false(runtime$builder_project_live_dirty(
    list(entry),
    "ds1",
    migrated
  ))
})

test_that("project preferences retain only validated non-secret values", {
  runtime <- builder_project_hydration_runtime()
  options <- list(
    welcome_message = "Saved welcome",
    initial_page = "overview",
    point_size = 7,
    variable_to_compare = TRUE,
    host = "0.0.0.0",
    port = 9090L,
    max_request_size = 1234,
    display_mode = "showcase",
    launch_browser = FALSE,
    show_upload_ui = TRUE,
    password = "must-not-be-saved"
  )

  saved <- runtime$builder_project_configuration(
    review_options = options,
    build_mode = TRUE,
    auth_enabled = TRUE,
    initial_dataset = "ds2"
  )

  expect_identical(saved$review_options$welcome_message, "Saved welcome")
  expect_identical(saved$review_options$initial_page, "overview")
  expect_identical(saved$review_options$port, 9090L)
  expect_true(saved$build_mode)
  expect_true(saved$auth_enabled)
  expect_identical(saved$initial_dataset, "ds2")
  expect_false(any(
    c(
      "accounts",
      "password",
      "token",
      "username",
      "account_count"
    ) %in%
      names(saved)
  ))
})

test_that("schema v3 preferences survive a manifest write and read", {
  runtime <- builder_project_hydration_runtime()
  root <- withr::local_tempdir()
  manifest <- runtime$builder_project_new_manifest(root, "Round trip")
  manifest$configuration <- runtime$builder_project_configuration(
    review_options = list(
      welcome_message = "Saved welcome",
      initial_page = "overview",
      point_size = 7,
      variable_to_compare = TRUE,
      host = "0.0.0.0",
      port = 9090L,
      max_request_size = 1234,
      display_mode = "showcase",
      launch_browser = FALSE,
      show_upload_ui = TRUE
    ),
    build_mode = TRUE,
    auth_enabled = TRUE,
    initial_dataset = "ds2"
  )
  written <- runtime$builder_project_write(manifest, root)
  restored <- runtime$builder_project_read(written$path)

  expect_identical(restored$schema_version, 3L)
  expect_identical(restored$configuration, written$manifest$configuration)
  expect_null(restored$migrated_from_schema)
})

test_that("schema v3 excludes source-derived profiles from project storage", {
  runtime <- builder_project_hydration_runtime()
  root <- withr::local_tempdir()
  entry <- builder_minimal_entry("ds1", "Dataset one")
  entry$profile <- list(
    n_cells = 123L,
    n_genes = 45L,
    large_derived_value = rep("profile-only", 1000L)
  )
  entry$dataset_profile <- list(
    assays = list(large_derived_value = rep(1, 1000L))
  )
  entry$levels <- list(cluster = c("A", "B"))

  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(
      kind = "managed",
      path = "sources/ds1/source.rds",
      fingerprint = list(md5 = "source-md5")
    ),
    checked = TRUE,
    root = root
  )

  expect_identical(record$configuration$schema_version, 1L)
  expect_true(file.exists(file.path(root, record$configuration$path)))
  expect_null(record$cache)
  expect_null(record$configuration$payload)
  expect_null(record$configuration$legacy_payload)

  restored <- runtime$builder_project_restore_entry(record, root)
  expect_identical(restored$settings, entry$settings)
  expect_null(restored$profile$large_derived_value)
})

test_that("last UI restoration uses safe dataset and workflow fallbacks", {
  runtime <- builder_project_hydration_runtime()

  restored <- runtime$builder_project_last_ui_target(
    list(stage = "build", selected_dataset = "ds2"),
    available_ids = c("ds1", "ds2")
  )
  expect_identical(restored$selected_dataset, "ds2")
  expect_identical(restored$stage, "configure")

  spatial <- runtime$builder_project_last_ui_target(
    list(
      stage = "configure",
      selected_dataset = "ds2",
      spatial = list(dataset = "ds2", section = "fov-b", image = "DAPI")
    ),
    available_ids = c("ds1", "ds2")
  )
  expect_identical(
    spatial$spatial,
    list(dataset = "ds2", section = "fov-b", image = "DAPI")
  )

  fallback <- runtime$builder_project_last_ui_target(
    list(
      stage = "review",
      selected_dataset = "skipped",
      spatial = list(dataset = "skipped", section = "fov-z", image = "old")
    ),
    available_ids = "ds1"
  )
  expect_identical(fallback$selected_dataset, "ds1")
  expect_identical(fallback$stage, "configure")
  expect_null(fallback$spatial)

  empty <- runtime$builder_project_last_ui_target(
    list(stage = "review", selected_dataset = "ds1"),
    available_ids = character()
  )
  expect_null(empty$selected_dataset)
  expect_identical(empty$stage, "upload")
})

test_that("Spatial project UI restoration selects the saved FOV and image", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(testthat::test_path("..", "..", "inst", "builder"))
  sys.source("app.R", envir = app_env)
  record <- function(section, label) {
    app_env$builder_alignment_record(
      source = list(name = label),
      source_uri = "data:image/png;base64,AA==",
      uri = "data:image/png;base64,AA==",
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      section = list(id = section, kind = "spatial")
    )
  }
  entry <- list(
    id = "ds1",
    snapshot = list(
      path = "/runtime/ds1",
      owner_token = "owner",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = c("fov-a", "fov-b"), extras = list()),
    settings = list(
      name = "Dataset 1",
      images = list(
        `fov-a` = list(`H&E` = record("fov-a", "H&E")),
        `fov-b` = list(
          `H&E` = record("fov-b", "H&E"),
          DAPI = record("fov-b", "DAPI")
        )
      ),
      spatial_coordinate_transforms = list(),
      default_group = "cluster",
      default_projection = "umap"
    )
  )
  current <- shiny::reactiveVal("ds1")

  shiny::testServer(
    function(input, output, session) {
      alignment <- app_env$builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) entry,
        entries = shiny::reactiveVal(list(entry)),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(entry, images) invisible(entry),
        alignment_preview = shiny::reactiveVal(NULL),
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      expect_identical(alignment$active_section(), "fov-a")
      expect_identical(alignment$active_image(), "H&E")

      expect_true(alignment$restore_project_selection(list(
        dataset = "ds1",
        section = "fov-b",
        image = "DAPI"
      )))
      session$flushReact()

      expect_identical(alignment$active_section(), "fov-b")
      expect_identical(alignment$active_image(), "DAPI")

      session$setInputs(`enhance-active_section` = "fov-a")
      session$flushReact()
      expect_identical(alignment$active_section(), "fov-a")
      current(NULL)
      session$flushReact()
      current("ds1")
      session$flushReact()
      expect_identical(alignment$active_section(), "fov-a")
      expect_identical(alignment$active_image(), "H&E")
    }
  )
})

test_that("the assembled Builder server registers pre-store project hydration", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(testthat::test_path("..", "..", "inst", "builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this hydration test.")
  }
  root <- withr::local_tempdir()
  source_dir <- file.path(root, "sources", "ds1")
  dir.create(source_dir, recursive = TRUE)
  source_path <- file.path(source_dir, "input.rds")
  writeBin(charToRaw("source"), source_path)
  loaded <- builder_minimal_entry("ds1", "Source default")
  loaded$revision <- 1L
  loaded$snapshot <- list(
    path = "/runtime/new",
    owner_token = "new-owner",
    object_md5 = "new-object-md5"
  )
  saved <- loaded
  saved$settings$name <- "Saved project name"
  saved$settings$organism <- "mm"
  saved$settings$overview_point_size <- 9
  saved$settings$spatial_coordinate_transforms <- list(
    fov_a = list(schema_version = 1L, rotation_degrees = 66.9, scale = 1)
  )
  saved$settings$spatial_point_appearance <- list(
    fov_a = list(point_opacity = 0.7, point_size = 6)
  )
  record <- app_env$builder_project_dataset_record(
    saved,
    source = list(kind = "managed", path = "sources/ds1/input.rds"),
    checked = TRUE,
    root = root
  )

  shiny::testServer(app_env$server, {
    builder_project_pending_entries(list(ds1 = record))
    builder_project(list(root = root, manifest = list()))

    hydrated <- finalize_loaded_entry(loaded)

    expect_identical(hydrated$settings, saved$settings)
    expect_identical(hydrated$snapshot, loaded$snapshot)
    expect_identical(
      hydrated$settings$spatial_coordinate_transforms$fov_a$rotation_degrees,
      66.9
    )
    expect_identical(
      hydrated$settings$spatial_point_appearance$fov_a,
      list(point_opacity = 0.7, point_size = 6)
    )

    artifact <- saved
    artifact$load_state <- "artifact_ready"
    artifact$snapshot <- NULL
    artifact$project_artifact <- list(
      status = "ready",
      path = "artifacts/ds1.crb"
    )
    use_state_only_fixture(list(artifact))
    dataset_check_marks(stats::setNames(
      app_env$builder_project_check_identity(artifact),
      "ds1"
    ))

    prepared <- builder_prepare_loaded_entry_attachment(loaded)
    expect_length(prepared$state$datasets, 1L)
    expect_identical(prepared$state$datasets[[1L]]$id, "ds1")
    expect_identical(prepared$state$current_dataset, "ds1")
    expect_identical(prepared$entry$settings, saved$settings)

    store(prepared$state)
    expect_true(builder_project_mark_restored_entry(prepared$entry))
    session$flushReact()
    expect_identical(checked_dataset_ids(), "ds1")

    use_state_only_fixture(list(hydrated))
    edited <- hydrated
    edited$settings$overview_point_size <- 10
    expect_true(replace_entry(edited, internal = TRUE))
    expect_null(isolate(store())$datasets[[1L]]$project_hydration)

    invalid_saved <- saved
    invalid_saved$settings$spatial_image_storage <- "forged"
    invalid_record <- app_env$builder_project_dataset_record(
      invalid_saved,
      source = list(kind = "managed", path = "sources/ds1/input.rds"),
      checked = TRUE,
      root = root
    )
    builder_project_pending_entries(list(ds1 = invalid_record))
    use_state_only_fixture(list())
    expect_error(
      builder_prepare_loaded_entry_attachment(loaded),
      class = "builder_state_error"
    )
  })
})

test_that("failed source resume preserves the checked artifact entry", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(testthat::test_path("..", "..", "inst", "builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this resume test.")
  }
  root <- withr::local_tempdir()
  source_dir <- file.path(root, "sources", "ds1")
  dir.create(source_dir, recursive = TRUE)
  source_path <- file.path(source_dir, "input.rds")
  writeBin(charToRaw("source"), source_path)
  saved <- builder_minimal_entry("ds1", "Saved dataset")
  record <- app_env$builder_project_dataset_record(
    saved,
    source = list(
      kind = "managed",
      path = "sources/ds1/input.rds",
      filename = "input.rds"
    ),
    checked = TRUE,
    root = root
  )
  artifact <- saved
  artifact$load_state <- "artifact_ready"
  artifact$snapshot <- NULL
  artifact$project_artifact <- list(
    status = "ready",
    path = "artifacts/ds1.crb"
  )

  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(artifact))
    current("ds1")
    mark <- app_env$builder_project_check_identity(artifact)
    dataset_check_marks(stats::setNames(mark, "ds1"))
    builder_project(list(
      root = root,
      manifest = list(datasets = list(record))
    ))
    builder_project_pending_entries(list())
    session$flushReact()

    session$setInputs(project_resume_current_source = 1)
    session$flushReact()

    expect_length(sets(), 1L)
    expect_identical(sets()[[1L]]$id, "ds1")
    expect_identical(sets()[[1L]]$load_state, "artifact_ready")
    expect_identical(isolate(dataset_check_marks())[["ds1"]], mark)
    expect_null(isolate(builder_project_pending_entries())[["ds1"]])
  })
})

test_that("source resume catches start failures and restores pending state", {
  source <- paste(
    readLines(
      testthat::test_path("..", "..", "inst", "builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  observer <- sub(
    ".*observeEvent\\(input\\$project_resume_current_source, \\{",
    "",
    source
  )
  observer <- sub(
    "\\n\\}\\)\\n\\nbuilder_project_crb_request_from_input.*",
    "",
    observer
  )

  expect_match(observer, "started <- tryCatch(", fixed = TRUE)
  expect_match(observer, "conditionMessage(started)", fixed = TRUE)
  expect_match(observer, "pending[[id]] <- previous_pending", fixed = TRUE)
  expect_match(
    observer,
    'record$runtime_restore_target <- "configure"',
    fixed = TRUE
  )
})

test_that("an explicitly resumed source stays in Configure", {
  source <- paste(
    readLines(
      testthat::test_path("..", "..", "inst", "builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    source,
    'identical(record$runtime_restore_target %||% NULL, "configure")',
    fixed = TRUE
  )
  expect_match(
    source,
    'restored_last_ui$stage <- "configure"',
    fixed = TRUE
  )
})

test_that("failed pre-store hydration releases its unattached snapshot", {
  source <- paste(
    readLines(
      testthat::test_path("..", "..", "inst", "builder", "server", "imports.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  branch <- sub(
    ".*if \\(inherits\\(finalized_entry, \"try-error\"\\)\\) \\{",
    "",
    source
  )
  branch <- sub("\\n    entry <- finalized_entry.*", "", branch)

  expect_match(
    branch,
    ".builder_snapshot_release(value$snapshot)",
    fixed = TRUE
  )
  expect_match(
    branch,
    "builder_protocol_acknowledge(protocol(), request$request_id)",
    fixed = TRUE
  )
  expect_match(branch, "builder_import_progress_remove", fixed = TRUE)
})

test_that("failed pre-store load acknowledgement releases the persistent queue", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    testthat::test_path("..", "..", "inst", "builder", "worker.R"),
    envir = runtime
  )
  protocol <- runtime$builder_request_protocol("worker-1")
  protocol <- runtime$builder_enqueue(
    protocol,
    runtime$builder_command("load", "ds1", payload = list(path = "ds1.rds"))
  )
  protocol <- runtime$builder_enqueue(
    protocol,
    runtime$builder_command("load", "ds2", payload = list(path = "ds2.rds"))
  )

  dispatched <- runtime$builder_protocol_dispatch(protocol)
  request <- dispatched$request
  expect_identical(request$kind, "load")
  expect_true(isTRUE(request$persistent))

  completed <- runtime$builder_protocol_complete(
    dispatched$protocol,
    runtime$builder_worker_response(request, value = list(snapshot = "invalid"))
  )
  expect_length(completed$protocol$awaiting_ack, 1L)

  acknowledged <- runtime$builder_protocol_acknowledge(
    completed$protocol,
    request$request_id
  )
  expect_length(acknowledged$awaiting_ack, 0L)

  next_dispatched <- runtime$builder_protocol_dispatch(acknowledged)
  expect_identical(next_dispatched$request$dataset, "ds2")
})
