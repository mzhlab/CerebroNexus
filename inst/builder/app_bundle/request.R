.builder_app_plan_contract <- function(plan, context = "App assembly") {
  invalid <- function() {
    stop(
      context,
      paste0(
        " requires an inert, reference-free frozen contract-v1 ",
        "BuildPlan."
      ),
      call. = FALSE
    )
  }
  if (
    !identical(typeof(plan), "list") ||
      !identical(
        attr(plan, "class", exact = TRUE),
        c("builder_build_plan", "list")
      )
  ) {
    invalid()
  }
  raw_items <- .subset2(plan, "items")
  if (
    !is.list(raw_items) ||
      any(vapply(
        raw_items,
        function(item) {
          !is.list(item) || !is.null(attr(item, "class", exact = TRUE))
        },
        logical(1)
      ))
  ) {
    invalid()
  }
  app_items <- lapply(raw_items, function(item) {
    list(
      id = .subset2(item, "id"),
      name = .subset2(item, "name"),
      filename = .subset2(item, "filename"),
      colors = .subset2(item, "colors"),
      default_projection = .subset2(item, "default_projection"),
      default_trajectory = .subset2(item, "default_trajectory"),
      overview_point_size = .subset2(item, "overview_point_size"),
      overview_point_opacity = .subset2(item, "overview_point_opacity"),
      overview_percentage_cells_to_show = .subset2(
        item,
        "overview_percentage_cells_to_show"
      ),
      expression_backend = .subset2(item, "expression_backend"),
      sidecars = .subset2(item, "sidecars"),
      spatial_image_storage = .subset2(item, "spatial_image_storage") %||%
        "embedded",
      external_images = .subset2(item, "external_images") %||% list(),
      external_image_settings = .subset2(item, "external_image_settings") %||%
        list()
    )
  })
  plan <- list(
    app_contract_version = .subset2(plan, "app_contract_version"),
    make_app = .subset2(plan, "make_app"),
    dataset_order = .subset2(plan, "dataset_order"),
    items = app_items,
    app_options = .subset2(plan, "app_options"),
    app_auth = .subset2(plan, "app_auth")
  )
  if (.builder_app_has_reference(plan)) {
    invalid()
  }
  .builder_app_plain_value(plan)
}

.builder_app_capture_spatial_identities <- function(images) {
  identities <- lapply(names(images), function(dataset) {
    sections <- lapply(names(images[[dataset]]), function(section) {
      values <- lapply(names(images[[dataset]][[section]]), function(label) {
        .builder_app_capture_file_identity(
          images[[dataset]][[section]][[label]]$path,
          paste(dataset, section, label, sep = "/")
        )
      })
      names(values) <- names(images[[dataset]][[section]])
      values
    })
    names(sections) <- names(images[[dataset]])
    sections
  })
  names(identities) <- names(images)
  identities
}

builder_app_bundle_request <- function(plan, built, labels) {
  plan <- .builder_app_plan_contract(plan)
  if (
    !identical(plan[["app_contract_version"]], 1L) ||
      !isTRUE(plan[["make_app"]])
  ) {
    stop(
      "App assembly requires a contract-v1 BuildPlan with App output enabled.",
      call. = FALSE
    )
  }
  order <- plan$dataset_order
  items <- plan$items
  if (
    !is.character(order) ||
      !length(order) ||
      anyNA(order) ||
      any(!nzchar(order)) ||
      anyDuplicated(order) ||
      !is.list(items) ||
      length(items) != length(order)
  ) {
    stop("BuildPlan dataset order is invalid.", call. = FALSE)
  }
  if (
    .builder_app_has_reference(built) ||
      .builder_app_has_reference(labels)
  ) {
    stop("Verified CRB labels must be inert values.", call. = FALSE)
  }
  built <- .builder_app_plain_value(built)
  labels <- .builder_app_plain_value(labels)
  item_ids <- vapply(items, `[[`, character(1), "id")
  item_labels <- vapply(items, `[[`, character(1), "name")
  filenames <- vapply(items, `[[`, character(1), "filename")
  if (
    !identical(item_ids, order) ||
      anyDuplicated(item_labels) ||
      !is.character(labels) ||
      !identical(unname(labels), item_labels) ||
      !is.character(built) ||
      length(built) != length(items) ||
      !identical(names(built), item_labels) ||
      anyDuplicated(built)
  ) {
    stop("Verified CRB labels do not match BuildPlan.", call. = FALSE)
  }
  valid_files <- vapply(
    built,
    function(path) {
      length(path) == 1L &&
        !is.na(path) &&
        nzchar(path) &&
        file.exists(path) &&
        !dir.exists(path) &&
        !.builder_app_is_link(path)
    },
    logical(1)
  )
  if (!all(valid_files)) {
    stop("A verified staged CRB is missing or invalid.", call. = FALSE)
  }
  resolved <- vapply(
    built,
    normalizePath,
    character(1),
    winslash = "/",
    mustWork = TRUE
  )
  stage <- dirname(resolved[[1L]])
  if (
    !all(dirname(resolved) == stage) ||
      !identical(unname(basename(resolved)), filenames)
  ) {
    stop(
      "Verified CRBs must be exact files inside one assigned stage.",
      call. = FALSE
    )
  }
  options <- plan$app_options
  app_auth <- plan$app_auth
  if (
    is.list(options) &&
      identical(options$initial_dataset_mode, "automatic") &&
      !identical(options$initial_dataset, order[[1L]])
  ) {
    stop(
      "An automatic initial dataset must be the first ordered dataset.",
      call. = FALSE
    )
  }
  if (
    .builder_app_has_reference(options) ||
      !.builder_app_options_valid(options, order)
  ) {
    stop("Frozen generated-App options are invalid.", call. = FALSE)
  }
  if (!.builder_app_auth_summary_valid(app_auth)) {
    stop("Frozen generated-App login settings are invalid.", call. = FALSE)
  }
  if (
    identical(options$initial_dataset_mode, "automatic") &&
      !identical(options$initial_dataset, order[[1L]])
  ) {
    stop(
      "An automatic initial dataset must be the first ordered dataset.",
      call. = FALSE
    )
  }
  initial_index <- match(options$initial_dataset, order)
  colors <- lapply(items, `[[`, "colors")
  names(colors) <- item_labels
  viewer_content <- .builder_app_viewer_content(
    items,
    item_labels,
    options$point_size
  )
  backend_entries <- lapply(items, .builder_app_backend_entry)
  names(backend_entries) <- paste0("private-data/", filenames)
  crb_identities <- lapply(
    seq_along(resolved),
    function(index) {
      .builder_app_capture_file_identity(
        resolved[[index]],
        item_labels[[index]]
      )
    }
  )
  names(crb_identities) <- item_labels
  backend_plan <- list(schema_version = 1L, entries = backend_entries)
  backend_identities <- lapply(seq_along(resolved), function(index) {
    relative_crb <- names(backend_entries)[[index]]
    .builder_app_capture_backend_identity(
      backend_entries[[relative_crb]],
      resolved[[index]]
    )
  })
  names(backend_identities) <- names(backend_entries)
  content_identities <- .builder_app_content_identities(
    crb_identities,
    backend_identities,
    backend_plan
  )
  storage_valid <- vapply(
    items,
    function(item) {
      storage <- item$spatial_image_storage
      images <- item$external_images
      settings <- item$external_image_settings
      is.character(storage) &&
        length(storage) == 1L &&
        !is.na(storage) &&
        storage %in% c("embedded", "external") &&
        is.list(images) &&
        !is.object(images) &&
        is.list(settings) &&
        !is.object(settings) &&
        identical(names(images), names(settings)) &&
        (identical(storage, "external") ||
          (!length(images) && !length(settings)))
    },
    logical(1)
  )
  if (!all(storage_valid)) {
    stop("BuildPlan external spatial image contract is invalid.", call. = FALSE)
  }
  external <- vapply(
    items,
    function(item) identical(item$spatial_image_storage, "external"),
    logical(1)
  )
  spatial_images <- lapply(items[external], `[[`, "external_images")
  spatial_image_settings <- lapply(
    items[external],
    `[[`,
    "external_image_settings"
  )
  names(spatial_images) <- item_labels[external]
  names(spatial_image_settings) <- item_labels[external]
  populated <- vapply(spatial_images, length, integer(1)) > 0L
  spatial_images <- spatial_images[populated]
  spatial_image_settings <- spatial_image_settings[populated]
  spatial_image_identities <- .builder_app_capture_spatial_identities(
    spatial_images
  )

  request <- structure(
    list(
      contract_version = 1L,
      stage = stage,
      cerebro_data = stats::setNames(unname(resolved), item_labels),
      crb_identities = crb_identities,
      selector_order = item_labels,
      initial_dataset = item_labels[[initial_index]],
      initial_dataset_mode = options$initial_dataset_mode,
      initial_page = options$initial_page,
      show_upload_ui = options$show_upload_ui,
      welcome_message = options$welcome_message,
      point_size = options$point_size,
      viewer_content = viewer_content,
      variable_to_compare = options$variable_to_compare,
      host = options$host,
      port = as.integer(options$port),
      max_request_size = options$max_request_size,
      display_mode = options$display_mode,
      launch_browser = options$launch_browser,
      auth = .builder_app_auth_request(app_auth),
      colors = colors,
      crb_pick_smallest_file = FALSE,
      backend_plan = backend_plan,
      backend_identities = backend_identities,
      content_identities = content_identities,
      spatial_images = spatial_images,
      spatial_image_settings = spatial_image_settings,
      spatial_image_identities = spatial_image_identities
    ),
    class = c("builder_app_bundle_request", "list")
  )
  .builder_app_validate_request(request)
  request
}
