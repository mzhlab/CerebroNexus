.builder_plan_app_auth_valid <- function(value, make_app) {
  expected <- c("enabled", "account_count", "timeout_minutes")
  is.list(value) &&
    !is.object(value) &&
    identical(names(value), expected) &&
    is.logical(value$enabled) &&
    length(value$enabled) == 1L &&
    !is.na(value$enabled) &&
    is.integer(value$account_count) &&
    length(value$account_count) == 1L &&
    !is.na(value$account_count) &&
    value$account_count >= 0L &&
    value$account_count <= 50L &&
    identical(value$timeout_minutes, 15L) &&
    (!isTRUE(value$enabled) || isTRUE(make_app)) &&
    if (isTRUE(value$enabled)) {
      value$account_count >= 1L
    } else {
      identical(value$account_count, 0L)
    }
}

.builder_plan_manifest_with_tables <- function(manifest, tables) {
  tables <- tables %||% list()
  if (!length(tables)) {
    return(manifest)
  }
  table_names <- names(tables)
  if (is.null(table_names)) {
    table_names <- rep.int("", length(tables))
  }
  labels <- vapply(
    seq_along(tables),
    function(index) {
      table <- tables[[index]]
      candidates <- trimws(as.character(c(
        table$name %||% character(),
        table$display_name %||% character(),
        table$sheet_name %||% character(),
        table_names[[index]]
      )))
      candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
      if (length(candidates)) candidates[[1L]] else paste("Table", index)
    },
    character(1)
  )
  workbooks <- unique(vapply(
    tables,
    function(table) {
      values <- trimws(as.character(c(
        table$workbook_name %||% character(),
        table$file_name %||% character()
      )))
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values)) values[[1L]] else ""
    },
    character(1)
  ))
  workbooks <- workbooks[nzchar(workbooks)]

  existing <- manifest[["extra_material"]] %||% NULL
  evidence <- existing$evidence %||% list()
  normalized <- evidence$normalized %||% list()
  merged_tables <- normalized$tables %||% list()
  for (label in labels) {
    merged_tables[[label]] <- list()
  }
  normalized$tables <- merged_tables
  normalized$plots <- normalized$plots %||% list()
  diagnostics <- existing$diagnostics %||% list()
  diagnostics$table_count <- as.integer(length(merged_tables))
  diagnostics$attached_table_count <- as.integer(length(tables))
  diagnostics$workbook_count <- as.integer(max(1L, length(workbooks)))
  if ("normalized" %in% names(diagnostics)) {
    diagnostics$normalized <- normalized
  }
  status <- existing$status %||% "not_applicable"
  if (identical(status, "not_applicable")) {
    status <- "valid"
  }
  compatibility <- existing$compatibility %||% list()
  compatibility$viewer <- status %in% c("valid", "attention")
  entry <- builder_manifest_entry(
    id = "extra_material",
    source = list(type = "builder_attachment", location = "settings$tables"),
    status = status,
    disposition = "attached",
    artifact_scope = existing$artifact_scope %||% "both",
    summary = paste(length(merged_tables), "table(s) will be included."),
    diagnostics = diagnostics,
    compatibility = compatibility,
    pages = unique(c(existing$pages %||% character(), "extra_material")),
    required_action = existing$required_action %||% NULL,
    verifier = existing$verifier %||% "verify_extra_material"
  )
  evidence$detected <- TRUE
  evidence$valid <- !status %in% c("blocking", "checking")
  evidence$normalized <- normalized
  evidence$diagnostics <- evidence$diagnostics %||% character()
  evidence$requirements <- unique(c(
    evidence$requirements %||% character(),
    "named_tables_or_plots",
    "unique_names"
  ))
  evidence$page_candidates <- unique(c(
    evidence$page_candidates %||% character(),
    "extra_material"
  ))
  entry$evidence <- evidence
  entry$page_visible <- TRUE

  entries <- unname(manifest)
  ids <- vapply(entries, `[[`, character(1), "id")
  index <- match("extra_material", ids)
  if (is.na(index)) {
    entries[[length(entries) + 1L]] <- entry
  } else {
    entries[[index]] <- entry
  }
  builder_content_manifest(entries)
}

builder_freeze_plan <- function(
  entries,
  out_dir,
  make_app = FALSE,
  overwrite = FALSE,
  revision = NULL,
  app_options = list(),
  expected_prior_identity = NULL,
  app_auth = list(
    enabled = FALSE,
    account_count = 0L,
    timeout_minutes = 15L
  ),
  dataset_state_cache = NULL
) {
  if (
    !exists(
      "builder_dataset_state",
      mode = "function",
      inherits = TRUE
    )
  ) {
    return(builder_plan_error(
      "The Builder dataset-state authority is unavailable.",
      "state_authority_unavailable"
    ))
  }
  if (!is.list(entries)) {
    return(builder_plan_error(
      "Datasets must be supplied as a list.",
      "invalid_entries"
    ))
  }
  legacy_asset_key <- vapply(
    entries,
    function(entry) {
      settings <- if (is.list(entry)) entry$settings else NULL
      is.list(settings) &&
        any(
          c("public_assets", "public_asset_claims") %in% names(settings)
        )
    },
    logical(1)
  )
  if (any(legacy_asset_key)) {
    return(builder_plan_error(
      "Dataset settings use a retired asset field.",
      "invalid_entries"
    ))
  }
  if (
    !is.logical(make_app) ||
      length(make_app) != 1L ||
      is.na(make_app)
  ) {
    return(builder_plan_error(
      "Generated-app selection must be one non-missing logical value.",
      "invalid_make_app"
    ))
  }
  if (
    !is.logical(overwrite) ||
      length(overwrite) != 1L ||
      is.na(overwrite)
  ) {
    return(builder_plan_error(
      "Overwrite selection must be one non-missing logical value.",
      "invalid_overwrite"
    ))
  }
  if (.builder_plan_has_reference(app_options)) {
    return(builder_plan_error(
      "BuildPlan values cannot contain mutable reference objects.",
      "unsafe_reference"
    ))
  }
  if (!.builder_plan_app_options_valid(app_options)) {
    return(builder_plan_error(
      "Generated-app options must be an inert list.",
      "invalid_app_options"
    ))
  }
  if (!.builder_plan_app_auth_valid(app_auth, make_app)) {
    login_requires_app <- is.list(app_auth) &&
      !is.object(app_auth) &&
      identical(
        names(app_auth),
        c(
          "enabled",
          "account_count",
          "timeout_minutes"
        )
      ) &&
      isTRUE(app_auth$enabled) &&
      !isTRUE(make_app)
    return(builder_plan_error(
      if (login_requires_app) {
        "Generated-App login requires App output."
      } else {
        "The generated-App login settings are invalid."
      },
      "invalid_app_auth"
    ))
  }
  if (
    !is.null(expected_prior_identity) &&
      (!is.list(expected_prior_identity) ||
        is.object(expected_prior_identity))
  ) {
    return(builder_plan_error(
      "Expected prior identity must be an inert list.",
      "invalid_expected_prior_identity"
    ))
  }
  if (
    .builder_plan_has_reference(app_options) ||
      .builder_plan_has_reference(expected_prior_identity)
  ) {
    return(builder_plan_error(
      "BuildPlan values cannot contain mutable reference objects.",
      "unsafe_reference"
    ))
  }
  preflight <- .builder_plan_preflight_entries(entries, make_app = make_app)
  if (inherits(preflight, "builder_plan_failure")) {
    return(preflight)
  }
  plan_revision <- .builder_plan_revision(revision, entries)
  if (is.null(plan_revision)) {
    return(builder_plan_error(
      "Choose a valid positive BuildPlan revision.",
      "invalid_revision"
    ))
  }
  if (is.null(out_dir)) {
    return(builder_plan_error(
      "Choose an output directory.",
      "missing_output_directory"
    ))
  }
  if (
    !is.character(out_dir) ||
      length(out_dir) != 1L ||
      is.na(out_dir)
  ) {
    return(builder_plan_error(
      "The output directory must be one non-missing string.",
      "invalid_output_directory"
    ))
  }
  out_dir <- trimws(out_dir)
  if (!nzchar(out_dir)) {
    return(builder_plan_error(
      "Choose an output directory.",
      "missing_output_directory"
    ))
  }
  out_dir <- tryCatch(
    as.character(fs::path_norm(fs::path_abs(fs::path_expand(out_dir)))),
    error = function(error) NULL
  )
  if (is.null(out_dir)) {
    return(builder_plan_error(
      "The output directory could not be normalized.",
      "invalid_output_directory"
    ))
  }
  if (!length(entries)) {
    return(builder_plan_error(
      "Add at least one dataset.",
      "missing_dataset"
    ))
  }

  app_capability <- builder_app_capability()
  app_contract_version <- if (
    isTRUE(make_app) &&
      is.list(app_capability) &&
      identical(app_capability$version, 1L)
  ) {
    1L
  } else {
    0L
  }
  if (
    isTRUE(make_app) &&
      !(is.list(app_capability) &&
        isTRUE(app_capability$available) &&
        identical(app_contract_version, 1L))
  ) {
    reason <- if (
      is.list(app_capability) && builder_has_text(app_capability$reason)
    ) {
      app_capability$reason
    } else {
      builder_app_capability(0L)$reason
    }
    return(builder_plan_error(reason, "app_capability_unavailable"))
  }

  dataset_order <- vapply(
    entries,
    function(entry) {
      if (is.list(entry) && builder_has_text(entry$id %||% "")) {
        entry$id
      } else {
        ""
      }
    },
    character(1)
  )
  if (any(!nzchar(dataset_order)) || anyDuplicated(dataset_order)) {
    return(builder_plan_error(
      "Every dataset needs a unique stable id.",
      "invalid_dataset_order"
    ))
  }

  states <- lapply(entries, function(entry) {
    tryCatch(
      builder_cached_dataset_state(entry, dataset_state_cache),
      builder_state_error = function(error) error,
      builder_manifest_error = function(error) error
    )
  })
  state_errors <- vapply(states, inherits, logical(1), "condition")
  if (any(state_errors)) {
    error <- states[[which(state_errors)[[1L]]]]
    return(builder_plan_error(
      conditionMessage(error),
      error$code %||% "invalid_dataset_state"
    ))
  }
  names(states) <- dataset_order

  load_states <- vapply(states, `[[`, "", "load_state")
  if (any(load_states == "loading")) {
    return(builder_plan_error(
      "A dataset is still loading and cannot enter BuildPlan.",
      "dataset_loading"
    ))
  }
  if (any(load_states == "reload_required")) {
    return(builder_plan_error(
      "A dataset must be reloaded before BuildPlan can be frozen.",
      "dataset_reload_required"
    ))
  }
  missing_manifest <- vapply(
    states,
    function(state) identical(state$error_code, "missing_manifest"),
    logical(1)
  )
  if (any(missing_manifest)) {
    return(builder_plan_error(
      "A profiled dataset is missing its typed content manifest.",
      "missing_manifest"
    ))
  }
  readiness <- vapply(states, `[[`, "", "readiness")
  if (any(readiness == "blocked")) {
    index <- which(readiness == "blocked")[[1L]]
    ids <- states[[index]]$blocking_ids
    return(builder_plan_error(
      paste0(
        "Dataset ",
        dataset_order[[index]],
        " has a blocking capability",
        if (length(ids)) paste0(": ", paste(ids, collapse = ", ")) else "",
        "."
      ),
      "blocking_capability",
      list(dataset_id = dataset_order[[index]], capability_ids = ids)
    ))
  }
  if (any(readiness == "checking")) {
    return(builder_plan_error(
      "A dataset capability is still being checked.",
      "checking_capability"
    ))
  }
  if (any(readiness == "needs_attention")) {
    return(builder_plan_error(
      "A dataset capability still needs attention.",
      "attention_capability"
    ))
  }

  labels <- preflight$labels
  included_groups <- preflight$included_groups
  included_projections <- preflight$included_projections
  included_trajectories <- preflight$included_trajectories
  cell_cycle <- preflight$cell_cycle
  spatial_coordinate_transforms <- preflight$spatial_coordinate_transforms
  invalid_nomenclature <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      if (
        is.null(settings$recommendations) ||
          is.null(settings$nomenclature)
      ) {
        return(FALSE)
      }
      tryCatch(
        {
          builder_validate_nomenclature(
            settings$organism,
            settings$nomenclature
          )
          FALSE
        },
        error = function(error) TRUE
      )
    },
    logical(1)
  )
  if (any(invalid_nomenclature)) {
    return(builder_plan_error(
      "The selected nomenclature is invalid for the dataset organism.",
      "invalid_nomenclature"
    ))
  }

  planned_filenames <- vapply(
    seq_along(entries),
    function(index) {
      builder_item_filename(entries[[index]], index, length(entries))
    },
    character(1)
  )
  backends <- lapply(seq_along(entries), function(index) {
    .builder_plan_backend(entries[[index]]$settings, planned_filenames[[index]])
  })
  invalid_backends <- !vapply(backends, `[[`, logical(1), "valid")
  if (any(invalid_backends)) {
    error_code <- backends[[which(invalid_backends)[[1L]]]]$error_code
    return(builder_plan_error(
      "Every dataset needs a supported expression backend and sidecar plan.",
      error_code %||% "invalid_expression_backend"
    ))
  }
  sidecar_claims <- unlist(
    lapply(backends, `[[`, "sidecars"),
    use.names = FALSE
  )
  if (
    anyDuplicated(sidecar_claims) ||
      any(sidecar_claims %in% planned_filenames)
  ) {
    return(builder_plan_error(
      "Backend sidecar targets cannot be claimed by another dataset.",
      "backend_sidecar_conflict"
    ))
  }

  viewer_bundle_asset_records <- lapply(entries, function(entry) {
    .builder_plan_asset_set(entry$settings$viewer_bundle_assets)
  })
  private_user_asset_records <- lapply(entries, function(entry) {
    .builder_plan_asset_set(entry$settings$private_assets)
  })
  valid_asset_sets <- all(vapply(
    c(viewer_bundle_asset_records, private_user_asset_records),
    `[[`,
    logical(1),
    "valid"
  ))
  if (!valid_asset_sets) {
    return(builder_plan_error(
      paste0(
        "Asset manifests must use unique non-empty legacy targets or ",
        "typed source/target/artifact claims."
      ),
      "invalid_asset_manifest"
    ))
  }
  viewer_bundle_asset_sets <- lapply(
    viewer_bundle_asset_records,
    `[[`,
    "targets"
  )
  private_asset_sets <- lapply(seq_along(entries), function(index) {
    c(
      planned_filenames[[index]],
      backends[[index]]$sidecars,
      private_user_asset_records[[index]]$targets
    )
  })
  viewer_bundle_asset_targets <- unlist(
    viewer_bundle_asset_sets,
    use.names = FALSE
  )
  private_asset_targets <- unlist(private_asset_sets, use.names = FALSE)
  viewer_bundle_asset_claim_sets <- lapply(seq_along(entries), function(index) {
    viewer_bundle_asset_records[[index]]$claims
  })
  private_asset_claim_sets <- lapply(seq_along(entries), function(index) {
    c(
      .builder_plan_internal_asset_claims(
        c(planned_filenames[[index]], backends[[index]]$sidecars),
        entries[[index]]
      ),
      private_user_asset_records[[index]]$claims
    )
  })
  all_viewer_bundle_asset_claims <- unlist(
    viewer_bundle_asset_claim_sets,
    recursive = FALSE,
    use.names = FALSE
  )
  all_private_asset_claims <- unlist(
    private_asset_claim_sets,
    recursive = FALSE,
    use.names = FALSE
  )
  if (
    .builder_plan_claim_target_conflict(all_viewer_bundle_asset_claims) ||
      .builder_plan_claim_target_conflict(all_private_asset_claims)
  ) {
    return(builder_plan_error(
      "One asset target is claimed by different sources or artifacts.",
      "asset_target_conflict"
    ))
  }
  if (length(intersect(viewer_bundle_asset_targets, private_asset_targets))) {
    return(builder_plan_error(
      "An asset cannot be both Viewer-bundle eligible and private.",
      "asset_scope_conflict"
    ))
  }

  items <- tryCatch(
    lapply(seq_along(entries), function(index) {
      entry <- entries[[index]]
      settings <- entry$settings
      filename <- planned_filenames[[index]]
      if (identical(entry$load_state %||% "loaded", "artifact_ready")) {
        artifact <- entry$project_artifact %||% list()
        saved <- artifact$plan_item %||% NULL
        expected <- list(
          id = entry$id,
          name = labels[[index]],
          filename = filename,
          sidecars = backends[[index]]$sidecars,
          viewer_bundle_assets = viewer_bundle_asset_sets[[index]],
          private_assets = private_asset_sets[[index]],
          viewer_bundle_asset_claims = viewer_bundle_asset_claim_sets[[index]],
          private_asset_claims = private_asset_claim_sets[[index]]
        )
        if (
          !is.list(saved) ||
            !.builder_project_text(artifact$resolved_path %||% "") ||
            !file.exists(artifact$resolved_path) ||
            !builder_project_reused_plan_matches(saved, expected)
        ) {
          stop("invalid_reusable_artifact", call. = FALSE)
        }
        saved$reused_artifact <- list(
          path = artifact$resolved_path,
          fingerprint = artifact$fingerprint %||% list(),
          members = artifact$members %||% list()
        )
        saved$source_snapshot_identity <- list(
          available = FALSE,
          reused_artifact = TRUE
        )
        return(saved)
      }
      has_marker_genes <- .builder_state_content_available(
        entry,
        "marker_genes"
      )
      analyses <- states[[index]]$analyses
      analysis_dependency_graph <- .builder_plan_analysis_graph(
        analyses,
        has_marker_genes
      )
      artifact_identity <- .builder_plan_artifact_identity(
        entry,
        included_groups[[index]],
        included_projections[[index]],
        analyses,
        included_trajectories[[index]],
        cell_cycle[[index]]
      )
      source_snapshot_identity <- .builder_plan_source_snapshot_identity(entry)
      alignments <- .builder_plan_partition_alignments(
        settings$images %||% list()
      )
      spatial_sections <- artifact_identity$spatial_sections %||% character()
      profile_extras <- entry$profile$extras %||% list()
      has_trekker <- any(vapply(
        profile_extras,
        function(extra) {
          identical(extra$key %||% "", "trekker") && isTRUE(extra$found)
        },
        logical(1)
      )) ||
        isTRUE(
          entry$dataset_profile$content$trekker$detected %||% FALSE
        )
      alignment_sections <- unique(c(
        spatial_sections,
        if (has_trekker) "trekker" else character()
      ))
      alignments$spatial <- alignments$spatial[
        intersect(names(alignments$spatial), spatial_sections)
      ]
      if (!has_trekker) {
        alignments$trekker <- NULL
      }
      image_sections <- names(alignments$spatial) %||% character()
      aligned_sections <- unique(c(
        image_sections,
        if (!is.null(alignments$trekker)) "trekker" else character()
      ))
      default_group <- settings$default_group %||% settings$groups[[1L]]
      all_color_overrides <- builder_settings_color_overrides(settings)
      selected_color_overrides <- lapply(
        included_groups[[index]],
        function(group) {
          group_levels <- entry$levels[[group]] %||% character()
          values <- all_color_overrides[[group]] %||% character()
          kept <- intersect(group_levels, names(values))
          kept <- kept[vapply(
            values[kept],
            function(value) !is.null(builder_normalize_hex_color(value)),
            logical(1)
          )]
          values[kept]
        }
      )
      names(selected_color_overrides) <- included_groups[[index]]
      selected_color_overrides <- Filter(length, selected_color_overrides)
      custom_color_count <- sum(vapply(
        selected_color_overrides,
        length,
        integer(1)
      ))
      runtime_costs <- c(
        percent_mt_ribo = "seconds",
        most_expressed = "seconds",
        marker_genes = "minutes",
        enriched_pathways = "network-dependent"
      )
      page_expectations <- states[[index]]$page_expectations
      if (length(settings$tables %||% list())) {
        catalog <- builder_viewer_page_catalog()$conditional$id
        visible_pages <- unique(c(
          page_expectations$visible_conditional %||% character(),
          "extra_material"
        ))
        page_expectations$visible_conditional <- catalog[
          catalog %in% visible_pages
        ]
        page_expectations$hidden_conditional <- setdiff(
          catalog,
          page_expectations$visible_conditional
        )
      }
      item_manifest <- .builder_plan_manifest_with_tables(
        states[[index]]$manifest,
        settings$tables %||% list()
      )
      item <- list(
        id = entry$id,
        name = labels[index],
        filename = filename,
        organism = settings$organism,
        assay = settings$assay,
        layer = settings$layer,
        groups = settings$groups,
        included_groups = included_groups[[index]],
        cell_cycle = cell_cycle[[index]],
        reductions = settings$reductions,
        included_projections = included_projections[[index]],
        included_trajectories = included_trajectories[[index]],
        analyses = analyses,
        analysis_dependency_graph = analysis_dependency_graph,
        artifact_identity = artifact_identity,
        spatial_coordinate_transforms = spatial_coordinate_transforms[[
          index
        ]] %||%
          list(),
        cell_count = as.integer(
          entry$profile$n_cells %||%
            artifact_identity$cells$count %||%
            0L
        ),
        gene_count = as.integer(
          entry$profile$n_genes %||%
            artifact_identity$features$count %||%
            0L
        ),
        histology_coverage = list(
          sections = spatial_sections,
          with_histology = intersect(spatial_sections, image_sections),
          missing_histology = setdiff(spatial_sections, image_sections)
        ),
        spatial_alignment = list(
          section_count = as.integer(length(spatial_sections)),
          image_count = .builder_plan_spatial_image_count(alignments$spatial),
          points_only = setdiff(spatial_sections, image_sections)
        ),
        estimated_runtime = if (length(analyses)) {
          paste(unique(unname(runtime_costs[analyses])), collapse = ", ")
        } else {
          "no optional analysis runtime"
        },
        estimated_disk_bytes = as.double(
          source_snapshot_identity$closure_bytes %||% 0
        ),
        tables = settings$tables %||% list(),
        marker_imports = builder_freeze_marker_imports(
          settings$marker_imports %||% list()
        ),
        images = alignments$spatial,
        spatial_image_storage = settings$spatial_image_storage %||% "embedded",
        trekker_alignment = alignments$trekker,
        colors = builder_resolve_colors(settings, entry$levels %||% list()),
        group_color_overrides = selected_color_overrides,
        color_custom_count = as.integer(custom_color_count),
        nUMI = settings$nUMI %||% entry$profile$nUMI,
        nGene = settings$nGene %||% entry$profile$nGene,
        default_group = default_group,
        default_projection = settings$default_projection %||%
          settings$reductions[[1L]],
        default_trajectory = settings$default_trajectory %||% NULL,
        overview_point_size = settings$overview_point_size %||% 5,
        overview_point_opacity = settings$overview_point_opacity %||% 1,
        overview_percentage_cells_to_show = settings[[
          "overview_percentage_cells_to_show"
        ]] %||%
          100,
        metadata_policy = states[[index]]$metadata_policy %||%
          list(
            included = unique(c(
              "cell_barcode",
              included_groups[[index]],
              cell_cycle[[index]],
              settings$nUMI %||% entry$profile$nUMI,
              settings$nGene %||% entry$profile$nGene
            )),
            excluded = character()
          ),
        nomenclature = settings$nomenclature,
        expression_backend = backends[[index]]$mode,
        sidecars = backends[[index]]$sidecars,
        source_snapshot_identity = source_snapshot_identity,
        readiness = states[[index]]$readiness,
        manifest = item_manifest,
        viewer_page_expectations = page_expectations,
        acknowledgements = states[[index]]$acknowledgements,
        viewer_bundle_assets = viewer_bundle_asset_sets[[index]],
        private_assets = private_asset_sets[[index]],
        viewer_bundle_asset_claims = viewer_bundle_asset_claim_sets[[index]],
        private_asset_claims = private_asset_claim_sets[[index]]
      )
      if (!is.null(settings$recommendations)) {
        item$recommendations <- settings$recommendations
      }
      item
    }),
    error = function(error) error
  )
  if (inherits(items, "condition")) {
    code <- switch(
      conditionMessage(items),
      unsafe_reference = "unsafe_reference",
      invalid_snapshot_identity = "invalid_snapshot_identity",
      invalid_artifact_identity = "invalid_artifact_identity",
      invalid_reusable_artifact = "invalid_reusable_artifact",
      "invalid_frozen_value"
    )
    return(builder_plan_error(
      "BuildPlan values could not be frozen safely.",
      code
    ))
  }

  filenames <- vapply(items, `[[`, "", "filename")
  if (anyDuplicated(filenames)) {
    return(builder_plan_error(
      "Generated dataset filenames must be unique.",
      "duplicate_dataset_filename"
    ))
  }

  transient_input_names <- unlist(
    lapply(items, function(item) {
      c(item$filename, item$sidecars)
    }),
    use.names = FALSE
  )
  target_names <- if (isTRUE(make_app)) {
    c(
      "cerebro_app",
      if (isTRUE(app_auth$enabled)) "viewer-auth.env" else character()
    )
  } else {
    transient_input_names
  }
  targets <- file.path(out_dir, target_names)

  names(items) <- dataset_order
  manifests <- lapply(items, `[[`, "manifest")
  source_snapshot_identities <- lapply(
    items,
    `[[`,
    "source_snapshot_identity"
  )
  metadata_policy <- lapply(items, `[[`, "metadata_policy")
  backend_sidecars <- lapply(items, function(item) {
    list(mode = item$expression_backend, sidecars = item$sidecars)
  })
  analysis_dependency_graph <- lapply(
    items,
    `[[`,
    "analysis_dependency_graph"
  )
  viewer_page_expectations <- lapply(
    items,
    `[[`,
    "viewer_page_expectations"
  )
  acknowledgements <- lapply(items, `[[`, "acknowledgements")
  viewer_bundle_asset_claims <- .builder_plan_dedupe_claims(
    all_viewer_bundle_asset_claims
  )
  private_asset_claims <- .builder_plan_dedupe_claims(
    all_private_asset_claims
  )
  viewer_bundle_assets <- vapply(
    viewer_bundle_asset_claims,
    `[[`,
    character(1),
    "target"
  )
  private_assets <- vapply(
    private_asset_claims,
    `[[`,
    character(1),
    "target"
  )
  initial_dataset_supplied <- "initial_dataset" %in% names(app_options)
  initial_dataset_for_defaults <- app_options$initial_dataset %||%
    dataset_order[[1L]]
  if (!initial_dataset_for_defaults %in% dataset_order) {
    initial_dataset_for_defaults <- dataset_order[[1L]]
  }
  initial_point_size <- items[[initial_dataset_for_defaults]][[
    "overview_point_size"
  ]]
  initial_point_opacity <- items[[initial_dataset_for_defaults]][[
    "overview_point_opacity"
  ]]
  default_app_options <- list(
    enabled = isTRUE(make_app),
    show_upload_ui = FALSE,
    initial_dataset = dataset_order[[1L]],
    initial_dataset_mode = "automatic",
    initial_page = "data_info",
    welcome_message = "Welcome to CerebroNexus!",
    point_size = list(
      overview_projection_point_size = initial_point_size,
      projection_point_opacity = initial_point_opacity
    ),
    variable_to_compare = FALSE,
    host = "127.0.0.1",
    port = 8080L,
    max_request_size = 8000,
    display_mode = "normal",
    launch_browser = FALSE
  )
  frozen_app_options <- utils::modifyList(
    default_app_options,
    app_options,
    keep.null = TRUE
  )
  frozen_app_options$enabled <- isTRUE(make_app)
  frozen_app_options$initial_dataset_mode <- if (initial_dataset_supplied) {
    "explicit"
  } else {
    "automatic"
  }
  if (
    !builder_has_text(frozen_app_options$initial_dataset %||% "") ||
      !frozen_app_options$initial_dataset %in% dataset_order
  ) {
    return(builder_plan_error(
      "The generated-app initial dataset is not part of BuildPlan.",
      "invalid_app_options"
    ))
  }
  if (isTRUE(make_app)) {
    initial_expectations <- items[[
      frozen_app_options$initial_dataset
    ]]$viewer_page_expectations %||%
      list()
    always <- initial_expectations$always
    always_ids <- if (is.data.frame(always) && "id" %in% names(always)) {
      always$id
    } else {
      builder_viewer_page_catalog()$always$id
    }
    available_pages <- unique(c(
      always_ids,
      initial_expectations$visible_conditional %||% character()
    ))
    if (!frozen_app_options$initial_page %in% available_pages) {
      return(builder_plan_error(
        "The starting Viewer page is not available for the initial dataset.",
        "invalid_app_options"
      ))
    }
  }
  output_release <- list(
    directory = out_dir,
    overwrite = isTRUE(overwrite),
    replacement_policy = if (isTRUE(overwrite)) {
      "replace_existing_atomically"
    } else {
      "preserve_existing"
    },
    estimated_runtime = paste(
      unique(vapply(items, `[[`, character(1), "estimated_runtime")),
      collapse = "; "
    ),
    estimated_disk_bytes = sum(vapply(
      items,
      `[[`,
      numeric(1),
      "estimated_disk_bytes"
    )),
    targets = targets
  )

  plan <- list(
    error = NULL,
    error_code = NULL,
    details = list(),
    revision = plan_revision,
    readiness = "ready",
    dataset_order = dataset_order,
    out_dir = out_dir,
    make_app = isTRUE(make_app),
    app_contract_version = app_contract_version,
    overwrite = isTRUE(overwrite),
    items = unname(items),
    targets = targets,
    existing_targets = targets[file.exists(targets) | dir.exists(targets)],
    manifests = manifests,
    manifest = .builder_plan_release_manifest(manifests),
    source_snapshot_identities = source_snapshot_identities,
    metadata_policy = metadata_policy,
    backend_sidecars = backend_sidecars,
    analysis_dependency_graph = analysis_dependency_graph,
    viewer_page_expectations = viewer_page_expectations,
    viewer_bundle_assets = viewer_bundle_assets,
    private_assets = private_assets,
    viewer_bundle_asset_claims = viewer_bundle_asset_claims,
    private_asset_claims = private_asset_claims,
    acknowledgements = acknowledgements,
    app_options = frozen_app_options,
    app_auth = app_auth,
    output_release = output_release,
    expected_prior_identity = expected_prior_identity
  )
  safe <- tryCatch(
    !.builder_plan_has_reference(plan),
    error = function(error) FALSE
  )
  if (!isTRUE(safe)) {
    return(builder_plan_error(
      "BuildPlan values could not be frozen safely.",
      "unsafe_reference"
    ))
  }
  structure(plan, class = c("builder_build_plan", "list"))
}

builder_make_plan <- function(
  entries,
  out_dir,
  make_app = FALSE,
  overwrite = FALSE,
  app_options = list(),
  app_auth = list(
    enabled = FALSE,
    account_count = 0L,
    timeout_minutes = 15L
  )
) {
  builder_freeze_plan(
    entries = entries,
    out_dir = out_dir,
    make_app = make_app,
    overwrite = overwrite,
    app_options = app_options,
    app_auth = app_auth
  )
}
