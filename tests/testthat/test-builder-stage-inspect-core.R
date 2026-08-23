builder_stage_contract_source_runtime(environment())

test_that("Inspect leads with attention and compact detected-content tags", {
  model <- list(
    summary = c("1,200 cells", "4,500 genes"),
    attention = c("Metadata column sample has missing values"),
    blockers = c("Projection coordinates are incomplete"),
    content_tags = list(
      list(label = "Expression", tone = "core"),
      list(label = "UMAP", tone = "projection")
    ),
    diagnostics = c("source fingerprint abc123")
  )

  html <- builder_stage_html(builder_inspect_stage_ui("inspect", model))

  expect_match(html, "builder-stage-section", fixed = TRUE)
  expect_match(html, "<h3>Import &amp; Inspect</h3>", fixed = TRUE)
  expect_match(html, "builder-dataset-summary", fixed = TRUE)
  expect_match(html, "builder-dataset-summary-facts", fixed = TRUE)
  expect_match(html, "<h4>Needs attention</h4>", fixed = TRUE)
  expect_match(html, "<h4>Detected content</h4>", fixed = TRUE)
  expect_match(html, "Needs attention", fixed = TRUE)
  expect_match(html, model$attention[[1L]], fixed = TRUE)
  expect_match(html, model$blockers[[1L]], fixed = TRUE)
  expect_match(html, "Detected content", fixed = TRUE)
  expect_match(html, "Expression", fixed = TRUE)
  expect_match(html, "UMAP", fixed = TRUE)
  expect_match(html, "builder-content-tag", fixed = TRUE)
  expect_false(grepl("View all detected content", html, fixed = TRUE))
  expect_false(grepl("Technical diagnostics", html, fixed = TRUE))
  expect_false(grepl("export|Build App", html, ignore.case = TRUE))
})

test_that("Inspect detected content comes from manifest readiness", {
  model <- builder_inspect_model(
    profile = list(n_cells = 1200L, n_genes = 4500L, internal_cache = TRUE),
    state = list(
      attention_ids = character(),
      blocking_ids = character(),
      manifest = list(
        expression = list(
          id = "expression",
          status = "valid",
          summary = "Expression matrix is Viewer-ready."
        ),
        spatial = list(
          id = "spatial",
          status = "not_applicable",
          summary = "No spatial sections were detected."
        )
      )
    ),
    format = "rds",
    dataset_id = "dataset-a"
  )
  html <- builder_stage_html(builder_inspect_stage_ui("inspect", model))

  expect_match(html, "Expression", fixed = TRUE)
  expect_false(grepl("spatial", html, fixed = TRUE))
  expect_false(grepl("No spatial sections were detected", html, fixed = TRUE))
  expect_false(grepl("internal_cache", html, fixed = TRUE))
})

test_that("Inspect content tags use readable colour families instead of a gray catch-all", {
  ids <- c(
    "marker_genes",
    "trajectory",
    "immune_repertoire",
    "extra_material"
  )
  tones <- vapply(
    ids,
    function(id) {
      builder_inspect_content_tag(list(id = id, status = "valid"))$tone
    },
    character(1)
  )

  expect_identical(
    unname(tones),
    c("analysis", "trajectory", "immune", "extra")
  )
})

test_that("Core offers cell-cycle annotations only for credible metadata", {
  metadata_catalog <- list(
    Phase = list(
      name = "Phase",
      classification = "categorical",
      group_eligible = TRUE,
      count = 80L,
      distinct_count = 3L,
      missing_count = 0L,
      missing_percentage = 0,
      sample_values = c("G1", "S", "G2M"),
      level_counts = list(
        items = list(
          list(value = "G1", count = 30L),
          list(value = "S", count = 25L),
          list(value = "G2M", count = 25L)
        ),
        total = 3L,
        truncated = FALSE,
        remaining_count = 0L
      )
    ),
    sample = list(
      name = "sample",
      classification = "categorical",
      group_eligible = TRUE,
      distinct_count = 2L
    ),
    S.Score = list(
      name = "S.Score",
      classification = "continuous",
      group_eligible = FALSE,
      distinct_count = 80L
    )
  )
  model <- list(
    id = "dataset-a",
    name = "PBMC",
    organism = "hg",
    organism_choices = c("hg", "mm"),
    included_groups = "sample",
    default_group = "sample",
    metadata_catalog = metadata_catalog,
    metadata_policy = list(included = c("Phase", "sample")),
    cell_cycle_columns = "Phase",
    default_projection = "umap",
    projection_choices = "umap",
    assay = "RNA",
    assay_choices = "RNA",
    layer = "data",
    layer_choices = "data",
    nUMI = "nCount_RNA",
    nUMI_choices = "nCount_RNA",
    nGene = "nFeature_RNA",
    nGene_choices = "nFeature_RNA",
    backend = "embedded",
    backend_choices = "embedded"
  )

  catalog <- builder_cell_cycle_catalog_model(model)
  cell_cycle_html <- builder_stage_html(
    builder_cell_cycle_catalog_ui("core", catalog)
  )
  html <- builder_stage_html(builder_core_stage_ui("core", model))

  expect_identical(
    vapply(catalog$items, `[[`, character(1), "id"),
    "Phase"
  )
  expect_identical(catalog$included, "Phase")
  expect_match(html, "Cell cycle", fixed = TRUE)
  expect_match(html, "1 included", fixed = TRUE)
  expect_match(html, 'id="core-cell_cycle"', fixed = TRUE)
  expect_match(html, "Phase · 3 phases", fixed = TRUE)
  expect_false(grepl("S.Score", cell_cycle_html, fixed = TRUE))

  model$metadata_catalog$Phase <- NULL
  html_without_candidate <- builder_stage_html(
    builder_core_stage_ui("core", model)
  )
  expect_false(grepl("Cell cycle", html_without_candidate, fixed = TRUE))
})

test_that("Group details describe the effective metadata policy truthfully", {
  metadata_catalog <- list(
    score = list(
      name = "score",
      classification = "continuous",
      group_eligible = FALSE,
      group_reason = "Continuous values are not suitable Viewer Groups.",
      count = 80L,
      distinct_count = 80L,
      missing_count = 0L,
      missing_percentage = 0,
      sample_values = as.character(seq_len(5L)),
      level_counts = list(
        items = list(),
        total = 80L,
        truncated = TRUE,
        remaining_count = 75L
      )
    )
  )
  cases <- list(
    included = list(
      retained = TRUE,
      expected = "Kept as ordinary metadata."
    ),
    excluded = list(
      retained = FALSE,
      expected = "Not retained in the CRB."
    ),
    attention = list(
      retained = FALSE,
      expected = "Not retained in the CRB."
    )
  )

  for (disposition in names(cases)) {
    case <- cases[[disposition]]
    policy <- list(
      columns = list(
        score = list(
          name = "score",
          value = disposition,
          disposition = disposition,
          effective_included = case$retained,
          requires_confirmation = disposition %in% c("attention", "blocking")
        )
      )
    )
    catalog <- builder_group_catalog_model(list(
      metadata_catalog = metadata_catalog,
      metadata_policy = policy
    ))
    detail <- builder_stage_html(builder_group_detail_ui(
      "core",
      builder_group_detail_model(catalog, "score")
    ))

    expect_identical(
      catalog$items[[1L]]$metadata_retained,
      case$retained,
      info = disposition
    )
    expect_identical(
      catalog$items[[1L]]$metadata_disposition,
      disposition,
      info = disposition
    )
    expect_match(detail, "Not a Group", fixed = TRUE, info = disposition)
    expect_match(detail, case$expected, fixed = TRUE, info = disposition)
    expect_false(
      grepl("Needs attention before build.", detail, fixed = TRUE),
      info = disposition
    )
  }

  legacy <- builder_group_catalog_model(list(
    metadata_catalog = metadata_catalog,
    metadata_policy = list()
  ))
  legacy_detail <- builder_stage_html(builder_group_detail_ui(
    "core",
    builder_group_detail_model(legacy, "score")
  ))

  expect_true(is.na(legacy$items[[1L]]$metadata_retained))
  expect_false(grepl("Kept as ordinary metadata.", legacy_detail, fixed = TRUE))
  expect_false(grepl(
    "Not retained in the CRB.",
    legacy_detail,
    fixed = TRUE
  ))
})

test_that("Group colors stay behind a secondary closed Edit colors entry", {
  model <- list(
    metadata_catalog = list(
      cluster = list(
        name = "cluster",
        classification = "categorical",
        group_eligible = TRUE,
        count = 4L,
        distinct_count = 2L,
        missing_count = 0L,
        missing_percentage = 0,
        sample_values = c("A", "B", "A", "B"),
        level_counts = list(
          items = list(
            list(value = "A", count = 2L),
            list(value = "B", count = 2L)
          ),
          total = 2L,
          truncated = FALSE,
          remaining_count = 0L
        )
      )
    ),
    included_groups = "cluster",
    default_group = "cluster"
  )
  catalog <- builder_group_catalog_model(model)
  html <- builder_stage_html(builder_group_detail_ui(
    "core",
    builder_group_detail_model(catalog, "cluster")
  ))

  expect_match(html, "Edit colors", fixed = TRUE)
  expect_match(
    html,
    'class="viewer-group-colors-disclosure"',
    fixed = TRUE
  )
  expect_match(
    html,
    'data-disclosure-key="group-colors:cluster"',
    fixed = TRUE
  )
  expect_false(grepl('open="open"', html, fixed = TRUE))
})

test_that("Core keeps accessible group colors inside the Groups workspace", {
  model <- list(
    id = "dataset-a",
    name = "PBMC",
    organism = "hg",
    organism_choices = c("hg", "mm"),
    default_group = "cluster",
    group_choices = c("cluster", "sample"),
    default_projection = "umap",
    projection_choices = c("umap", "pca"),
    assay = "RNA",
    assay_choices = "RNA",
    layer = "data",
    layer_choices = "data",
    nUMI = "nCount_RNA",
    nUMI_choices = "nCount_RNA",
    nGene = "nFeature_RNA",
    nGene_choices = "nFeature_RNA",
    backend = "embedded",
    backend_choices = "embedded"
  )
  colors <- builder_group_colors_model(
    model$default_group,
    c("0", "1", "N/A"),
    "cerebro",
    list(cluster = c(`1` = "#e76f51"))
  )

  core <- builder_stage_html(builder_core_stage_ui("core", model))
  html <- builder_stage_html(builder_group_colors_ui("core", colors))

  expect_match(core, "core-groups", fixed = TRUE)
  expect_false(grepl("core-projection_gallery", core, fixed = TRUE))
  expect_match(html, "Group colors", fixed = TRUE)
  expect_match(html, "Coloring by:", fixed = TRUE)
  expect_match(html, "cluster", fixed = TRUE)
  expect_match(html, 'type="color"', fixed = TRUE)
  expect_match(html, 'aria-label="Color for cluster 0"', fixed = TRUE)
  expect_match(html, "Missing", fixed = TRUE)
  expect_match(html, "#E76F51", fixed = TRUE)
  expect_match(html, "Reset colors", fixed = TRUE)
  expect_false(grepl("UMAP colors|PCA colors", html))
})

test_that("Group colors bounds large groups and exposes local search", {
  twenty <- builder_group_colors_model(
    "cluster",
    sprintf("value-%02d", seq_len(20L)),
    "cerebro",
    list()
  )
  fifty <- builder_group_colors_model(
    "cluster",
    sprintf("long-category-name-%02d", seq_len(50L)),
    "cerebro",
    list()
  )
  twenty_html <- builder_stage_html(builder_group_colors_ui("core", twenty))
  fifty_html <- builder_stage_html(builder_group_colors_ui("core", fifty))

  expect_match(twenty_html, "Show all 20 colors", fixed = TRUE)
  expect_match(twenty_html, "Show fewer", fixed = TRUE)
  expect_match(twenty_html, 'data-visible-limit="12"', fixed = TRUE)
  expect_false(grepl("Find a group value", twenty_html, fixed = TRUE))
  expect_match(fifty_html, "Find a group value", fixed = TRUE)
  expect_match(fifty_html, "Show all 50 colors", fixed = TRUE)
  expect_match(fifty_html, 'title="long-category-name-50"', fixed = TRUE)
})

test_that("Group colors has a short empty state for invalid groups", {
  model <- builder_group_colors_model("", character(), "cerebro", list())
  html <- builder_stage_html(builder_group_colors_ui("core", model))

  expect_match(
    html,
    "Select an included Group to set its retained colors.",
    fixed = TRUE
  )
  expect_false(grepl('type="color"', html, fixed = TRUE))
})

test_that("Inspect does not repeat the verified cell and gene summary", {
  model <- list(
    summary = c("24 cells", "40 genes"),
    statistics = list(cells = 24L, genes = 40L),
    attention = character(),
    blockers = character(),
    content_tags = list()
  )

  html <- builder_stage_html(builder_inspect_stage_ui("inspect", model))

  expect_match(html, "24 cells", fixed = TRUE)
  expect_match(html, "40 genes", fixed = TRUE)
  expect_false(grepl("Verified profile", html, fixed = TRUE))
  expect_false(grepl("were verified", html, fixed = TRUE))
})

test_that("Organism keeps a custom current value in the editable choices", {
  model <- list(
    id = "dataset-a",
    name = "PBMC",
    organism = "danio_rerio",
    organism_choices = c(
      "Human (hg)" = "hg",
      "Mouse (mm)" = "mm",
      Other = "other"
    ),
    default_group = "cluster",
    group_choices = "cluster",
    default_projection = "umap",
    projection_choices = "umap",
    assay = "RNA",
    assay_choices = "RNA",
    layer = "data",
    layer_choices = "data",
    nUMI = "nCount_RNA",
    nUMI_choices = "nCount_RNA",
    nGene = "nFeature_RNA",
    nGene_choices = "nFeature_RNA",
    backend = "embedded",
    backend_choices = c(Embedded = "embedded")
  )

  html <- builder_stage_html(builder_core_stage_ui("core", model))

  expect_match(html, 'value="danio_rerio" selected', fixed = TRUE)
})
