builder_core_stage_ui <- function(id, model) {
  ns <- NS(id)
  organism_choices <- model$organism_choices %||% character()
  organism <- model$organism %||% ""
  if (
    builder_stage_has_text(organism) &&
      !organism %in% unname(organism_choices)
  ) {
    organism_choices <- c(organism_choices, organism)
  }
  group_catalog <- builder_group_catalog_model(model)
  cell_cycle_catalog <- builder_cell_cycle_catalog_model(model)
  analysis_results <- builder_analysis_results_model(model)
  specialized_content <- builder_specialized_content_model(model)
  div(
    id = ns("stage"),
    class = "builder-stage-section builder-stage-core",
    h3("Core settings"),
    tags$input(
      id = ns("rendered_for"),
      type = "text",
      class = "builder-rendered-for-input",
      value = model$id,
      hidden = "hidden",
      tabindex = "-1",
      `aria-hidden` = "true"
    ),
    div(
      class = "builder-form-grid",
      div(
        class = "builder-field builder-field--name",
        textInput(ns("name"), "Dataset name", value = model$name %||% "")
      ),
      div(
        class = "builder-field builder-field--organism",
        `data-builder-creatable-select` = "true",
        `data-builder-create-input-label` = "Custom organism",
        `data-builder-create-placeholder` = "Type another organism",
        `data-builder-create-action-label` = "Add custom organism",
        `data-builder-create-maxlength` = "80",
        selectizeInput(
          ns("organism"),
          "Organism",
          choices = organism_choices,
          selected = organism,
          options = list(
            create = FALSE,
            persist = TRUE,
            maxItems = 1L
          )
        )
      ),
      div(
        class = "builder-field builder-field--assay",
        selectInput(
          ns("assay"),
          "Assay",
          choices = model$assay_choices,
          selected = model$assay
        )
      ),
      div(
        class = "builder-field builder-field--layer",
        selectInput(
          ns("layer"),
          "Layer",
          choices = model$layer_choices,
          selected = model$layer
        )
      )
    ),
    div(
      class = "builder-configure-metadata-chips",
      lapply(group_catalog$items, function(item) {
        span(item$label)
      })
    ),
    tags$section(
      id = ns("content"),
      class = "builder-viewer-content",
      div(
        class = "builder-viewer-content-head",
        h4("CRB content"),
        p(
          "Metadata is retained automatically. Review the available columns and configure Viewer Groups, default group, and colors."
        )
      ),
      if (specialized_content$total_count > 0L) {
        tags$details(
          class = "builder-viewer-card builder-viewer-specialized-content",
          `data-disclosure-key` = "viewer-specialized-content",
          tags$summary(
            span(class = "builder-viewer-card-title", "Specialized content"),
            span(
              class = "builder-viewer-card-count",
              specialized_content$summary
            )
          ),
          div(
            class = "builder-viewer-card-body",
            builder_specialized_content_ui(specialized_content, id)
          )
        )
      },
      tags$details(
        id = ns("groups"),
        class = "builder-viewer-card builder-viewer-groups",
        `data-disclosure-key` = "viewer-groups",
        tags$summary(
          span(class = "builder-viewer-card-title", "Groups"),
          span(
            class = "builder-viewer-card-count",
            `data-viewer-group-count` = "true",
            paste0(
              group_catalog$included_count,
              " included · ",
              "Default: ",
              group_catalog$default %||% "None"
            )
          )
        ),
        div(
          class = "builder-viewer-card-body",
          builder_group_catalog_ui(id, group_catalog)
        )
      ),
      if (length(cell_cycle_catalog$items)) {
        tags$details(
          class = "builder-viewer-card builder-viewer-cell-cycle",
          `data-disclosure-key` = "viewer-cell-cycle",
          tags$summary(
            span(class = "builder-viewer-card-title", "Cell cycle"),
            span(
              class = "builder-viewer-card-count",
              paste(cell_cycle_catalog$included_count, "included")
            )
          ),
          div(
            class = "builder-viewer-card-body",
            builder_cell_cycle_catalog_ui(id, cell_cycle_catalog)
          )
        )
      },
      if (analysis_results$total_count > 0L) {
        tags$details(
          class = "builder-viewer-card builder-viewer-analysis-results",
          `data-disclosure-key` = "viewer-analysis-results",
          tags$summary(
            span(class = "builder-viewer-card-title", "Analysis results"),
            span(
              class = "builder-viewer-card-count",
              paste(analysis_results$total_count, "detected or planned")
            )
          ),
          div(
            class = "builder-viewer-card-body",
            builder_analysis_results_ui(analysis_results)
          )
        )
      }
    ),
    if (builder_stage_has_text(model$metadata_attention %||% "")) {
      div(class = "notice warn", model$metadata_attention)
    },
    if (builder_stage_has_text(model$layer_attention %||% "")) {
      div(class = "notice bad", model$layer_attention)
    },
    tags$details(
      class = "builder-viewer-card builder-viewer-advanced-settings",
      `data-disclosure-key` = "viewer-advanced-settings",
      tags$summary(
        span(class = "builder-viewer-card-title", "Advanced settings"),
        span(class = "builder-viewer-card-count", "3 settings")
      ),
      div(
        class = "builder-viewer-card-body",
        div(
          class = "builder-advanced-grid",
          selectInput(
            ns("nUMI"),
            "UMI QC column",
            choices = model$nUMI_choices,
            selected = model$nUMI
          ),
          selectInput(
            ns("nGene"),
            "Gene QC column",
            choices = model$nGene_choices,
            selected = model$nGene
          ),
          selectInput(
            ns("backend"),
            "Expression backend",
            choices = model$backend_choices,
            selected = model$backend
          )
        )
      )
    )
  )
}

builder_views_stage_ui <- function(id, model, spatial = NULL) {
  ns <- NS(id)
  projection_catalog <- builder_projection_catalog_model(model)
  trajectory_catalog <- builder_trajectory_catalog_model(model)
  projection_default <- Filter(
    function(item) identical(item$id, projection_catalog$default),
    projection_catalog$items
  )
  projection_default_label <- if (length(projection_default)) {
    projection_default[[1L]]$label
  } else {
    "None"
  }

  tags$section(
    class = "builder-stage-section builder-stage-views builder-viewer-content",
    if (length(trajectory_catalog$items)) {
      tags$details(
        class = "builder-viewer-card builder-viewer-trajectories",
        `data-disclosure-key` = "viewer-trajectories",
        tags$summary(
          span(class = "builder-viewer-card-title", "Trajectories"),
          span(
            class = "builder-viewer-card-count",
            `data-viewer-trajectory-count` = "true",
            paste0(
              trajectory_catalog$included_count,
              " included",
              if (is.list(trajectory_catalog$default)) {
                paste0(" · Default: ", trajectory_catalog$default$name)
              } else {
                ""
              }
            )
          )
        ),
        div(
          class = "builder-viewer-card-body",
          uiOutput(ns("trajectory_gallery"))
        )
      )
    },
    tags$details(
      class = "builder-viewer-card builder-viewer-projections",
      `data-disclosure-key` = "viewer-projections",
      tags$summary(
        span(class = "builder-viewer-card-title", "Projections"),
        span(
          class = "builder-viewer-card-count",
          `data-viewer-projection-count` = "true",
          paste0(
            projection_catalog$included_count,
            " included · Default: ",
            projection_default_label
          )
        )
      ),
      div(
        class = "builder-viewer-card-body",
        uiOutput(ns("projection_gallery"))
      )
    ),
    spatial
  )
}
