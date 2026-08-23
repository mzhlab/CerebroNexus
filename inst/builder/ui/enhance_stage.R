## Guided Enhance stage.

builder_enhance_model <- function(
  id,
  profile,
  state,
  settings,
  modules,
  active_section = NULL,
  active_image = NULL
) {
  manifest <- state$manifest %||% list()
  retained <- Filter(
    function(entry) {
      identical(entry$status, "valid") &&
        !identical(entry$disposition, "rejected")
    },
    unname(manifest)
  )
  auto_retained <- lapply(retained, function(entry) {
    list(
      id = entry$id,
      label = entry$summary %||% entry$id,
      enabled_pages = entry$pages %||% character(),
      cost = "No additional build cost.",
      network = "No network access.",
      prerequisite = "Already validated by the frozen content manifest.",
      replacement_policy = paste0(
        "Existing validated content is ",
        entry$disposition %||% "retained",
        "."
      ),
      skip_consequence = "Not optional: frozen valid content stays in the CRB."
    )
  })
  extras <- profile$extras %||% list()
  has_trekker <- any(vapply(
    extras,
    function(entry) {
      identical(entry$key %||% "", "trekker") && isTRUE(entry$found)
    },
    logical(1)
  ))
  spatial_sections <- unique(c(
    profile$images %||% character(),
    if (has_trekker) "trekker" else character()
  ))
  spatial_images <- builder_image_collection_normalize(
    settings$images %||% list()
  )
  selected_section <- active_section
  if (is.null(selected_section) || !selected_section %in% spatial_sections) {
    selected_section <- if (length(spatial_sections)) {
      spatial_sections[[1L]]
    } else {
      NULL
    }
  }
  active_coordinate_transform <- if (is.null(selected_section)) {
    NULL
  } else {
    (settings$spatial_coordinate_transforms %||% list())[[selected_section]]
  }
  image_labels <- if (is.null(selected_section)) {
    character()
  } else {
    names(spatial_images[[selected_section]] %||% list()) %||% character()
  }
  selected_image <- if (
    !is.null(active_image) && active_image %in% image_labels
  ) {
    active_image
  } else if (length(image_labels)) {
    image_labels[[1L]]
  } else {
    NULL
  }
  active_image_record <- if (is.null(selected_image)) {
    NULL
  } else {
    spatial_images[[selected_section]][[selected_image]]
  }
  control_defaults <- builder_alignment_defaults()
  controls <- .builder_alignment_parameters(
    active_image_record %||% control_defaults
  )
  ranges <- builder_alignment_control_ranges(active_image_record)
  controls$dx_min <- ranges$dx$min
  controls$dx_max <- ranges$dx$max
  controls$dx_step <- ranges$dx$step
  controls$dy_min <- ranges$dy$min
  controls$dy_max <- ranges$dy$max
  controls$dy_step <- ranges$dy$step
  list(
    id = id,
    modules = modules,
    attachments = list(
      tables = list(
        label = "Extra material",
        enabled_pages = "extra material",
        relevant = TRUE,
        cost = "Lists sheets now and reads selected tables when building.",
        network = "No network access.",
        prerequisite = "Requires a readable CSV, TSV, XLS, XLSX, or XLSM file.",
        selected = names(settings$tables %||% list()) %||% character(),
        replacement_policy = "Each selected sheet is kept separately.",
        skip_consequence = paste(
          "Skipped tables will not appear in Extra material."
        )
      ),
      histology = list(
        label = "Spatial alignment",
        enabled_pages = "spatial",
        relevant = length(spatial_sections) > 0L ||
          any(vapply(
            manifest,
            function(entry) {
              (identical(entry$id, "spatial") ||
                "spatial" %in% (entry$pages %||% character())) &&
                !identical(entry$status, "not_applicable")
            },
            logical(1)
          )),
        cost = "Image decoding, encoding, and alignment.",
        network = "No network access.",
        prerequisite = "Requires spatial FOVs and coordinates.",
        sections = spatial_sections,
        active_section = selected_section,
        active_image = selected_image,
        coordinate_rotation = active_coordinate_transform$rotation_degrees %||%
          0,
        controls = controls,
        images = spatial_images,
        spatial_image_storage = settings$spatial_image_storage %||% "embedded",
        selected = names(settings$images %||% list()) %||% character(),
        replacement_policy = "Named images remain separate within each FOV.",
        skip_consequence = paste(
          "Sections without an image keep points-only spatial views."
        )
      )
    ),
    auto_retained = auto_retained
  )
}

builder_enhance_analysis_profile <- function(profile, organism) {
  profile$organism_guess <- organism
  profile
}

builder_enhance_analysis_applicability <- function(
  step,
  organism,
  blocked_reason = NULL
) {
  organism <- organism %||% ""
  intrinsic_not_applicable <- identical(step$id, "percent_mt_ribo") &&
    !organism %in% c("hg", "mm")
  list(
    relevant = !intrinsic_not_applicable,
    blocked = !intrinsic_not_applicable &&
      builder_stage_has_text(blocked_reason %||% ""),
    blocked_reason = if (intrinsic_not_applicable) NULL else blocked_reason
  )
}

builder_enhance_modules <- function(profile, settings) {
  steps <- builder_analysis_steps()
  analysis_profile <- builder_enhance_analysis_profile(
    profile,
    settings$organism
  )
  lapply(steps, function(step) {
    blocked <- try(
      builder_step_blocked(step, analysis_profile, settings$analyses),
      silent = TRUE
    )
    blocked_reason <- if (inherits(blocked, "try-error")) {
      "Prerequisites could not be evaluated."
    } else {
      blocked
    }
    applicability <- builder_enhance_analysis_applicability(
      step,
      settings$organism,
      blocked_reason
    )
    enabled_pages <- switch(
      step$id,
      percent_mt_ribo = "overview",
      most_expressed = "most expressed genes",
      marker_genes = "marker genes",
      enriched_pathways = "enriched pathways",
      character()
    )
    list(
      id = step$id,
      label = step$label,
      relevant = applicability$relevant,
      blocked = applicability$blocked,
      blocked_reason = applicability$blocked_reason,
      selected = step$id %in%
        settings$analyses ||
        (identical(step$id, "marker_genes") &&
          length(settings$marker_imports %||% list()) > 0L),
      enabled_pages = enabled_pages,
      replacement_policy = paste0(
        "A newly computed ",
        step$label,
        " result replaces the same existing result."
      ),
      skip_consequence = paste0(
        "No new ",
        step$label,
        " result or matching page is added."
      ),
      consequence = step$note,
      cost = step$cost,
      network = if (isTRUE(step$network)) {
        "Network access is required."
      } else {
        "No network access."
      },
      prerequisite = if (is.null(step$needs)) {
        "No analysis dependency."
      } else {
        paste("Requires", step$needs, "first.")
      }
    )
  })
}

builder_enhance_modules_ui <- function(id, modules) {
  ns <- NS(id)
  modules <- Filter(function(module) isTRUE(module$relevant), modules)
  if (!length(modules)) {
    return(p(class = "hint", "No optional modules apply to this dataset."))
  }
  tagList(lapply(modules, function(module) {
    marker_action <- identical(module$id, "marker_genes")
    div(
      class = paste(
        c(
          "enhance-module",
          if (isTRUE(module$selected)) "is-selected" else NULL,
          if (isTRUE(module$blocked)) "is-blocked" else NULL
        ),
        collapse = " "
      ),
      if (marker_action) {
        tags$button(
          id = ns("analysis_marker_genes_action"),
          type = "button",
          class = paste(
            "enhance-module-select marker-genes-action action-button",
            if (isTRUE(module$selected)) "is-selected" else ""
          ),
          `data-val` = "0",
          `aria-pressed` = if (isTRUE(module$selected)) "true" else "false",
          disabled = if (isTRUE(module$blocked)) "disabled",
          tags$span(class = "enhance-module-title", module$label),
          p(class = "consequence", module$consequence %||% ""),
          if (isTRUE(module$blocked)) {
            p(class = "blocked", module$blocked_reason %||% "Unavailable")
          }
        )
      } else {
        tags$label(
          class = "enhance-module-select",
          tags$input(
            id = ns(paste0("analysis_", module$id)),
            type = "checkbox",
            class = paste(
              "enhance-module-checkbox visually-hidden shiny-input-checkbox"
            ),
            checked = if (isTRUE(module$selected)) "checked",
            disabled = if (isTRUE(module$blocked)) "disabled"
          ),
          tags$span(class = "enhance-module-title", module$label),
          p(class = "consequence", module$consequence %||% ""),
          if (isTRUE(module$blocked)) {
            p(class = "blocked", module$blocked_reason %||% "Unavailable")
          }
        )
      },
      tags$button(
        type = "button",
        class = "enhance-info-button",
        `aria-label` = paste("More information about", module$label),
        `data-title` = module$label %||% "",
        `data-description` = module$consequence %||% "",
        `data-pages` = module$enabled_pages %||% "",
        `data-cost` = module$cost %||% "",
        `data-network` = module$network %||% "",
        `data-prerequisite` = module$prerequisite %||% "",
        `data-replacement` = module$replacement_policy %||% "",
        `data-skip` = module$skip_consequence %||% "",
        "i"
      )
    )
  }))
}

builder_tissue_image_file_ui <- function(id, record) {
  ns <- NS(id)
  source <- record$source %||% list()
  filename <- builder_safe_file_name(source$name, "Tissue image")
  detail <- paste(
    builder_file_type_label(filename, source$type),
    builder_file_human_size(source$size %||% NA_real_),
    sep = " · "
  )
  div(
    class = "builder-file-list builder-file-list--single",
    div(
      class = "builder-file-item enhance-tissue-file-item",
      div(
        class = "enhance-tissue-file-meta",
        strong(filename),
        span(class = "hint", detail)
      ),
      span(
        class = "builder-status enhance-tissue-file-status builder-status--ready",
        "Added"
      ),
      div(
        class = "builder-action-row enhance-tissue-file-action-row",
        div(
          class = "enhance-tissue-file-actions",
          actionButton(
            ns("rename_image"),
            "Rename image",
            class = "btn"
          ),
          actionButton(
            ns("drop_image"),
            "Remove",
            class = "btn btn-remove-soft"
          )
        )
      )
    )
  )
}

builder_alignment_plot_output <- function(id, label) {
  div(
    class = "spatial-alignment-plot-frame",
    tags$canvas(
      id = id,
      class = "builder-spatial-canvas",
      role = "img",
      `aria-label` = label,
      `aria-describedby` = paste0(id, "-summary")
    ),
    tags$div(
      id = paste0(id, "-tooltip"),
      class = "builder-spatial-canvas-tooltip",
      role = "tooltip",
      hidden = "hidden"
    ),
    tags$p(
      id = paste0(id, "-summary"),
      class = "visually-hidden builder-spatial-canvas-summary",
      "Spatial alignment preview is loading."
    )
  )
}

builder_spatial_alignment_ui <- function(id, model) {
  ns <- NS(id)
  sections <- as.character(model$sections %||% character())
  section_labels <- sections
  section_labels[sections == "trekker"] <- "Trekker physical space"
  choices <- stats::setNames(sections, section_labels)
  selected_section <- model$active_section
  if (
    length(sections) &&
      (is.null(selected_section) || !selected_section %in% sections)
  ) {
    selected_section <- sections[[1L]]
  }
  initial_image_choices <- if (length(sections)) {
    names(model$images[[selected_section]] %||% list()) %||% character()
  } else {
    character()
  }
  selected_image <- model$active_image
  if (is.null(selected_image) || !selected_image %in% initial_image_choices) {
    selected_image <- if (length(initial_image_choices)) {
      initial_image_choices[[1L]]
    } else {
      character()
    }
  }
  controls <- model$controls %||% builder_alignment_defaults()
  tagList(
    p(
      class = "enhance-attachment-description",
      "Align tissue images with the spatial coordinates for each FOV or section."
    ),
    if (length(sections)) {
      div(
        class = "spatial-alignment-layout",
        div(
          class = "spatial-alignment-sidebar builder-controls-grid",
          div(
            class = "spatial-alignment-sidebar-fixed",
            selectInput(
              ns("active_section"),
              "Spatial capture (FOV)",
              choices = choices,
              selected = selected_section,
              selectize = FALSE
            )
          ),
          div(
            class = "spatial-alignment-sidebar-body",
            div(
              class = "spatial-alignment-sidebar-primary",
              conditionalPanel(
                condition = "output['has_coordinate_frame']",
                tags$details(
                  class = "spatial-coordinate-settings",
                  open = "open",
                  tags$summary("Coordinate settings"),
                  div(
                    class = "spatial-coordinate-settings-body",
                    p(
                      class = "hint",
                      "Transform coordinates before export. Positive rotation is counter-clockwise."
                    ),
                    sliderInput(
                      ns("coordinate_rotation"),
                      "Coordinate rotation (degrees)",
                      -180,
                      180,
                      model$coordinate_rotation %||% 0,
                      step = 0.1,
                      ticks = FALSE
                    ),
                    sliderInput(
                      ns("point_opacity"),
                      "Point opacity",
                      0,
                      100,
                      (controls$point_opacity %||% 0.85) * 100,
                      step = 5,
                      post = "%",
                      ticks = FALSE
                    ),
                    sliderInput(
                      ns("point_size"),
                      "Point size",
                      1,
                      12,
                      controls$point_size %||% 5,
                      step = 1,
                      ticks = FALSE
                    ),
                    div(
                      class = "builder-action-row",
                      actionButton(
                        ns("reset_coordinate_transform"),
                        "Reset",
                        class = "btn btn-outline-secondary"
                      )
                    )
                  )
                ),
                ns = ns
              ),
              div(
                class = "enhance-tissue-file-control builder-file-picker builder-file-picker--compact",
                tags$input(
                  id = ns("tissue_image_file"),
                  name = ns("tissue_image_file"),
                  class = "shiny-input-file enhance-tissue-file-input builder-file-input",
                  type = "file",
                  accept = ".png,.jpg,.jpeg",
                  `tabindex` = "-1"
                ),
                tags$label(
                  `for` = ns("tissue_image_file"),
                  class = "enhance-tissue-file-button builder-file-trigger",
                  `tabindex` = "0",
                  role = "button",
                  uiOutput(ns("add_image_label"), inline = TRUE)
                )
              ),
              conditionalPanel(
                condition = "output['has_multiple_images']",
                selectInput(
                  ns("active_image"),
                  "Image",
                  choices = initial_image_choices,
                  selected = selected_image
                ),
                ns = ns
              ),
              div(
                class = "spatial-alignment-status",
                `aria-live` = "polite",
                uiOutput(ns("alignment_status"))
              ),
            ),
            div(
              class = "spatial-alignment-sidebar-scroll",
              conditionalPanel(
                condition = "output['has_image']",
                div(
                  class = "spatial-alignment-controls builder-controls-grid builder-controls-grid--sliders",
                  tags$details(
                    class = "spatial-coordinate-settings",
                    open = "open",
                    tags$summary("Position"),
                    div(
                      class = "spatial-coordinate-settings-body",
                      sliderInput(
                        ns("img_dx"),
                        "Horizontal offset",
                        controls$dx_min %||% -1,
                        controls$dx_max %||% 1,
                        controls$dx %||% 0,
                        step = controls$dx_step %||% NULL,
                        ticks = FALSE
                      ),
                      sliderInput(
                        ns("img_dy"),
                        "Vertical offset",
                        controls$dy_min %||% -1,
                        controls$dy_max %||% 1,
                        controls$dy %||% 0,
                        step = controls$dy_step %||% NULL,
                        ticks = FALSE
                      )
                    )
                  ),
                  tags$details(
                    class = "spatial-coordinate-settings",
                    open = "open",
                    tags$summary("Scale & orientation"),
                    div(
                      class = "spatial-coordinate-settings-body",
                      sliderInput(
                        ns("img_scale"),
                        "Scale",
                        0.2,
                        3,
                        controls$scale %||% 1,
                        step = 0.02,
                        ticks = FALSE
                      ),
                      sliderInput(
                        ns("img_rotate"),
                        "Rotation (degrees)",
                        -180,
                        180,
                        controls$rotation %||% 0,
                        ticks = FALSE
                      ),
                      checkboxInput(
                        ns("image_flip_x"),
                        "Flip horizontally",
                        isTRUE(controls$flip_x)
                      ),
                      checkboxInput(
                        ns("image_flip_y"),
                        "Flip vertically",
                        isTRUE(controls$flip_y)
                      )
                    )
                  ),
                  tags$details(
                    class = "spatial-coordinate-settings",
                    open = "open",
                    tags$summary("Image appearance"),
                    div(
                      class = "spatial-coordinate-settings-body",
                      sliderInput(
                        ns("image_opacity"),
                        "Image opacity",
                        0,
                        100,
                        (controls$image_opacity %||% 0.8) * 100,
                        step = 5,
                        post = "%",
                        ticks = FALSE
                      )
                    )
                  )
                ),
                div(
                  class = "spatial-alignment-actions builder-action-row",
                  actionButton(
                    ns("reset_align"),
                    "Reset alignment",
                    class = "btn btn-quiet"
                  ),
                  actionButton(
                    ns("apply_align_all"),
                    "Apply transform to matching image label",
                    class = "btn btn-quiet"
                  )
                ),
                ns = ns
              ),
              conditionalPanel(
                condition = "output['has_image']",
                tags$details(
                  class = "spatial-coordinate-settings spatial-image-options",
                  tags$summary("Spatial image options"),
                  div(
                    class = "spatial-coordinate-settings-body",
                    selectInput(
                      ns("spatial_image_storage"),
                      "Image storage",
                      choices = c(
                        "External files in App (spatial-assets/)" = "external",
                        "Embedded in CRB" = "embedded"
                      ),
                      selected = model$spatial_image_storage %||% "embedded"
                    )
                  )
                ),
                ns = ns
              )
            )
          )
        ),
        div(
          class = "spatial-alignment-main",
          div(
            class = "spatial-alignment-plots builder-preview-grid",
            tags$figure(
              class = "spatial-alignment-figure",
              tags$figcaption(
                class = "spatial-alignment-figure-header",
                h5("Spatial space"),
                uiOutput(ns("alignment_spatial_label"), inline = TRUE)
              ),
              builder_alignment_plot_output(
                ns("alignment_spatial_plot"),
                "Spatial-space cell plot"
              ),
              div(
                class = "spatial-alignment-legend-wrap",
                h5("Groups"),
                uiOutput(ns("alignment_legend"))
              )
            )
          )
        )
      )
    }
  )
}

builder_enhance_stage_ui <- function(
  id,
  model,
  dynamic_modules = FALSE,
  include_spatial = TRUE
) {
  ns <- NS(id)
  analysis_modules <- Filter(
    function(module) isTRUE(module$relevant),
    model$modules %||% list()
  )
  selected_analysis_count <- sum(vapply(
    analysis_modules,
    function(module) isTRUE(module$selected),
    logical(1)
  ))
  div(
    id = ns("stage"),
    class = "builder-enhancement-stack",
    tags$input(
      id = ns("rendered_for"),
      type = "text",
      class = "builder-rendered-for-input",
      value = model$id,
      hidden = "hidden",
      tabindex = "-1",
      `aria-hidden` = "true"
    ),
    tags$section(
      class = "builder-stage-section builder-stage-enhance",
      tags$details(
        class = paste(
          "enhance-group enhance-group--analyses",
          "builder-viewer-card builder-viewer-optional-analyses"
        ),
        `data-disclosure-key` = "optional-analyses",
        tags$summary(
          span(class = "builder-viewer-card-title", "Optional analyses"),
          span(
            class = "builder-viewer-card-count",
            `data-analysis-count` = "true",
            paste(selected_analysis_count, "included")
          )
        ),
        div(
          class = "builder-viewer-card-body",
          h3("Optional enhancements"),
          p(
            class = "stage-intro",
            "Optional: add analysis pages or attach supporting files. You can skip this stage."
          ),
          div(
            class = "enhance-module-grid",
            if (isTRUE(dynamic_modules)) {
              uiOutput(ns("analysis_modules"))
            } else {
              builder_enhance_modules_ui(id, model$modules %||% list())
            }
          )
        )
      ),
      div(
        class = "enhance-group enhance-group--attachments",
        tags$details(
          class = paste(
            "enhance-attachment-block enhance-attachment-block--tables",
            "builder-viewer-card builder-viewer-extra-material"
          ),
          `data-disclosure-key` = "extra-material",
          tags$summary(
            span(class = "builder-viewer-card-title", "Extra material"),
            span(
              class = "builder-viewer-card-count",
              `data-extra-material-count` = "true",
              paste(
                length(model$attachments$tables$selected %||% character()),
                "included"
              )
            )
          ),
          div(
            class = "builder-viewer-card-body",
            p(
              class = "enhance-attachment-description",
              "Add optional CSV, TSV, XLS, XLSX, or XLSM tables. Sheet names are listed now; selected tables are read when building."
            ),
            div(
              class = "enhance-table-file-actions builder-action-row",
              tags$button(
                id = ns("choose_local_tables"),
                class = paste(
                  "btn btn-primary action-button",
                  "enhance-table-local-button"
                ),
                type = "button",
                `data-val` = "0",
                span("Choose local tables…")
              ),
              div(
                class = "enhance-table-file-control builder-file-picker builder-file-picker--content",
                tags$input(
                  id = ns("table_files"),
                  name = ns("table_files"),
                  class = "shiny-input-file enhance-table-file-input builder-file-input",
                  type = "file",
                  multiple = "multiple",
                  accept = ".csv,.tsv,.txt,.xls,.xlsx,.xlsm",
                  `tabindex` = "-1"
                ),
                tags$label(
                  `for` = ns("table_files"),
                  class = paste(
                    "enhance-table-file-button builder-file-trigger",
                    "builder-file-trigger--secondary"
                  ),
                  `tabindex` = "0",
                  role = "button",
                  span("Upload through browser…")
                )
              )
            ),
            uiOutput(ns("table_list"))
          )
        )
      )
    ),
    if (isTRUE(include_spatial)) builder_spatial_stage_ui(id, model)
  )
}

builder_spatial_stage_ui <- function(id, model) {
  histology <- model$attachments$histology %||% list()
  if (!isTRUE(histology$relevant)) {
    return(NULL)
  }
  tags$div(
    class = "builder-stage-spatial spatial-alignment-workbench",
    tags$details(
      class = "builder-viewer-card builder-viewer-spatial-alignment",
      `data-disclosure-key` = "spatial-alignment",
      tags$summary(
        span(class = "builder-viewer-card-title", "Spatial alignment"),
        span(
          class = "builder-viewer-card-count",
          paste(
            length(histology$sections %||% character()),
            if (length(histology$sections %||% character()) == 1L) {
              "section"
            } else {
              "sections"
            }
          )
        )
      ),
      div(
        class = "builder-viewer-card-body builder-spatial-alignment-body",
        builder_spatial_alignment_ui(id, histology)
      )
    )
  )
}
