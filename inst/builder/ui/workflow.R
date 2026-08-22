builder_workflow_progress_ui <- function(
  stage,
  available,
  confirmed = FALSE,
  locked = FALSE
) {
  stages <- c("upload", "configure", "review", "build")
  if (
    !is.character(stage) ||
      length(stage) != 1L ||
      is.na(stage) ||
      !stage %in% stages
  ) {
    stop("A valid Builder workflow stage is required.", call. = FALSE)
  }
  if (!is.logical(confirmed) || length(confirmed) != 1L || is.na(confirmed)) {
    stop("A confirmation state is required.", call. = FALSE)
  }
  if (
    !is.logical(available) ||
      !identical(names(available), stages) ||
      anyNA(available) ||
      !is.logical(locked) ||
      length(locked) != 1L ||
      is.na(locked)
  ) {
    stop("Valid Builder workflow availability is required.", call. = FALSE)
  }
  labels <- c("Data", "Configure", "Review", "Build")
  descriptions <- c(
    "Add datasets",
    "Set each dataset",
    "Resolve issues",
    "Create output"
  )
  current_index <- match(stage, stages)
  tags$nav(
    class = "builder-workflow-progress",
    `aria-label` = "Builder progress",
    `data-workflow-confirmed` = if (confirmed) "true" else "false",
    tags$ol(lapply(seq_along(stages), function(index) {
      stage_id <- stages[[index]]
      current <- identical(stage, stage_id)
      complete <- index < current_index
      enabled <- isTRUE(available[[stage_id]]) && !isTRUE(locked)
      step_label <- tagList(
        tags$span(
          class = "builder-workflow-step-number",
          if (complete) "✓" else index
        ),
        tags$span(
          class = "builder-workflow-step-copy",
          tags$strong(labels[[index]]),
          tags$small(descriptions[[index]])
        )
      )
      label <- if (current) {
        tags$span(step_label)
      } else if (enabled) {
        actionLink(
          paste0("workflow_stage_", stage_id),
          step_label,
          class = "builder-workflow-stage-link",
          `aria-label` = if (complete) {
            paste(labels[[index]], "completed")
          } else {
            labels[[index]]
          }
        )
      } else {
        tags$span(
          `aria-disabled` = "true",
          `aria-label` = if (complete) {
            paste(labels[[index]], "completed")
          } else {
            labels[[index]]
          },
          step_label
        )
      }
      tags$li(
        class = paste(
          if (current) "is-current" else NULL,
          if (isTRUE(available[[stage_id]])) {
            "is-available"
          } else {
            "is-unavailable"
          },
          if (isTRUE(locked) && !current) "is-locked" else NULL,
          if (complete) "is-complete" else NULL
        ),
        `aria-current` = if (current) "step" else NULL,
        label
      )
    }))
  )
}

builder_stage_header_ui <- function(stage, title, intro) {
  step <- match(stage, c("Data", "Configure", "Review", "Build"))
  eyebrow <- if (length(step) && !is.na(step)) {
    paste("Step", step, "of 4")
  } else {
    stage
  }
  tags$header(
    class = "builder-stage-header",
    tags$p(class = "builder-stage-eyebrow", eyebrow),
    tags$h2(title),
    tags$p(class = "stage-intro", intro)
  )
}

builder_stage_summary_ui <- function(..., class = NULL) {
  tags$div(class = paste("builder-stage-summary", class), ...)
}

builder_stage_section_ui <- function(
  title,
  ...,
  description = NULL,
  class = NULL
) {
  tags$section(
    class = paste("builder-stage-section", class),
    tags$div(
      class = "builder-stage-section-head",
      tags$h3(title),
      if (!is.null(description)) tags$p(description)
    ),
    ...
  )
}

builder_stage_footer_ui <- function(status, ...) {
  tags$footer(
    class = "builder-stage-footer",
    tags$p(class = "builder-stage-footer-status", status),
    tags$div(class = "builder-stage-footer-actions", ...)
  )
}

builder_configure_actions_ui <- function(
  message,
  can_continue,
  dataset_checked = FALSE,
  remaining = 0L
) {
  stopifnot(
    is.character(message),
    length(message) == 1L,
    !is.na(message),
    is.logical(can_continue),
    length(can_continue) == 1L,
    !is.na(can_continue)
  )
  builder_stage_footer_ui(
    message,
    if (remaining < 1L) {
      actionButton(
        "continue_to_review",
        "✓ Continue to Review →",
        class = "btn btn-action builder-review-ready",
        disabled = !isTRUE(can_continue)
      )
    } else {
      actionButton(
        "complete_dataset_check",
        if (isTRUE(dataset_checked)) {
          "Review next dataset →"
        } else if (remaining > 1L) {
          "✓ Mark checked & review next dataset"
        } else {
          "✓ Finish checking"
        },
        class = "btn btn-dataset-check"
      )
    }
  )
}

builder_server_path_dialog <- function(
  title,
  input_id,
  action_id,
  label,
  action_label
) {
  modalDialog(
    title = title,
    tags$p(
      "The native picker is unavailable. Enter an absolute path on this server."
    ),
    tags$label(label, `for` = input_id),
    tags$input(
      id = input_id,
      type = "text",
      class = "shiny-input-text form-control",
      autocomplete = "off",
      spellcheck = "false"
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(action_id, action_label, class = "btn btn-primary")
    ),
    easyClose = FALSE,
    size = "m"
  )
}

builder_build_stage_controls_ui <- function(
  output_path,
  controls_disabled = FALSE
) {
  stopifnot(
    is.character(output_path),
    length(output_path) <= 1L,
    !length(output_path) || !is.na(output_path),
    is.logical(controls_disabled),
    length(controls_disabled) == 1L,
    !is.na(controls_disabled)
  )
  selected_label <- if (builder_has_text(output_path)) {
    output_path
  } else {
    "No output folder selected"
  }
  tags$section(
    class = "builder-stage-section builder-build-destination",
    tags$h3("Destination"),
    p(class = "builder-selected-output", selected_label),
    div(
      class = "builder-build-destination-actions",
      actionButton(
        "choose_output_folder",
        "Choose folder…",
        class = "btn",
        disabled = controls_disabled
      )
    )
  )
}

builder_build_stage_footer_ui <- function(model, controls_disabled = FALSE) {
  builder_stage_footer_ui(
    builder_build_stage_status_label(model),
    actionButton(
      "back_to_review",
      "Back to Review",
      class = "btn",
      disabled = controls_disabled
    ),
    builder_build_stage_primary_action_ui(
      model,
      controls_disabled = controls_disabled
    )
  )
}

builder_build_workbench_ui <- function(model) {
  stopifnot(is.list(model))
  output_label <- if (isTRUE(model$output$private_app)) {
    "Viewer App"
  } else {
    "CRB files"
  }
  div(
    class = "builder-stage builder-stage-shell builder-stage-build",
    `data-workflow-stage` = "build",
    builder_stage_header_ui(
      "Build",
      "Build your output",
      "Packaging starts only after Build is clicked."
    ),
    builder_stage_summary_ui(
      class = "builder-build-summary",
      span(
        class = "confirmed-plan-revision",
        paste("Confirmed plan revision", model$revision)
      ),
      span(
        paste0(
          model$output$crb_count,
          " dataset",
          if (identical(model$output$crb_count, 1L)) "" else "s"
        )
      ),
      span(output_label)
    ),
    uiOutput("build_output_options"),
    uiOutput("build_stage_controls"),
    div(
      id = "build-stage-status",
      class = "builder-build-stage-status",
      role = "status",
      `aria-live` = "polite",
      `aria-atomic` = "true",
      uiOutput("build_stage_status_content")
    ),
    uiOutput("build_stage_footer")
  )
}
