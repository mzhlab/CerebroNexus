## Builder project controls and recovery surfaces.

builder_project_toolbar_ui <- function() {
  tags$div(
    class = "builder-project-toolbar",
    uiOutput("project_status", inline = TRUE),
    actionButton(
      "open_builder_project",
      tagList(shiny::icon("folder-open"), span("Open")),
      class = "btn builder-project-action"
    ),
    actionButton(
      "save_builder_project",
      tagList(shiny::icon("floppy-disk"), span("Save")),
      class = "btn builder-project-action"
    )
  )
}

builder_project_status_ui <- function(
  project = NULL,
  phase = "none",
  source_sync = NULL
) {
  syncing <- is.list(source_sync) && identical(source_sync$status, "syncing")
  sync_failed <- is.list(source_sync) && identical(source_sync$status, "failed")
  sync_ready <- is.list(source_sync) && identical(source_sync$status, "ready")
  label <- if (syncing) {
    paste0(
      "Saving source files · ",
      source_sync$completed %||% 0L,
      " of ",
      source_sync$total %||% 0L
    )
  } else if (sync_failed) {
    "Some source files could not be saved"
  } else if (sync_ready && identical(phase, "clean")) {
    "Project fully saved · Safe to close"
  } else if (is.null(project)) {
    "Not saved"
  } else {
    switch(
      phase,
      dirty = paste0(project$name %||% "Builder project", " · Unsaved changes"),
      choosing = "Choosing project location…",
      saving = "Saving project…",
      restoring = "Restoring project…",
      registering = "Adding reusable CRBs…",
      save_failed = "Project save failed",
      conflict = "Project must be reopened",
      project$name %||% "Saved project"
    )
  }
  status_class <- if (syncing) {
    "is-busy"
  } else if (sync_failed) {
    "is-error"
  } else {
    switch(
      phase,
      dirty = "is-dirty",
      choosing = "is-busy",
      saving = "is-busy",
      restoring = "is-busy",
      registering = "is-busy",
      save_failed = "is-error",
      conflict = "is-error",
      if (is.null(project)) "is-unsaved" else "is-saved"
    )
  }
  tags$span(
    class = paste("builder-project-status", status_class),
    role = "status",
    `aria-live` = "polite",
    tags$span(class = "builder-project-status-dot", `aria-hidden` = "true"),
    tags$span(label)
  )
}

builder_project_dialog_content <- function(primary, secondary, icon) {
  tags$div(
    class = "builder-project-dialog",
    tags$p(class = "builder-project-dialog-lead", primary),
    tags$div(
      class = "builder-project-dialog-summary",
      tags$span(
        class = "builder-project-dialog-icon",
        `aria-hidden` = "true",
        shiny::icon(icon)
      ),
      tags$p(secondary)
    )
  )
}

builder_project_first_save_dialog <- function() {
  modalDialog(
    title = tagList(
      tags$span(class = "builder-project-modal-kicker", "Builder project"),
      tags$span("Save this project")
    ),
    tags$div(
      class = "builder-project-dialog builder-project-first-save-dialog",
      tags$p(
        class = "builder-project-dialog-lead",
        "Choose a folder for your datasets and current Builder settings."
      ),
      tags$div(
        class = "builder-project-dialog-note",
        tags$span(
          class = "builder-project-dialog-icon",
          `aria-hidden` = "true",
          shiny::icon("check")
        ),
        tags$p(
          "Source files are copied into the project so you can continue later."
        )
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(
        "choose_builder_project_folder",
        "Choose folder…",
        class = "btn btn-primary"
      )
    ),
    easyClose = FALSE,
    size = "m"
  )
}

builder_project_nonempty_folder_dialog <- function(path) {
  modalDialog(
    title = "Folder already contains files",
    builder_project_dialog_content(
      "Create the Builder project in this folder?",
      paste(
        "Existing files will be kept. Builder will add its project manifest",
        "and managed data."
      ),
      "exclamation-triangle"
    ),
    tags$p(
      class = "builder-project-dialog-path",
      tags$code(path)
    ),
    footer = tagList(
      actionButton(
        "cancel_builder_project_folder",
        "Cancel",
        class = "btn btn-outline-secondary"
      ),
      actionButton(
        "choose_another_builder_project_folder",
        "Choose another folder",
        class = "btn btn-outline-secondary"
      ),
      actionButton(
        "confirm_builder_project_folder",
        "Create project here",
        class = "btn btn-primary"
      )
    ),
    easyClose = FALSE,
    size = "m"
  )
}

builder_project_restore_row_ui <- function(record, root) {
  status <- builder_project_dataset_status(record, root)
  choices <- character()
  if (status$artifact_ready) {
    choices <- c(
      choices,
      stats::setNames(
        "reuse",
        "Use ready CRB — fast, view/build only"
      )
    )
  }
  if (status$restorable) {
    choices <- c(
      choices,
      stats::setNames(
        "resume",
        "Load source — continue editing"
      )
    )
  }
  choices <- c(
    choices,
    stats::setNames(
      "skip",
      "Skip this dataset for this session"
    )
  )
  selected <- if (!isTRUE(record$release$included %||% TRUE)) {
    "skip"
  } else if (isTRUE(status$checked) && isTRUE(status$artifact_ready)) {
    "reuse"
  } else if (status$restorable) {
    "resume"
  } else if (status$artifact_ready) {
    "reuse"
  } else {
    "skip"
  }
  tags$section(
    class = "builder-project-restore-row",
    tags$div(
      class = "builder-project-restore-summary",
      tags$div(
        tags$h4(record$name %||% record$id)
      ),
      tags$span(
        class = paste(
          "builder-project-restore-badge",
          if (status$artifact_ready) "is-ready" else NULL
        ),
        status$label
      )
    ),
    radioButtons(
      paste0("project_restore_", record$id),
      label = NULL,
      choices = choices,
      selected = selected
    )
  )
}

builder_project_restore_dialog <- function(manifest, root) {
  datasets <- manifest$datasets %||% list()
  modalDialog(
    title = paste0("Open ", manifest$project$name %||% "Builder project"),
    tags$p(
      class = "builder-project-dialog-intro",
      "Choose what Builder should load into memory. Reusable CRBs stay ",
      "lightweight until the next release is assembled. ",
      paste(
        "Skipped datasets remain saved in the project, but are not loaded",
        "or included in the next build."
      )
    ),
    tags$div(
      class = "builder-project-restore-list",
      lapply(datasets, builder_project_restore_row_ui, root = root)
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton(
        "confirm_builder_project_open",
        "Open selected datasets",
        class = "btn btn-primary"
      )
    ),
    easyClose = FALSE,
    size = "l"
  )
}

builder_project_artifact_workbench_ui <- function(entry, root = NULL) {
  artifact <- entry$project_artifact %||% list()
  path <- artifact$path %||% "Saved CRB"
  tags$div(
    class = "builder-stage builder-stage-shell builder-project-artifact-stage",
    `data-workflow-stage` = "configure",
    builder_stage_header_ui(
      "Configure",
      entry$settings$name %||% entry$id,
      "This dataset is represented by a checked CRB and is not loaded into memory."
    ),
    builder_stage_section_ui(
      "Reusable output",
      tags$div(
        class = "builder-project-artifact-card",
        tags$span(class = "builder-project-artifact-mark", "CRB"),
        tags$div(
          tags$strong("Ready for the next release"),
          tags$p(path)
        )
      ),
      description = paste(
        "Builder will copy this CRB into the next project package unless",
        "you load the source and change its settings."
      )
    ),
    uiOutput("configure_actions")
  )
}

builder_project_artifact_actions_ui <- function(message, can_continue) {
  builder_stage_footer_ui(
    message,
    actionButton(
      "project_resume_current_source",
      "Load source to edit",
      class = "btn"
    ),
    actionButton(
      "continue_to_review",
      "Continue to Review",
      class = "btn btn-action",
      disabled = !isTRUE(can_continue)
    )
  )
}
