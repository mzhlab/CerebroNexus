##----------------------------------------------------------------------------##
## Build Cerebro data sets from Seurat objects, by pointing and clicking.
##
## Several objects per session: each becomes a .crb, and they are bundled into
## one app with the data set switcher, which is how a lab usually wants to hand
## a project over -- one link, every sample behind a dropdown.
##
## Objects are opened only in an isolated worker process. This is still a tool
## for your own machine or RStudio Server session, not a service to deploy for
## untrusted users.
##----------------------------------------------------------------------------##

library(shiny)

## Serialized Seurat objects can be substantially larger than Shiny's default
## 5 MiB upload limit. Accept up to 10 GiB in the local Builder app.
options(shiny.maxRequestSize = 10 * 1024^3)

`%||%` <- function(a, b) if (is.null(a)) b else a

source("prerequisite.R", local = TRUE)
builder_activate_source_package()
runtime_capability <- builder_runtime_capability()
if (!isTRUE(runtime_capability$available)) {
  stop(runtime_capability$reason, call. = FALSE)
}

source(
  file.path("core", "bundle_path_contract.R"),
  local = TRUE
)
source("publish.R", local = TRUE)
source("app_bundle.R", local = TRUE)
source("report.R", local = TRUE)
source("coordinator.R", local = TRUE)

## runApp() sets the working directory to the app directory.
source("io.R", local = TRUE)
source("project.R", local = TRUE)
source(
  file.path(
    "..",
    "viewer",
    "core",
    "viewer_content_contract.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "viewer",
    "core",
    "spatial_coordinate_contract.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "viewer",
    "core",
    "spatial_coordinate_transform.R"
  ),
  local = TRUE
)
source("spatial.R", local = TRUE)
source(
  file.path(
    "..",
    "viewer",
    "hla_tcr_motifs",
    "core",
    "hla_typing.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "viewer",
    "hla_tcr_motifs",
    "core",
    "hla_motif_core.R"
  ),
  local = TRUE
)
source(
  file.path(
    "..",
    "viewer",
    "hla_tcr_motifs",
    "core",
    "hla_association_core.R"
  ),
  local = TRUE
)
source("manifest.R", local = TRUE)
source("content_tables.R", local = TRUE)
source("content_immune.R", local = TRUE)
source("content_spatial.R", local = TRUE)
source("content.R", local = TRUE)
source("profile.R", local = TRUE)
source("inspect.R", local = TRUE)
source("recommend.R", local = TRUE)
source("adapters.R", local = TRUE)
source("preview.R", local = TRUE)
source("stats.R", local = TRUE)
source("extras.R", local = TRUE)
source("analysis.R", local = TRUE)
source("marker_import.R", local = TRUE)
source("build.R", local = TRUE)
source("state.R", local = TRUE)
source("workflow.R", local = TRUE)
source("loading.R", local = TRUE)
source(file.path("ui", "dataset_rail.R"), local = TRUE)
source("plan.R", local = TRUE)
source(file.path("ui", "inspect_stage.R"), local = TRUE)
source(file.path("ui", "core_stage.R"), local = TRUE)
source(file.path("ui", "marker_import.R"), local = TRUE)
source(file.path("ui", "enhance_stage.R"), local = TRUE)
source(file.path("ui", "review_stage.R"), local = TRUE)
source(file.path("ui", "workflow.R"), local = TRUE)
source(file.path("ui", "build_status.R"), local = TRUE)
source(file.path("ui", "project.R"), local = TRUE)
source("worker.R", local = TRUE)
source("session.R", local = TRUE)
source("spatial_alignment_server.R", local = TRUE)

builder_projection_preview_contract <- function(entry, projections) {
  list(
    dataset = entry$id,
    snapshot_identity = .builder_worker_identity(entry$snapshot),
    group = entry$settings$default_group %||% NULL,
    projections = projections
  )
}

builder_trajectory_preview_contract <- function(entry, trajectories) {
  list(
    dataset = entry$id,
    snapshot_identity = .builder_worker_identity(entry$snapshot),
    trajectories = trajectories
  )
}

builder_preview_cache_hit <- function(cache, id, contract) {
  record <- cache[[id]] %||% NULL
  is.list(record) && identical(record$contract, contract)
}

builder_preview_cache_begin <- function(cache, id, contract) {
  cache[[id]] <- list(
    contract = contract,
    frames = list(),
    status = "pending"
  )
  cache
}

builder_preview_cache_store <- function(cache, id, frames) {
  record <- cache[[id]] %||% list(contract = NULL)
  record$frames <- frames %||% list()
  record$status <- "ready"
  cache[[id]] <- record
  cache
}

builder_preview_cache_frames <- function(cache, id) {
  record <- cache[[id]] %||% NULL
  if (is.list(record)) record$frames %||% list() else list()
}

builder_preview_revision_independent <- function(kind) {
  kind %in%
    c(
      "projection_previews",
      "trajectory_previews",
      "spatial_preview"
    )
}

app_capability <- builder_app_capability()
auth_capability <- builder_auth_capability

## Inline icons: an icon set would be another dependency, and emoji are not
## icons.
icon_svg <- function(path, label = NULL) {
  tags$svg(
    class = "icon",
    viewBox = "0 0 24 24",
    fill = "none",
    stroke = "currentColor",
    `stroke-width` = "2",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    `aria-hidden` = if (is.null(label)) "true" else NULL,
    role = if (is.null(label)) NULL else "img",
    if (!is.null(label)) tags$title(label),
    tags$path(d = path)
  )
}
ICON_PLUS <- "M12 5v14M5 12h14"

## The viewer's wordmark, inlined. The builder and the viewer are one product
## and should look like it; inlining avoids a resource path that would have to
## resolve both from the installed package and from a source checkout.
cerebro_wordmark <- local({
  candidates <- c(
    system.file(
      "viewer/www/cerebronexus.svg",
      package = "CerebroNexus"
    ),
    file.path("..", "viewer", "www", "cerebronexus.svg")
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(hit)) {
    HTML(paste(readLines(hit[1], warn = FALSE), collapse = "\n"))
  } else {
    NULL
  }
})

## Stamp the assets so a browser that already has them does not keep them past
## an upgrade. Shiny serves www/ with caching on, and a cached stylesheet
## against new markup is worse than either alone -- the classes change and the
## rules that give them meaning do not arrive. The stamp is the file's own
## mtime, so it changes exactly when the file does, including while editing.
asset_stamp <- function(file) {
  mt <- tryCatch(file.mtime(file), error = function(e) NA)
  if (is.na(mt)) {
    return("")
  }
  paste0("?v=", as.integer(as.numeric(mt)))
}

builder_stylesheet_files <- c(
  "builder.tokens.css",
  "builder.base.css",
  "builder.layout.css",
  "builder.components.css",
  "builder.features.css"
)

builder_stylesheet_tags <- function(
  files = builder_stylesheet_files
) {
  tagList(lapply(files, function(file) {
    tags$link(
      rel = "stylesheet",
      href = paste0(file, asset_stamp(file.path("www", file)))
    )
  }))
}

builder_example_buttons_ui <- function(examples = builder_example_directory()) {
  div(
    class = "builder-example-directory",
    lapply(examples, function(ex) {
      tags$button(
        class = "btn example-btn",
        type = "button",
        `data-ex` = ex$id,
        `data-label` = ex$label,
        `aria-disabled` = "false",
        tags$span(
          class = "ex-inner",
          tags$span(class = "ex-label", ex$label),
          tags$span(class = "ex-detail", ex$detail)
        )
      )
    })
  )
}

builder_app_acknowledge_build <- function(protocol, request_id) {
  builder_protocol_acknowledge(protocol, request_id)
}

builder_app_build_action <- function(result, id) {
  result <- builder_as_result(result)
  if (identical(result$state, "recovery_required")) {
    return(list(
      type = "fail",
      id = id,
      error = result$error %||%
        result$message %||%
        "Release recovery is required."
    ))
  }
  builder_build_action(result, id)
}

builder_app_settle_release <- function(
  release,
  value,
  .publish = builder_coordinator_publish,
  .abort = builder_coordinator_abort,
  .release_error = builder_release_error_result
) {
  if (
    !is.list(release) ||
      !is.list(release$handle) ||
      !builder_stage_has_text(release$handle$target %||% "")
  ) {
    return(builder_result_failure(
      "The parent release coordinator identity was lost."
    ))
  }
  settled <- builder_coordinator_settle(
    release$handle,
    value,
    .publish = .publish,
    .abort = .abort,
    .recovery = function(target) NULL
  )
  if (!isTRUE(settled$ok)) {
    return(.release_error(settled$error, release$handle$target))
  }
  typed <- try(builder_as_result(settled$value), silent = TRUE)
  if (inherits(typed, "try-error")) {
    return(builder_result_failure(
      "The worker returned an unsupported terminal build result."
    ))
  }
  typed
}

ui <- tagList(
  shiny::bootstrapLib(),
  tags$head(
    builder_stylesheet_tags(),
    tags$script(src = paste0("icons.js", asset_stamp("www/icons.js"))),
    tags$script(src = paste0("stats.js", asset_stamp("www/stats.js"))),
    tags$script(
      src = paste0(
        "builder-spatial-canvas.js",
        asset_stamp("www/builder-spatial-canvas.js")
      )
    ),
    tags$script(src = paste0("builder.js", asset_stamp("www/builder.js"))),
    tags$script(HTML(
      paste0(
        "Shiny.addCustomMessageHandler('builder_copy_text', function(message) { navigator.clipboard.writeText(message.text); });",
        "Shiny.addCustomMessageHandler('builder_click', function(message) { var el = document.getElementById(message.id); if (el && !el.disabled) el.click(); });"
      )
    )),
    tags$title("Cerebro Dataset Builder")
  ),
  div(
    class = "topbar builder-project-header",
    div(
      class = "builder-project-brand",
      div(class = "wordmark", cerebro_wordmark),
      div(
        class = "builder-project-brand-copy",
        h1("Dataset Builder"),
        span("Guided project workspace")
      )
    ),
    uiOutput("busy", inline = TRUE),
    builder_project_toolbar_ui()
  ),
  div(
    id = "builder-worker-status",
    class = "builder-worker-status is-starting",
    role = "status",
    `aria-live` = "polite",
    span(class = "builder-worker-status-dot", `aria-hidden` = "true"),
    span(
      class = "builder-worker-status-copy",
      strong(
        id = "builder-worker-status-title",
        "Starting background workspace…"
      ),
      span(
        id = "builder-worker-status-detail",
        "Loading dataset readers and analysis tools…"
      )
    )
  ),
  uiOutput("workflow_progress"),
  div(
    class = "shell builder-shell",
    div(
      class = "rail",
      div(
        class = "rail-head",
        span("Datasets"),
        span(
          textOutput("ds_count", inline = TRUE),
          uiOutput("rail_undo", inline = TRUE)
        )
      ),
      div(
        id = "ds_list",
        class = "ds-picker",
        div(
          id = "ds_ready_list",
          class = "shiny-html-output",
          builder_dataset_rail_ui(builder_state())
        ),
        div(
          id = "ds_import_list",
          class = "shiny-html-output"
        ),
        div(
          id = "ds_client_import_queue",
          class = "builder-client-import-queue",
          `aria-live` = "polite",
          `aria-relevant` = "additions text"
        )
      ),
      div(
        class = "rail-add",
        div(
          class = "dataset-file-control builder-file-picker builder-file-picker--sidebar",
          tags$input(
            id = "dataset_files",
            name = "dataset_files",
            class = "shiny-input-file builder-upload-transport",
            type = "file",
            accept = paste(
              paste0(
                ".",
                unique(unlist(lapply(builder_formats, `[[`, "extensions")))
              ),
              collapse = ","
            ),
            hidden = "hidden"
          ),
          tags$button(
            id = "choose_local_datasets",
            class = "btn btn-primary dataset-file-button action-button",
            type = "button",
            `data-val` = "0",
            icon_svg(ICON_PLUS),
            span("Choose local datasets…")
          )
        ),
        tags$button(
          id = "builder_upload_datasets",
          class = paste(
            "btn btn-outline-secondary builder-file-trigger",
            "builder-file-trigger--secondary"
          ),
          type = "button",
          icon_svg(ICON_PLUS),
          span("Upload through browser…")
        ),
        div(class = "or", "or try an example"),
        builder_example_buttons_ui(),
        uiOutput("add_error")
      )
    ),
    div(
      id = "builder-workspace",
      class = "builder-content",
      div(
        id = "pane",
        div(
          id = "workbench",
          class = "shiny-html-output",
          tabindex = "-1",
          builder_empty_workbench_ui()
        )
      )
    ),
  ),
  div(
    id = "builder-operation-overlay",
    class = "builder-operation-overlay",
    role = "status",
    `aria-live` = "assertive",
    `aria-hidden` = "true",
    `aria-labelledby` = "builder-operation-overlay-title",
    `aria-describedby` = paste(
      "builder-operation-overlay-message",
      "builder-operation-overlay-detail"
    ),
    tabindex = "-1",
    div(
      class = "builder-operation-overlay-card",
      span(
        class = "builder-operation-overlay-icon",
        `aria-hidden` = "true",
        span(class = "spinner"),
        span(class = "builder-operation-success-mark", "✓"),
        span(class = "builder-operation-error-mark", "!")
      ),
      div(
        class = "builder-operation-overlay-copy",
        strong(
          id = "builder-operation-overlay-title",
          "Working on your Builder project"
        ),
        span(
          id = "builder-operation-overlay-message",
          "Keep this page open."
        ),
        span(
          id = "builder-operation-overlay-detail",
          class = "builder-operation-overlay-detail"
        )
      ),
      div(
        id = "builder-operation-overlay-actions",
        class = "builder-operation-overlay-actions",
        `aria-hidden` = "true"
      )
    )
  ),
  builder_auth_dialog_ui(),
  builder_marker_dialog_ui(),
  div(
    id = "builder-live-status",
    class = "visually-hidden",
    role = "status",
    `aria-live` = "polite",
    `aria-atomic` = "true"
  ),
  div(
    class = "builder-first-run",
    `data-first-run` = "true",
    role = "region",
    `aria-label` = "Getting started",
    h2("Build your first Viewer in four steps"),
    tags$ol(
      tags$li("Upload a dataset or choose a bundled example."),
      tags$li("Set up what each CRB retains from the dataset."),
      tags$li("Review the exact output."),
      tags$li("Build the selected artifacts.")
    ),
    tags$button(
      type = "button",
      class = "btn builder-first-run-dismiss",
      "Got it"
    )
  )
)

server <- function(input, output, session) {
  for (.builder_server_source in c(
    "server/foundation.R",
    "server/imports.R",
    "server/datasets.R",
    "server/enhancements.R",
    "server/review.R",
    "server/workflow.R",
    "server/build.R",
    "server/project.R"
  )) {
    source(.builder_server_source, local = TRUE)
  }
  rm(.builder_server_source)
}

shinyApp(ui, server)
