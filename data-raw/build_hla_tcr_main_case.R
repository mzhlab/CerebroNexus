#!/usr/bin/env Rscript

## Build the durable artifacts for the real HLA/TCR biological main case.
##
## The source .crb is already a checked and published demo. This script does
## not modify it: it derives one CTgene-defined selection, its sequence-level
## motif context, and a normal Linked views schema-v1 JSON that can recreate
## the selection after a live 90-day share URL has expired.

command <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", command, value = TRUE)
script_file <- if (length(file_argument)) {
  sub("^--file=", "", file_argument[[1L]])
} else {
  "data-raw/build_hla_tcr_main_case.R"
}
repository_root <- normalizePath(
  file.path(dirname(script_file), ".."),
  mustWork = TRUE
)
if (!file.exists(file.path(repository_root, "DESCRIPTION"))) {
  stop("run this script from a CerebroNexus source checkout", call. = FALSE)
}

pkgload::load_all(repository_root, quiet = TRUE)

source_file <- file.path(
  repository_root,
  "inst/extdata/examples/demo_hla_tcr_dextramer.crb"
)
manifest_file <- file.path(
  repository_root,
  "inst/extdata/examples/demo_hla_tcr_main_case.expectations.json"
)
linked_view_file <- file.path(
  repository_root,
  "inst/extdata/examples/demo_hla_tcr_main_case.linked-view.json"
)
config_contract_file <- file.path(
  repository_root,
  "inst/viewer/coordinated_views/config.R"
)

target_ctgene <- paste0(
  "TRAV27.TRAJ42.TRAC_",
  "TRBV19.None.TRBJ2-7.TRBC2"
)
anchor_cdr3 <- "CASSIRSSYEQYF"

counts_as_list <- function(values) {
  counts <- table(values)
  counts <- counts[order(names(counts))]
  stats::setNames(as.list(as.integer(counts)), names(counts))
}

counts_as_vector <- function(values) {
  counts <- counts_as_list(values)
  stats::setNames(vapply(counts, as.integer, integer(1)), names(counts))
}

annotate_repertoire <- function(repertoire, metadata) {
  sample_names <- names(repertoire)
  output <- lapply(seq_along(repertoire), function(index) {
    frame <- repertoire[[index]]
    metadata_index <- match(frame$barcode, metadata$cell_barcode)
    additions <- setdiff(
      colnames(metadata),
      c("cell_barcode", colnames(frame))
    )
    for (column in additions) {
      frame[[column]] <- metadata[[column]][metadata_index]
    }
    frame$sample <- sample_names[[index]]
    frame
  })
  names(output) <- sample_names
  output
}

crb <- readRDS(source_file)
cells <- as.character(crb$getCellNames())
metadata <- crb$getMetaData()
repertoire <- crb$getImmuneRepertoire()
repertoire_rows <- do.call(rbind, repertoire)
selected_set <- as.character(repertoire_rows$barcode[
  repertoire_rows$CTgene == target_ctgene
])
selected_cells <- cells[cells %in% selected_set]
selected_metadata <- metadata[
  match(selected_cells, metadata$cell_barcode),
  ,
  drop = FALSE
]

annotated_repertoire <- annotate_repertoire(repertoire, metadata)
segments <- CerebroNexus:::hla_parse_ir_segments(annotated_repertoire, "TRB")
selected_segments <- segments[
  segments$barcode %in% selected_cells,
  ,
  drop = FALSE
]
selected_cdr3_counts <- sort(table(selected_segments$cdr3), decreasing = TRUE)

graph <- CerebroNexus:::hla_build_motif_graph(
  segments,
  by_v = FALSE,
  min_nodes = 2L,
  show_isolated = FALSE,
  meta_cols = c(
    "sample",
    "dextramer_antigen",
    "restriction_in_genotype"
  )
)
if (!CerebroNexus:::hla_motif_graph_ok(graph)) {
  stop("the all-cell TRB motif graph is unavailable", call. = FALSE)
}
vertices <- as.data.frame(igraph::vertex.attributes(graph))
anchor_row <- vertices[vertices$cdr3 == anchor_cdr3, , drop = FALSE]
if (nrow(anchor_row) != 1L) {
  stop("the main-case motif anchor is missing or duplicated", call. = FALSE)
}
anchor_cluster <- as.character(anchor_row$cluster[[1L]])
motif_members <- sort(as.character(vertices$cdr3[
  as.character(vertices$cluster) == anchor_cluster
]))
motif_segments <- segments[
  segments$cdr3 %in% motif_members,
  ,
  drop = FALSE
]

clone_sizes <- sort(table(repertoire_rows$CTgene), decreasing = TRUE)
clone_rank <- match(target_ctgene, names(clone_sizes))
clone_size <- unname(as.integer(clone_sizes[[target_ctgene]]))

stopifnot(
  "the source data must contain 12,000 cells" = length(cells) == 12000L,
  "the selected CTgene clone must contain 293 cells" = length(selected_cells) ==
    293L,
  "the selected clone must remain rank six" = clone_rank == 6L,
  "the selected clone size drifted" = clone_size == 293L,
  "the selected clone must span donor1 and donor2" = identical(
    counts_as_vector(selected_segments$sample),
    c(donor1 = 142L, donor2 = 151L)
  ),
  "the selected clone binder labels drifted" = identical(
    counts_as_vector(selected_metadata$dextramer_antigen),
    stats::setNames(293L, "Flu-MP_Influenza")
  ),
  "the selected clone genotype context drifted" = identical(
    counts_as_vector(selected_metadata$restriction_in_genotype),
    c(yes = 293L)
  ),
  "the selected clone must contain ten TRB CDR3 sequences" = length(
    selected_cdr3_counts
  ) ==
    10L,
  "the dominant TRB CDR3 changed" = names(selected_cdr3_counts)[[1L]] ==
    anchor_cdr3,
  "the dominant TRB CDR3 count changed" = unname(selected_cdr3_counts[[1L]]) ==
    227L,
  "the selected CDR3s left the anchored motif" = all(
    names(selected_cdr3_counts) %in% motif_members
  ),
  "the anchored motif node count changed" = length(motif_members) == 34L,
  "the anchored motif cell count changed" = nrow(motif_segments) == 627L,
  "the anchored motif donor distribution changed" = identical(
    counts_as_vector(motif_segments$sample),
    c(donor1 = 308L, donor2 = 318L, donor4 = 1L)
  ),
  "the anchored motif consensus changed" = as.character(anchor_row$motif_consensus[[
    1L
  ]]) ==
    "CASSxxxxxEQxF"
)

config_environment <- new.env(parent = baseenv())
sys.source(config_contract_file, envir = config_environment)
dataset_fingerprint <- config_environment$cv_config_cell_fingerprint(cells)

manifest <- list(
  schema = "cerebronexus-biological-main-case",
  version = 1L,
  case_id = "hla-tcr-expanded-clone",
  title = "From an expanded gene-defined TCR clone to a shareable selection",
  source_dataset = list(
    file = basename(source_file),
    cell_count = length(cells),
    cell_fingerprint = dataset_fingerprint,
    receptor = "TCR",
    selection_context = "antigen-selected"
  ),
  selection = list(
    clone_call = "gene",
    clone_column = "CTgene",
    ctgene = target_ctgene,
    clone_rank = clone_rank,
    cell_count = length(selected_cells),
    cells = unname(selected_cells),
    donor_counts = counts_as_list(selected_segments$sample),
    binder_counts = counts_as_list(selected_metadata$dextramer_antigen),
    restriction_counts = counts_as_list(
      selected_metadata$restriction_in_genotype
    ),
    distinct_trb_cdr3 = length(selected_cdr3_counts),
    dominant_trb_cdr3 = names(selected_cdr3_counts)[[1L]],
    dominant_trb_cdr3_cells = unname(
      as.integer(selected_cdr3_counts[[1L]])
    )
  ),
  motif = list(
    definition = "equal-length TRB CDR3, Hamming distance 1",
    anchor_cdr3 = anchor_cdr3,
    consensus = as.character(anchor_row$motif_consensus[[1L]]),
    node_count = length(motif_members),
    cell_count = nrow(motif_segments),
    donor_counts = counts_as_list(motif_segments$sample),
    member_cdr3 = unname(motif_members)
  ),
  interpretation = list(
    claims = c(
      "The CTgene-defined clone is expanded across donor1 and donor2.",
      "Its ten TRB CDR3 sequences belong to one anchored Hamming-1 family.",
      "The anchored family is observed across donors in this selected cohort."
    ),
    non_claims = c(
      "CTgene is not an exact sequence-defined clonotype.",
      "A raw dextramer binder call does not prove peptide specificity.",
      "The motif does not prove HLA restriction or population association.",
      "The antigen-selected four-donor cohort is not an unbiased repertoire."
    )
  )
)

clone_x <- clone_rank - 1L
linked_view <- list(
  schema = "cerebronexus-linked-view",
  version = 1L,
  created_at = "2026-08-21T00:00:00Z",
  dataset = list(
    cell_count = length(cells),
    cell_fingerprint = dataset_fingerprint
  ),
  selection = list(
    cells = unname(selected_cells),
    source = "Clonal expansion (TCR)",
    geometry = list(
      space = "clone",
      mode = "box",
      polygon = list(
        c(clone_x - 0.45, -0.5),
        c(clone_x + 0.45, -0.5),
        c(clone_x + 0.45, clone_size - 0.5),
        c(clone_x - 0.45, clone_size - 0.5)
      )
    )
  ),
  view = list(
    colour = list(
      mode = "sample",
      gene = NULL,
      rgb_genes = character(),
      clip = 0
    ),
    projections = "umap",
    spatial_sections = character(),
    active_spatial = NULL,
    filters = structure(list(), names = character()),
    hidden_levels = list(),
    display = list(
      percentage_cells = 100,
      point_size = 3,
      point_opacity = 0.8,
      group_labels = TRUE,
      selection_mode = "box",
      clone_layout = "stack"
    ),
    lenses = list(
      list(
        space = "projection::umap",
        viewport = list(cx = 0.5, cy = 0.5, span = 1),
        rotation = NULL
      ),
      list(
        space = "clone",
        viewport = list(cx = 0.5, cy = 0.5, span = 1),
        rotation = NULL
      )
    ),
    spatial_backgrounds = list(),
    trekker = list(
      dissolve_percentage = 0,
      evidence = FALSE,
      niche_radius = 250
    )
  )
)
linked_view <- config_environment$cv_config_normalize(
  linked_view,
  cells = cells
)
linked_view_json <- config_environment$cv_config_encode(linked_view)
decoded <- config_environment$cv_config_decode(linked_view_json, cells = cells)
stopifnot(
  "the generated Linked views selection drifted" = identical(
    decoded$selection$cells,
    selected_cells
  )
)

manifest_json <- jsonlite::toJSON(
  manifest,
  auto_unbox = TRUE,
  null = "null",
  na = "null",
  digits = NA,
  pretty = TRUE
)
writeLines(paste0(as.character(manifest_json), "\n"), manifest_file)
writeLines(linked_view_json, linked_view_file)

message(
  "Published main case: ",
  length(selected_cells),
  " selected cells; ",
  length(motif_members),
  " motif nodes; ",
  nrow(motif_segments),
  " motif cells."
)
