# test-hla-tcr-main-case.R — reproducible end-to-end biological case.

case_inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE),
  normalizePath(system.file(package = "CerebroNexus"), mustWork = FALSE)
)
case_inst <- case_inst_candidates[file.exists(file.path(
  case_inst_candidates,
  "extdata/examples/demo_hla_tcr_dextramer.crb"
))][1]

case_source_file <- file.path(
  case_inst,
  "extdata/examples/demo_hla_tcr_dextramer.crb"
)
case_manifest_file <- file.path(
  case_inst,
  "extdata/examples/demo_hla_tcr_main_case.expectations.json"
)
case_config_file <- file.path(
  case_inst,
  "extdata/examples/demo_hla_tcr_main_case.linked-view.json"
)
case_contract_file <- file.path(
  case_inst,
  "viewer/coordinated_views/config.R"
)

case_root_candidates <- c(
  normalizePath(".", mustWork = FALSE),
  normalizePath("../..", mustWork = FALSE),
  normalizePath(testthat::test_path("../.."), mustWork = FALSE)
)
case_root <- case_root_candidates[file.exists(file.path(
  case_root_candidates,
  "DESCRIPTION"
))][1]
case_vignette_file <- file.path(
  case_root,
  "vignettes/hla_tcr_main_case.Rmd"
)
case_pkgdown_file <- file.path(case_root, "_pkgdown.yml")

case_target_ctgene <- paste0(
  "TRAV27.TRAJ42.TRAC_",
  "TRBV19.None.TRBJ2-7.TRBC2"
)
case_anchor_cdr3 <- "CASSIRSSYEQYF"

case_named_integers <- function(value) {
  out <- vapply(value, as.integer, integer(1))
  out[order(names(out))]
}

case_table_integers <- function(value) {
  case_named_integers(as.list(table(value)))
}

case_annotated_repertoire <- function(crb) {
  repertoire <- crb$getImmuneRepertoire()
  metadata <- crb$getMetaData()
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

test_that("the HLA/TCR main-case artifacts match the shipped biology", {
  expect_true(file.exists(case_source_file))
  expect_true(
    file.exists(case_manifest_file),
    info = "run data-raw/build_hla_tcr_main_case.R"
  )
  expect_true(
    file.exists(case_config_file),
    info = "run data-raw/build_hla_tcr_main_case.R"
  )
  if (!file.exists(case_manifest_file) || !file.exists(case_config_file)) {
    return(invisible(NULL))
  }

  crb <- readRDS(case_source_file)
  cells <- as.character(crb$getCellNames())
  metadata <- crb$getMetaData()
  repertoire <- crb$getImmuneRepertoire()
  rows <- do.call(rbind, repertoire)
  selected_set <- as.character(rows$barcode[rows$CTgene == case_target_ctgene])
  selected <- cells[cells %in% selected_set]
  selected_metadata <- metadata[match(selected, metadata$cell_barcode), ]

  manifest <- jsonlite::fromJSON(
    case_manifest_file,
    simplifyVector = FALSE
  )
  expect_identical(manifest$schema, "cerebronexus-biological-main-case")
  expect_identical(as.integer(manifest$version), 1L)
  expect_identical(manifest$case_id, "hla-tcr-expanded-clone")
  expect_identical(manifest$selection$clone_call, "gene")
  expect_identical(manifest$selection$ctgene, case_target_ctgene)
  expect_identical(
    unlist(manifest$selection$cells, use.names = FALSE),
    selected
  )
  expect_identical(as.integer(manifest$selection$cell_count), 293L)
  expect_identical(length(selected), 293L)
  expect_identical(
    case_named_integers(manifest$selection$donor_counts),
    c(donor1 = 142L, donor2 = 151L)
  )
  expect_identical(
    case_table_integers(selected_metadata$sample),
    c(donor1 = 142L, donor2 = 151L)
  )
  expect_identical(
    case_named_integers(manifest$selection$binder_counts),
    stats::setNames(293L, "Flu-MP_Influenza")
  )
  expect_identical(
    unname(as.integer(table(selected_metadata$dextramer_antigen))),
    293L
  )
  expect_identical(
    names(table(selected_metadata$dextramer_antigen)),
    "Flu-MP_Influenza"
  )
  expect_identical(
    case_named_integers(manifest$selection$restriction_counts),
    c(yes = 293L)
  )
  expect_identical(
    case_table_integers(selected_metadata$restriction_in_genotype),
    c(yes = 293L)
  )

  annotated <- case_annotated_repertoire(crb)
  segments <- CerebroNexus:::hla_parse_ir_segments(annotated, "TRB")
  selected_segments <- segments[segments$barcode %in% selected, , drop = FALSE]
  selected_cdr3 <- sort(table(selected_segments$cdr3), decreasing = TRUE)
  expect_identical(as.integer(manifest$selection$distinct_trb_cdr3), 10L)
  expect_identical(length(selected_cdr3), 10L)
  expect_identical(
    manifest$selection$dominant_trb_cdr3,
    case_anchor_cdr3
  )
  expect_identical(names(selected_cdr3)[[1L]], case_anchor_cdr3)
  expect_identical(
    as.integer(manifest$selection$dominant_trb_cdr3_cells),
    227L
  )
  expect_identical(unname(as.integer(selected_cdr3[[1L]])), 227L)

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
  expect_true(CerebroNexus:::hla_motif_graph_ok(graph))
  vertices <- as.data.frame(igraph::vertex.attributes(graph))
  anchor_row <- vertices[vertices$cdr3 == case_anchor_cdr3, , drop = FALSE]
  expect_equal(nrow(anchor_row), 1L)
  anchor_cluster <- as.character(anchor_row$cluster[[1L]])
  motif_members <- sort(as.character(vertices$cdr3[
    as.character(vertices$cluster) == anchor_cluster
  ]))
  motif_segments <- segments[segments$cdr3 %in% motif_members, , drop = FALSE]

  expect_identical(manifest$motif$anchor_cdr3, case_anchor_cdr3)
  expect_identical(
    unlist(manifest$motif$member_cdr3, use.names = FALSE),
    motif_members
  )
  expect_identical(as.integer(manifest$motif$node_count), 34L)
  expect_identical(length(motif_members), 34L)
  expect_identical(as.integer(manifest$motif$cell_count), 627L)
  expect_identical(nrow(motif_segments), 627L)
  expect_identical(manifest$motif$consensus, "CASSxxxxxEQxF")
  expect_identical(
    case_named_integers(manifest$motif$donor_counts),
    c(donor1 = 308L, donor2 = 318L, donor4 = 1L)
  )
  expect_identical(
    case_table_integers(motif_segments$sample),
    c(donor1 = 308L, donor2 = 318L, donor4 = 1L)
  )
  expect_true(all(unique(selected_segments$cdr3) %in% motif_members))

  config_environment <- new.env(parent = baseenv())
  sys.source(case_contract_file, envir = config_environment)
  config_json <- paste(
    readLines(case_config_file, warn = FALSE),
    collapse = "\n"
  )
  config <- config_environment$cv_config_decode(config_json, cells = cells)
  expect_identical(config$selection$cells, selected)
  expect_identical(config$selection$geometry$space, "clone")
  expect_identical(config$selection$geometry$mode, "box")
  expect_identical(config$view$colour$mode, "sample")
  expect_identical(config$view$projections, "umap")
  expect_identical(config$view$display$clone_layout, "stack")
  expect_identical(
    config$dataset$cell_fingerprint,
    config_environment$cv_config_cell_fingerprint(cells)
  )
})

test_that("the HLA/TCR main case is documented as a published workflow", {
  expect_true(
    file.exists(case_vignette_file),
    info = "the interactive biological walkthrough must be written"
  )
  expect_true(file.exists(case_pkgdown_file))
  if (!file.exists(case_vignette_file) || !file.exists(case_pkgdown_file)) {
    return(invisible(NULL))
  }

  guide <- paste(readLines(case_vignette_file, warn = FALSE), collapse = "\n")
  pkgdown <- paste(readLines(case_pkgdown_file, warn = FALSE), collapse = "\n")
  expect_match(guide, "293 cells", fixed = TRUE)
  expect_match(guide, "CTgene", fixed = TRUE)
  expect_match(guide, case_anchor_cdr3, fixed = TRUE)
  expect_match(guide, "raw binder call", fixed = TRUE)
  expect_match(guide, "Share selection", fixed = TRUE)
  expect_match(guide, "90 days", fixed = TRUE)
  expect_match(
    guide,
    "demo_hla_tcr_main_case.linked-view.json",
    fixed = TRUE
  )
  expect_match(guide, "hla_tcr_antigen_selected", fixed = TRUE)
  expect_match(pkgdown, "hla_tcr_main_case", fixed = TRUE)
})
