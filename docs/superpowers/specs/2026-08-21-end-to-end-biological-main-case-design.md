# End-to-end biological main case

## Goal

Ship one reproducible biological walkthrough that starts with a real immune
repertoire, identifies an expanded clone in Linked views, explains its
sequence-level context in HLA & TCR Motifs, and produces a portable cell
selection that can be turned into an anonymous share link.

The case uses the bundled real 10x dextramer cohort. It does not change Viewer
behaviour or add a new analysis method.

## Why this case is suitable

`demo_hla_tcr_dextramer.crb` contains 12,000 measured CD8 T cells from four
donors, paired alpha/beta TCR contigs, transcriptomes, raw dextramer binder
calls, and independently published HLA genotypes. The existing build pipeline
already records the ascertainment and cross-reactivity limitations.

The original candidate, all 344 cells carrying TRB CDR3
`CASSIRSSYEQYF`, is not a valid Linked views clone: Linked views follows the
application-wide `cloneCall = "gene"` contract and groups cells by `CTgene`,
whereas HLA & TCR Motifs groups nodes by TRB CDR3. The main case therefore uses
one CTgene clone as the shareable selection and treats its CDR3 motif as
sequence context.

## Golden selection

The stable Linked views selection is the CTgene clone:

```text
TRAV27.TRAJ42.TRAC_TRBV19.None.TRBJ2-7.TRBC2
```

On the shipped data it contains:

- 293 cells: 142 from donor1 and 151 from donor2;
- 293 `Flu-MP_Influenza` raw binder calls;
- 293 cells whose reagent restriction is present in the donor genotype;
- 10 distinct TRB CDR3 sequences;
- dominant TRB CDR3 `CASSIRSSYEQYF` in 227 cells.

The dominant sequence belongs to a Hamming-distance-1 family with consensus
`CASSxxxxxEQxF`. On the shipped all-cell TRB graph, that family contains 34
CDR3 nodes and 627 cells across donor1, donor2, and donor4. The generated
artifacts identify the family by its anchor sequence and member sequences, not
by the graph's arbitrary numeric motif id.

## Biological interpretation boundary

The walkthrough may state that:

- a gene-defined expanded clone is visible across the linked clone and UMAP
  spaces;
- the clone contains several closely related TRB CDR3 sequences;
- the dominant sequence sits in a convergent Hamming-1 family observed across
  donors in this antigen-selected repertoire.

The walkthrough must not state that:

- `CTgene` is an exact sequence-defined clonotype;
- a raw dextramer binder call proves peptide specificity;
- the motif proves HLA restriction or a population-level HLA association;
- the four-donor selected cohort is an unbiased repertoire.

## Versioned artifacts

### Case manifest

Create
`inst/extdata/examples/demo_hla_tcr_main_case.expectations.json` with:

- case and source-dataset identifiers;
- the CTgene key and clone-call contract;
- the selected cell barcodes in dataset order;
- selected-cell, donor, binder, restriction, and CDR3 counts;
- the dominant TRB CDR3;
- motif anchor, consensus, sorted member sequences, node count, cell count, and
  donor counts;
- concise machine-readable interpretation and non-claim statements.

### Portable Linked views configuration

Create `inst/extdata/examples/demo_hla_tcr_main_case.linked-view.json` as a
normal schema-version-1 configuration. It selects exactly the manifest's 293
barcodes, colours by `sample`, displays UMAP plus the TCR clone space, uses the
rank-stack clone layout, and carries a box geometry around the selected clone
column. The configuration must pass `cv_config_decode()` against all 12,000
dataset cells.

The checked-in JSON is durable. A live share URL is deliberately not checked
in because share records expire after 90 days; the Viewer can create a fresh
anonymous URL from the portable JSON whenever needed.

## Deterministic generator

Create `data-raw/build_hla_tcr_main_case.R`. It reads the shipped `.crb`, sources
the existing clone and configuration contracts, derives the golden selection,
builds the all-cell TRB motif graph with the existing motif core, and writes
both JSON artifacts in canonical order.

The generator stops if the target clone disappears, any declared count drifts,
the target's CDR3 members no longer belong to the anchored motif family, or the
portable configuration fails normalization. It does not download data or
rewrite the `.crb`.

## Walkthrough

Create `vignettes/hla_tcr_main_case.Rmd` and add it to the Module guides in
`_pkgdown.yml`. The walkthrough covers:

1. load the bundled `HLA & TCR` data set;
2. import the checked-in Linked views JSON;
3. inspect the 293-cell selection in UMAP and rank-stack clone views;
4. explain the gene-defined clone and its ten TRB CDR3 sequences;
5. locate the anchored sequence family in HLA & TCR Motifs;
6. distinguish measured convergence from unsupported specificity or
   restriction claims;
7. create a fresh `Share selection` link and open it anonymously;
8. recover the case later from the checked-in JSON after a live URL expires.

The guide links to `hla_tcr_antigen_selected.Rmd` for raw-data provenance and
build details instead of duplicating that material.

## Verification

Add `tests/testthat/test-hla-tcr-main-case.R`. It independently reads the
shipped `.crb` and checks that:

- both artifacts exist and parse;
- the manifest selection equals the CTgene-derived cell set;
- every declared count, dominant sequence, and motif member set is current;
- the portable configuration selects exactly the manifest barcodes;
- the configuration validates against the current dataset fingerprint;
- the configuration uses `sample`, UMAP, and rank-stack layout;
- the vignette contains the interpretation limits and share/recovery path;
- `_pkgdown.yml` publishes the walkthrough.

The focused test is the acceptance gate. Existing linked-view configuration and
HLA motif tests remain unchanged and are run once after the new test is green.

## Out of scope

- changing clone-call semantics;
- synchronising selections between separate Viewer tabs;
- committing a live share token or SQLite row;
- adding marker-gene or differential-expression claims;
- adding screenshots before the textual and data contracts are stable.
