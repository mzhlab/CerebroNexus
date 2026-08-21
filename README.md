<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="man/figures/logo-dark.svg">
    <img src="man/figures/logo.svg" alt="CerebroNexus" width="380">
  </picture>
</p>

[![R-CMD-check (upstream)](https://github.com/mihem/CerebroNexus/actions/workflows/R-cmd-check.yaml/badge.svg)](https://github.com/mihem/CerebroNexus/actions/workflows/R-cmd-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Lifecycle: stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)


CerebroNexus is a [Shiny](https://shiny.posit.co/) platform for exploring and sharing single-cell and spatial transcriptomics data, with gene-expression, immune-repertoire, trajectory, and HLA-TCR analyses. See the [full documentation](https://mihem.github.io/CerebroNexus/).

[Try the live demo](https://osmzhlab.uni-muenster.de/shiny/demo/).

*CerebroNexus began as a fork of [cerebroApp](https://github.com/romanhaa/cerebroApp) by Roman Hillje and has since evolved with substantial new features and active development by [mihem](https://github.com/mihem) and [Xuesong Wang](https://github.com/duocang).*

Automated tests run in a reproducible Nix environment.

![CerebroNexus spatial data view](man/figures/featured.png)

## 1. Installation

```r
remotes::install_github('mihem/CerebroNexus')
```

## 2. Quick Start

For a guided local workflow, launch the Dataset Builder:

```r
library(CerebroNexus)
launchCerebroBuilder()
```

The Builder leads you through **Import and Inspect**, **Core setup**, **Enhance
content**, and **Review and Build**. It reads trusted Seurat objects on the
machine running R, freezes private snapshots for repeatable builds, and can
publish CRBs alone, a public generated app, or a login-protected generated app
with multiple local accounts. Review the
planned payload targets and snapshot-based disk estimate before anything is
replaced. The published release also adds `build-report.json` and the
`.cerebro-builder-release-v1` ownership record.

See [Build a data set without writing
code](https://mihem.github.io/CerebroNexus/articles/build_a_data_set_by_pointing.html)
for the complete workflow, examples, privacy boundary, and recovery model.

For a scripted export, use the package API directly:

```r
library(CerebroNexus)

convertSeuratToCerebro(
  seurat_file = "my_seurat.rds",
  result_dir = "output",
  groups = c("sample", "cluster")
)

createShinyApp(
  cerebro_data = c("My dataset" = "output/cerebro_my_seurat.crb"),
  result_dir = "my_app"
)
```

## 3. Linked views

Open **Linked views** in a Viewer to compare the expression embedding, spatial
or Trekker coordinates, and immune-repertoire projections for the same cells.
A selection made in any selectable two-dimensional panel is applied across the
workspace, so the cohort can be inspected in each modality without exporting or
recreating it.

The enhanced workspace can also save, import, export, and share a validated
configuration. Shared links are read-only; authenticated administrators can
review and revoke them without publishing a data bundle.

## License

MIT, see [LICENSE.md](LICENSE.md). 
