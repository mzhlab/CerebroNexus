# End-to-end biological main case 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 为真实 HLA/TCR demo 固化一个可重复、可分享、科学边界明确的 293-cell 生物学主案例。

**架构：** 用一个确定性 data-raw 脚本从现有 `.crb` 派生案例 manifest 和标准 Linked views JSON；测试独立重算 CTgene、CDR3 motif 和配置契约；新 vignette 只讲交互式生物学路线并链接现有原始数据来源文档。

**技术栈：** R、R6 Cerebro objects、igraph、jsonlite、testthat、R Markdown、Linked views schema v1

---

### 任务 1：锁定黄金选择和 motif 数据契约

**文件：**
- 创建：`tests/testthat/test-hla-tcr-main-case.R`
- 创建：`data-raw/build_hla_tcr_main_case.R`
- 创建：`inst/extdata/examples/demo_hla_tcr_main_case.expectations.json`
- 创建：`inst/extdata/examples/demo_hla_tcr_main_case.linked-view.json`

- [x] **步骤 1：编写失败的产物契约测试**

测试先定位 bundled `.crb` 和两个预期 JSON。若 JSON 不存在，用
`expect_true(file.exists(...))` 产生明确失败；存在后独立执行以下断言：

```r
target_ctgene <- paste0(
  "TRAV27.TRAJ42.TRAC_",
  "TRBV19.None.TRBJ2-7.TRBC2"
)
selected <- ir_rows$barcode[ir_rows$CTgene == target_ctgene]
selected <- cells[cells %in% selected]

expect_length(selected, 293L)
expect_equal(unname(table(sub("_.*", "", selected))), c(142L, 151L))
expect_identical(manifest$selection$cells, selected)
expect_identical(manifest$selection$clone_call, "gene")
expect_identical(manifest$selection$ctgene, target_ctgene)
```

使用现有 motif core 解析 TRB 并重建全细胞图：

```r
segments <- CerebroNexus:::hla_parse_ir_segments(annotated_ir, "TRB")
graph <- CerebroNexus:::hla_build_motif_graph(
  segments,
  by_v = FALSE,
  min_nodes = 2L,
  show_isolated = FALSE,
  meta_cols = c("sample", "dextramer_antigen", "restriction_in_genotype")
)
vertices <- as.data.frame(igraph::vertex.attributes(graph))
anchor <- "CASSIRSSYEQYF"
anchor_cluster <- as.character(vertices$cluster[vertices$cdr3 == anchor])
members <- sort(vertices$cdr3[as.character(vertices$cluster) == anchor_cluster])

expect_identical(manifest$motif$anchor_cdr3, anchor)
expect_identical(manifest$motif$member_cdr3, members)
expect_length(members, 34L)
```

最后 source `config.R`，用 12,000 个 dataset cells 解码 portable JSON，并
断言 selection、`sample` colour、UMAP 和 rank-stack 布局。

- [x] **步骤 2：运行测试验证失败**

运行：

```bash
Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_file("tests/testthat/test-hla-tcr-main-case.R", reporter = "summary")'
```

预期：FAIL，明确报告两个 main-case JSON 尚不存在。

- [x] **步骤 3：实现确定性生成脚本**

脚本必须：

```r
pkgload::load_all(root, quiet = TRUE)
crb <- readRDS(file.path(root, "inst/extdata/examples/demo_hla_tcr_dextramer.crb"))
cells <- as.character(crb$getCellNames())
metadata <- crb$getMetaData()
repertoire <- crb$getImmuneRepertoire()

annotated_ir <- lapply(seq_along(repertoire), function(index) {
  frame <- repertoire[[index]]
  match_index <- match(frame$barcode, metadata$cell_barcode)
  for (column in setdiff(colnames(metadata), c("cell_barcode", colnames(frame)))) {
    frame[[column]] <- metadata[[column]][match_index]
  }
  frame$sample <- names(repertoire)[index]
  frame
})
names(annotated_ir) <- names(repertoire)
```

从 `target_ctgene` 派生 selected cells；用
`hla_parse_ir_segments()` / `hla_build_motif_graph()` 派生 anchor family；
使用 `cv_config_cell_fingerprint()` / `cv_config_normalize()` /
`cv_config_encode()` 生成标准配置。配置固定为：

```r
view = list(
  colour = list(mode = "sample", gene = NULL, rgb_genes = character(), clip = 0),
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
  trekker = list(dissolve_percentage = 0, evidence = FALSE, niche_radius = 250)
)
```

geometry 使用 `space = "clone"` 和包围 rank-stack 第六列的四点 box；最终
再次 decode，确认所选 barcode 与 manifest 完全一致后才写文件。

- [x] **步骤 4：运行生成器并验证绿灯**

运行：

```bash
Rscript data-raw/build_hla_tcr_main_case.R
Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_file("tests/testthat/test-hla-tcr-main-case.R", reporter = "summary")'
```

预期：生成 manifest 与 portable JSON；focused test 为 0 failures。

- [x] **步骤 5：提交数据契约**

```bash
git add tests/testthat/test-hla-tcr-main-case.R \
  data-raw/build_hla_tcr_main_case.R \
  inst/extdata/examples/demo_hla_tcr_main_case.expectations.json \
  inst/extdata/examples/demo_hla_tcr_main_case.linked-view.json
git commit -m "feat(viewer): add biological main case fixtures"
```

### 任务 2：编写交互式生物学 walkthrough

**文件：**
- 修改：`tests/testthat/test-hla-tcr-main-case.R`
- 创建：`vignettes/hla_tcr_main_case.Rmd`
- 修改：`_pkgdown.yml`

- [x] **步骤 1：添加失败的文档发布测试**

```r
expect_true(file.exists(vignette_file))
guide <- paste(readLines(vignette_file, warn = FALSE), collapse = "\n")
expect_match(guide, "293 cells", fixed = TRUE)
expect_match(guide, "CTgene", fixed = TRUE)
expect_match(guide, "raw binder call", fixed = TRUE)
expect_match(guide, "Share selection", fixed = TRUE)
expect_match(guide, "90 days", fixed = TRUE)
expect_match(pkgdown, "hla_tcr_main_case", fixed = TRUE)
```

- [x] **步骤 2：运行测试验证失败**

运行任务 1 的 focused test。预期：FAIL，因为 vignette 不存在且 pkgdown
尚未列出它。

- [x] **步骤 3：写 walkthrough 并发布到 pkgdown**

Vignette 必须包含：数据和主张边界、导入 portable JSON、Linked views 中
的 293-cell selection、CTgene 与 CDR3 的差异、anchored motif、匿名分享、
90 天到期后的 JSON 恢复。`_pkgdown.yml` 将 `hla_tcr_main_case` 放在
`hla_tcr_antigen_selected` 后面。

- [x] **步骤 4：运行测试验证通过**

运行 focused test，预期 0 failures。

- [x] **步骤 5：提交 walkthrough**

```bash
git add tests/testthat/test-hla-tcr-main-case.R \
  vignettes/hla_tcr_main_case.Rmd _pkgdown.yml
git commit -m "docs(viewer): add biological main case walkthrough"
```

### 任务 3：回归验证和交付

**文件：**
- 验证：上述全部文件

- [ ] **步骤 1：重跑生成器并检查无漂移**

```bash
Rscript data-raw/build_hla_tcr_main_case.R
git diff --exit-code -- \
  inst/extdata/examples/demo_hla_tcr_main_case.expectations.json \
  inst/extdata/examples/demo_hla_tcr_main_case.linked-view.json
```

- [ ] **步骤 2：运行相关回归测试**

```bash
Rscript -e 'pkgload::load_all(".", quiet = TRUE); testthat::test_file("tests/testthat/test-hla-tcr-main-case.R", reporter = "summary"); testthat::test_file("tests/testthat/test-coordinated-views-config.R", reporter = "summary"); testthat::test_file("tests/testthat/test-hla-app-contract.R", reporter = "summary")'
```

预期：全部 0 failures、0 warnings。

- [ ] **步骤 3：检查格式与仓库状态**

```bash
git diff --check
git status --short --branch
```

预期：无格式错误；只有计划进度记录可能尚未提交。
