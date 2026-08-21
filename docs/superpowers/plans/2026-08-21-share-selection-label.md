# Share selection label 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 将 Linked views 配置入口的可见文案固定为 `Share selection`。

**架构：** 仅修改服务端生成的按钮文本，并同步现有源码契约测试。保留按钮可用性、辅助标题、对话框内容及全部分享行为。

**技术栈：** R、Shiny、testthat

---

### 任务 1：更新固定入口文案

**文件：**
- 修改：`tests/testthat/test-coordinated-views-config.R`
- 修改：`inst/viewer/coordinated_views/UI.R`

- [x] **步骤 1：编写失败的测试**

将现有入口文案契约改为：

```r
expect_match(ui, 'tags$span("Share selection")', fixed = TRUE)
expect_false(grepl('tags$span("Share views")', ui, fixed = TRUE))
```

- [x] **步骤 2：运行测试验证失败**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'
```

预期：FAIL，因为 UI 仍包含 `tags$span("Share views")`。

- [x] **步骤 3：编写最少实现代码**

将按钮的可见标签改为：

```r
tags$span("Share selection")
```

- [x] **步骤 4：运行测试验证通过**

再次运行步骤 2 的命令，预期：PASS。

- [x] **步骤 5：Commit**

```bash
git add docs/superpowers/plans/2026-08-21-share-selection-label.md \
  tests/testthat/test-coordinated-views-config.R \
  inst/viewer/coordinated_views/UI.R
git commit -m "fix(viewer): label share selection action"
```
