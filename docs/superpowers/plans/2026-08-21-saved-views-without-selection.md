# Saved views without selection implementation plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** Show data-set-matching Saved views even when no cells are selected.

**架构：** Add the loaded data-set fingerprint to the existing lightweight state summary. Make snapshot filtering read that value instead of invoking selection-dependent configuration capture.

**技术栈：** JavaScript, Shiny, shinytest2, testthat

---

### Task 1: Reproduce the missing Saved views

**Files:**
- Modify: `tests/testthat/test-coordinated-views-config-browser.R`

- [ ] Seed localStorage with a valid snapshot matching the loaded fingerprint.
- [ ] Clear the current selection, open Share views, and assert the saved row is visible.
- [ ] Run the focused browser test and confirm the assertion fails because the list is empty.

### Task 2: Remove the selection dependency

**Files:**
- Modify: `inst/viewer/www/coordviews.js`
- Modify: `inst/viewer/www/coordviews-config.js`
- Modify: `tests/testthat/test-coordinated-views-config.R`

- [ ] Add `datasetFingerprint` to `configStateSummary()`.
- [ ] Make `currentFingerprint()` read `adapter().summary().datasetFingerprint`.
- [ ] Add source-contract assertions for the new data flow.
- [ ] Run the focused unit and browser tests and confirm zero failures.

### Task 3: Deliver

**Files:**
- Verify all files above.

- [ ] Run `git diff --check`.
- [ ] Commit with `fix(viewer): show saved views without selection`.
- [ ] Restart port 3939 and confirm `/admin` and the Viewer return HTTP 200.
