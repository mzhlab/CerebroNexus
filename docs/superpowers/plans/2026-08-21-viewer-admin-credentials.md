# Viewer Admin credentials implementation plan

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** Allow exported and repository Viewers to configure the built-in Admin account and password while preserving `admin` / `admin123` defaults.

**架构：** Store the credential pair only in server-side `Cerebro.options`. A focused Admin credential helper validates and resolves that pair, and both standalone Admin login and optional Viewer authentication call it. `createShinyApp()` writes explicit arguments into generated configuration; repository `inst/app.R` reads R options.

**技术栈：** R, Shiny, shinymanager, roxygen2, testthat

---

## File structure

- `inst/viewer/admin/core.R`: validate, normalize, and verify the built-in credential pair.
- `inst/app.R`: map repository-launch R options into `Cerebro.options`.
- `R/createShinyApp.R`: expose, validate, and serialize export arguments.
- `inst/viewer/auth.R`: reuse the shared configured credential pair in shinymanager authentication.
- `tests/testthat/test-viewer-admin-ui.R`: credential helper and repository option tests.
- `tests/testthat/test-createShinyApp-auth.R`: export argument validation and propagation tests.
- `tests/testthat/test-viewer-auth-runtime.R`: configured built-in account authentication test.
- `man/createShinyApp.Rd`: generated public parameter documentation.

### Task 1: Define the credential contract

**Files:**
- Modify: `tests/testthat/test-viewer-admin-ui.R`
- Modify: `inst/viewer/admin/core.R`

- [ ] Add tests asserting default values, custom values, rejection of empty/non-scalar values, and constant server-side verification.
- [ ] Run `Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-viewer-admin-ui.R")'` and confirm failure because the resolver does not exist.
- [ ] Implement `viewer_admin_credentials(config)` and make `viewer_admin_default_login()` consume its resolved pair.
- [ ] Re-run the focused test and confirm it passes.

### Task 2: Configure repository Viewer launches

**Files:**
- Modify: `tests/testthat/test-viewer-admin-ui.R`
- Modify: `inst/app.R`

- [ ] Add a source-contract test for `cerebro.admin.account` and `cerebro.admin.password` option reads with `admin` / `admin123` defaults.
- [ ] Run the Admin test and confirm failure because `inst/app.R` has no option mapping.
- [ ] Add `admin_account` and `admin_password` keys to repository `Cerebro.options`, sourced from `getOption()`.
- [ ] Re-run the Admin test and confirm it passes.

### Task 3: Configure exported Viewers

**Files:**
- Modify: `tests/testthat/test-createShinyApp-auth.R`
- Modify: `R/createShinyApp.R`
- Modify: `man/createShinyApp.Rd`

- [ ] Add tests for formal defaults, invalid empty/non-scalar input, and custom values in generated `cerebro_config.rds`.
- [ ] Run the createShinyApp auth test and confirm failure on missing formals/config keys.
- [ ] Add `admin_account = "admin"` and `admin_password = "admin123"`, validate them as non-empty scalar strings, and serialize both into `Cerebro.options`.
- [ ] Run `devtools::document()` to update `man/createShinyApp.Rd`.
- [ ] Re-run the focused export test and confirm it passes.

### Task 4: Unify optional Viewer authentication

**Files:**
- Modify: `tests/testthat/test-viewer-auth-runtime.R`
- Modify: `inst/viewer/auth.R`

- [ ] Change the runtime test to authenticate a custom built-in account from `Cerebro.options` and reject the old default.
- [ ] Run the runtime test and confirm failure because authentication remains hard-coded.
- [ ] Remove `.viewer_auth_default_login`; resolve the pair with `viewer_admin_credentials(Cerebro.options)` and construct the shinymanager checker from it.
- [ ] Re-run the runtime test and confirm it passes.

### Task 5: Final verification and delivery

**Files:**
- Verify all files above.

- [ ] Run `git diff --check`.
- [ ] Run the three focused test files together and confirm zero failures.
- [ ] Commit with `feat(viewer): configure admin credentials`.
- [ ] Restart the App on port 3939 with custom R options and confirm `/admin` returns HTTP 200.
- [ ] Confirm the custom password succeeds through the browser login and the default password fails.
