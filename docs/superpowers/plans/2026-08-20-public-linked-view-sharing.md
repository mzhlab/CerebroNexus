# Public Linked View Sharing 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 让匿名、普通登录和管理员 Viewer 用户都能创建 Linked views 分享链接，同时保持 `/admin` 管理操作仅限管理员。

**架构：** 客户端分享区只依赖 Linked views 是否已准备好，不再依赖 Admin capability。服务端创建分支沿用现有配置校验、SQLite 存储和创建者审计，但移除管理员授权门槛；Admin 列表与撤销处理不变。

**技术栈：** R/Shiny、浏览器 JavaScript、SQLite/DBI、testthat、shinytest2。

---

## 文件结构

- 修改 `tests/testthat/test-coordinated-views-config.R`：声明公开创建与管理员专属管理的静态回归契约。
- 修改 `tests/testthat/test-coordinated-views-config-browser.R`：声明匿名 Viewer 中分享区域可见、可触发创建的浏览器契约。
- 修改 `inst/viewer/coordinated_views/UI.R`：让独立分享区默认可见并更新公开分享文案。
- 修改 `inst/viewer/www/coordviews-config.js`：移除 Admin capability 对分享区与创建动作的控制，区分准备和创建状态。
- 修改 `inst/viewer/coordinated_views/server.R`：允许所有会话创建，同时保留可选用户名审计。

### 任务 1：公开分享权限契约与最小实现

**文件：**
- 修改：`tests/testthat/test-coordinated-views-config.R`
- 修改：`inst/viewer/coordinated_views/UI.R`
- 修改：`inst/viewer/www/coordviews-config.js`
- 修改：`inst/viewer/coordinated_views/server.R`

- [ ] **步骤 1：编写失败的权限契约测试**

将原有 Admin-only 断言改为以下行为：

```r
expect_false(grepl('hidden = "hidden"', share_section, fixed = TRUE))
expect_false(grepl("shareAdminAllowed", controller, fixed = TRUE))
expect_false(grepl("viewer_admin_capability", controller, fixed = TRUE))
expect_false(grepl("viewer_is_admin(session)", create_branch, fixed = TRUE))
expect_match(controller, "Preparing view…", fixed = TRUE)
expect_match(controller, "Creating share link…", fixed = TRUE)
```

- [ ] **步骤 2：记录但不执行定向红灯命令**

按用户要求不做本地验证。正常验证命令应为：

```bash
Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-coordinated-views-config.R")'
```

预期在实现前因分享区仍隐藏、客户端仍读取 Admin capability、服务端仍调用 `viewer_is_admin(session)` 而失败。

- [ ] **步骤 3：实现公开分享 UI 与客户端行为**

在 `UI.R` 中移除分享 section 的 `hidden = "hidden"`，并使用明确文案：任何持链接者可只读打开、90 天过期、管理员可在 Admin 管理。

在 `coordviews-config.js` 中删除 `shareAdminAllowed` 和 `receiveShareCapability()`；`renderShareResult()` 始终显示分享区，按钮只依赖 `exportBusy || sharePreparing || pendingShare || !exportReady`；`sendShare()` 不再拒绝非管理员；准备缓存开始时显示 `Preparing view…`，真正发送时显示 `Creating share link…`；不再注册 `viewer_admin_capability` handler。

- [ ] **步骤 4：实现服务端公开创建**

删除 `share_create` 分支中的：

```r
if (!viewer_is_admin(session)) {
  cv_share_abort("forbidden", "Administrator access is required.")
}
```

保留：

```r
creator = viewer_auth_context(session)$user %||% ""
```

因此匿名用户记录空字符串，登录用户记录用户名；配置校验、数据集指纹、SQLite 写入、token 和过期策略不变。

- [ ] **步骤 5：记录但不执行绿灯命令**

按用户要求不运行。正常验证命令与步骤 2 相同，预期 PASS。

- [ ] **步骤 6：提交权限实现**

```bash
git add tests/testthat/test-coordinated-views-config.R \
  inst/viewer/coordinated_views/UI.R \
  inst/viewer/www/coordviews-config.js \
  inst/viewer/coordinated_views/server.R
git commit -m "feat(viewer): allow public linked view sharing"
```

### 任务 2：浏览器回归契约

**文件：**
- 修改：`tests/testthat/test-coordinated-views-config-browser.R`

- [ ] **步骤 1：更新匿名 Viewer 浏览器测试**

在现有无认证启动场景中断言：

```r
app$wait_for_js("document.getElementById('cv-config-share') && !document.getElementById('cv-config-share').hidden")
app$expect_js("document.getElementById('cv-share-create').disabled === false")
app$click("cv-share-create")
app$wait_for_js("document.getElementById('cv-config-status').textContent.indexOf('Administrator access is required') === -1")
```

保留现有分享结果断言，使测试覆盖匿名创建、SQLite 返回 token 与 Copy link 呈现。

- [ ] **步骤 2：记录但不执行浏览器验证**

按用户要求不运行。正常验证命令应为：

```bash
Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-coordinated-views-config-browser.R")'
```

预期 PASS，且 Admin 页面权限测试仍由 `test-viewer-admin-ui.R` 覆盖。

- [ ] **步骤 3：提交浏览器契约**

```bash
git add tests/testthat/test-coordinated-views-config-browser.R
git commit -m "test(viewer): cover anonymous linked view sharing"
```

## 最终审查

- [ ] 对照设计确认 Viewer 创建无需认证、Admin 管理仍需管理员、SQLite 与 90 天过期未改变。
- [ ] 检查 diff 中没有修改 Admin server 的授权判断、认证数据库或分享表结构。
- [ ] 明确交付状态为“测试未按用户要求执行”，不声称验证通过。
