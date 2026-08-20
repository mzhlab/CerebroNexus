# Shared Linked View Selection Geometry 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 分享 URL 直接打开 Linked views，并恢复创建者实际绘制的选区多边形且不弹出管理窗口。

**架构：** 在现有 v1 配置的 `selection` 中加入必需的 `geometry` 记录，服务器负责严格规范化，浏览器捕获并恢复来源空间的 `lassoData`。分享 URL 的启动逻辑主动选择 `coordinated_views` 导航项，然后静默请求和应用配置；错误显示在 Linked views 页面内。

**技术栈：** R、Shiny、JavaScript、testthat、现有 Canvas Linked views 引擎。

---

### 任务 1：定义并验证选区几何契约

**文件：**
- 修改：`inst/viewer/coordinated_views/config.R`
- 修改：`tests/testthat/test-coordinated-views-config.R`

- [ ] **步骤 1：编写失败的测试**

在有效配置 fixture 的 `selection` 中加入：

```r
geometry = list(
  space = "umap",
  mode = "lasso",
  polygon = list(list(0.1, 0.2), list(0.8, 0.2), list(0.4, 0.9))
)
```

断言规范化结果逐点保持坐标；再分别断言少于三个点、非有限坐标、未知 mode 和空 space 被拒绝。

- [ ] **步骤 2：运行测试验证失败**

运行：`Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`

预期：FAIL，错误指出 `$.selection.geometry` 是未知字段。

- [ ] **步骤 3：编写最少实现代码**

新增点和多边形规范化函数，并将 selection 允许字段改为：

```r
c("cells", "source", "geometry")
```

规范化输出为：

```r
geometry = list(
  space = cv_config_string(geometry$space, "$.selection.geometry.space"),
  mode = cv_config_choice(geometry$mode, "$.selection.geometry.mode", c("lasso", "box")),
  polygon = normalized_polygon
)
```

- [ ] **步骤 4：运行测试验证通过**

运行相同 testthat 命令，预期 0 FAIL。

- [ ] **步骤 5：Commit**

```bash
git add inst/viewer/coordinated_views/config.R tests/testthat/test-coordinated-views-config.R
git commit -m "feat(viewer): validate shared selection geometry"
```

### 任务 2：捕获并精确恢复原始多边形

**文件：**
- 修改：`inst/viewer/www/coordviews.js`
- 修改：`tests/testthat/test-coordinated-views-config.R`

- [ ] **步骤 1：编写失败的测试**

断言 `captureConfigState()` 从唯一持有 `lassoData` 的面板生成：

```js
geometry: {
  space: selectionPanel.spaceId,
  mode: selectMode === 'box' ? 'box' : 'lasso',
  polygon: selectionPanel.lassoData.map(function (point) { return point.slice(); })
}
```

断言恢复代码直接赋值保存的 polygon，且旧的 `restoreConfigSelectionOutline` 凸包算法不存在。

- [ ] **步骤 2：运行测试验证失败**

运行配置 testthat 文件，预期因 capture 中没有 geometry 而失败。

- [ ] **步骤 3：编写最少实现代码**

新增 `committedSelectionGeometry()` 查找持有 `lassoData` 的来源面板；capture 将其放入 selection。prepare 校验来源 space 存在且是可画选区的二维面板；commit 在 `setSelection()` 后清空其他 outline，并把深拷贝 polygon 赋给来源面板 `lassoData` 后 redraw。

- [ ] **步骤 4：运行测试验证通过**

运行配置 testthat 文件和 `node --check inst/viewer/www/coordviews.js`，预期全部通过。

- [ ] **步骤 5：Commit**

```bash
git add inst/viewer/www/coordviews.js tests/testthat/test-coordinated-views-config.R
git commit -m "feat(viewer): preserve exact shared selection region"
```

### 任务 3：让分享 URL 静默直达 Linked views

**文件：**
- 修改：`inst/viewer/www/coordviews-config.js`
- 修改：`inst/viewer/www/coordviews.css`
- 修改：`tests/testthat/test-coordinated-views-config.R`
- 修改：`tests/testthat/test-coordinated-views-config-browser.R`

- [ ] **步骤 1：编写失败的测试**

源码契约断言 `openShareFromUrl()` 不调用 `openDialog()`，并点击：

```js
document.querySelector('a[data-value="coordinated_views"]').click();
```

浏览器测试使用 `?linked_view=<token>` 启动，断言 `#shiny-tab-coordinated_views.active`、`#cv-config-dialog:not([open])`，以及恢复后的选择数量和来源面板 polygon 坐标。

- [ ] **步骤 2：运行测试验证失败**

运行：

```bash
Rscript -e 'testthat::test_file("tests/testthat/test-coordinated-views-config.R"); testthat::test_file("tests/testthat/test-coordinated-views-config-browser.R")'
```

预期：源码测试因 `openDialog()` 失败，浏览器测试因默认页或打开 dialog 失败。

- [ ] **步骤 3：编写最少实现代码**

`openShareFromUrl()` 先激活 Linked views 导航，再发送 share-open，不打开 dialog。新增页面内 `aria-live` 状态容器或复用 Linked views 可见状态区域显示 open/apply 错误；`finishApply()` 返回成功布尔值，仅在成功后清理 URL。

- [ ] **步骤 4：运行测试验证通过**

运行上述 testthat 文件、两个修改 JS 的 `node --check` 和 `git diff --check`，预期全部通过。

- [ ] **步骤 5：Commit**

```bash
git add inst/viewer/www/coordviews-config.js inst/viewer/www/coordviews.css tests/testthat/test-coordinated-views-config.R tests/testthat/test-coordinated-views-config-browser.R
git commit -m "feat(viewer): open shared links directly in linked views"
```

### 任务 4：最终回归和运行实例

**文件：**
- 验证：上述全部修改文件

- [ ] **步骤 1：运行聚焦测试**

运行配置、浏览器和 Viewer shell 测试，预期 0 FAIL。

- [ ] **步骤 2：运行静态检查**

运行 `node --check`、`git diff --check` 和 `git status --short`，预期无语法或空白错误。

- [ ] **步骤 3：重启 3939**

精确终止监听 3939 的 R PID，再运行：

```bash
Rscript -e 'pkgload::load_all("."); shiny::runApp("inst", port = 3939, launch.browser = FALSE)'
```

以 `curl http://127.0.0.1:3939/` 验证 HTTP 200。

