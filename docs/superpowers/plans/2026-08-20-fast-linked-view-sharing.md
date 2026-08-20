# Fast Linked View Sharing 实现计划

> **面向 AI 代理的工作者：** 必需子技能：使用 superpowers:subagent-driven-development（推荐）或 superpowers:executing-plans 逐任务实现此计划。步骤使用复选框（`- [ ]`）语法来跟踪进度。

**目标：** 用会话级 `prepared_id` 消除分享创建的完整 JSON 二次往返，并让可用的同步剪贴板路径即时反馈。

**架构：** Shiny 会话在 prepare 成功时缓存规范化 JSON，并向浏览器返回随机 `prepared_id`；create 仅解析该 ID 并写现有 SQLite。剪贴板助手先在原始点击事件内同步复制，失败时再使用异步 Clipboard API。

**技术栈：** R/Shiny、JavaScript、SQLite/DBI、testthat、Node.js 契约测试。

---

### 任务 1：prepared_id 契约与服务端缓存

**文件：**
- 修改：`tests/testthat/test-coordinated-views-config.R`
- 修改：`inst/viewer/coordinated_views/server.R`

- [ ] 测试先断言 prepare 响应包含 `prepared_id`，create schema 接受 `prepared_id` 且不接受 `config_json`，create 分支不再调用 `cv_config_decode()`。
- [ ] 按用户要求不运行红灯命令；正常命令为 `Rscript -e 'pkgload::load_all("."); testthat::test_file("tests/testthat/test-coordinated-views-config.R")'`。
- [ ] 在 server 会话作用域创建最多 8 条、5 分钟 TTL 的缓存；随机 ID 使用 `cv_share_token()`；记录绑定 dataset fingerprint。
- [ ] prepare 成功时缓存 JSON 并返回 ID；create 解析 ID、检查过期和 fingerprint 后直接调用 `cv_share_store_create()`。
- [ ] 保留 nonce replay、SQLite schema、分享 token 和 90 天 TTL。

### 任务 2：客户端小载荷与状态

**文件：**
- 修改：`inst/viewer/www/coordviews-config-cache.js`
- 修改：`inst/viewer/www/coordviews-config.js`
- 修改：`tests/testthat/test-coordinated-views-config.R`

- [ ] prepared cache 的成功记录保留 `prepared_id`。
- [ ] `share_create` payload 设置 `prepared_id = prepared.prepared_id`，不再设置 `config_json`。
- [ ] 缺少 ID 时在客户端立即报准备结果无效；准备与创建继续分别显示 `Preparing view…` 和 `Creating share link…`。
- [ ] 扩展 Node 契约输入，使 fresh prepare response 带 ID，并断言缓存返回 ID。

### 任务 3：即时复制

**文件：**
- 修改：`inst/viewer/www/viewer-clipboard.js`
- 修改：`tests/testthat/test-viewer-admin-ui.R`

- [ ] 把 textarea fallback 改为同步布尔返回值，并保存/恢复先前焦点。
- [ ] `copyText()` 首先同步调用 fallback；成功立即 `Promise.resolve(true)`；失败再调用 Clipboard API，保留 500ms 上限。
- [ ] 静态契约断言同步 fallback 位于 Clipboard API 分支之前，并继续覆盖 500ms 上限。
- [ ] 按用户要求不执行 Node、testthat 或浏览器验证。

### 任务 4：审查与提交

- [ ] 只读检查 diff，确认未修改 SQLite schema、分享 token、90 天 TTL、Admin 授权。
- [ ] 提交实现和测试代码，明确报告测试未执行。
