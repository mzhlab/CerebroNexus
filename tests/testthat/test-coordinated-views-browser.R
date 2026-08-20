# test-coordinated-views-browser.R — interaction regressions for Linked views.
#
# test-coordinated-views.R covers the bundle builders. Everything here exists
# only in the browser: viewport state, popover dismissal, escaping, the reveal
# of conditional UI. Every case below is a regression that actually shipped and
# that the ~8,000 unit assertions could not see, because none of it is reachable
# without a DOM.
#
# Most cases drive a SYNTHETIC bundle pushed straight at the client instead of a
# demo .crb. That is deliberate: it pins the exact geometry a case needs — two
# projections whose points do not overlap, a column with more levels than a
# legend can hold, a name containing markup — rather than hoping a demo happens
# to contain it. `Shiny.shinyapp.dispatchMessage` is Shiny's own inbound-message
# entry point; it is internal API, so if a Shiny upgrade ever breaks these, the
# fix is here and not in the app.

library(shinytest2)

inst_dir <- system.file(package = "CerebroNexus")

## Open the app on the Linked views tab, ready for a bundle.
cv_app <- function(name) {
  app <- AppDriver$new(
    inst_dir,
    name = name,
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]') !== null",
    timeout = 30000
  )
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]').click();"
  )
  app$wait_for_idle(timeout = 20000)
  app
}

## JS that builds a synthetic bundle and hands it to the client. `extra` is
## merged over the defaults, so each test states only what it cares about.
cv_bundle_js <- function(extra = "{}", n = 800) {
  paste0(
    "(function () {\n",
    "  var n = ",
    n,
    ";\n",
    "  var blob = function (c) { var a = new Array(n);\n",
    "    for (var i = 0; i < n; i++) a[i] = c + (Math.random() - 0.5) * 2;\n",
    "    return a; };\n",
    "  var cells = [], vals = [];\n",
    "  for (var i = 0; i < n; i++) { cells.push('c' + i); vals.push(i % 3); }\n",
    "  var b = {\n",
    "    cells: cells, n: n,\n",
    "    groups: { cluster: { values: vals, levels: ['a', 'b', 'c'],\n",
    "      colors: ['#636EFA', '#EF553B', '#00CC96'] } },\n",
    "    cat_extra: {}, cat_skipped: {}, fields: {},\n",
    "    default_group: 'cluster',\n",
    "    projections: { umap: { x: blob(0), y: blob(0), ndim: 2 } },\n",
    "    default_projection: 'umap',\n",
    "    spaces: [{ id: 'umap', label: 'umap (expression)',\n",
    "      x: blob(0), y: blob(0) }],\n",
    "    clone: null, trekker: null\n",
    "  };\n",
    "  var extra = ",
    extra,
    ";\n",
    "  for (var k in extra) b[k] = extra[k];\n",
    "  Shiny.shinyapp.dispatchMessage(JSON.stringify(\n",
    "    { custom: { coordviews_data: b } }));\n",
    "})();"
  )
}

cv_set_color_js <- function(value) {
  paste0(
    "(function(){var e=document.getElementById('cv-pick-color');",
    "if(e.selectize)e.selectize.setValue(",
    encodeString(value, quote = "'"),
    ");else{e.value=",
    encodeString(value, quote = "'"),
    ";e.dispatchEvent(new Event('change',{bubbles:true}));}})()"
  )
}

cv_refresh_color_js <- paste0(
  "(function(){var e=document.getElementById('cv-pick-color');",
  "if(e.selectize)e.selectize.settings.onChange(e.selectize.getValue());",
  "else e.dispatchEvent(new Event('change',{bubbles:true}));})()"
)

## Fraction of the canvas that has something drawn on it, 0-100. The measure the
## "view left on empty space" regressions are about: a panel can be perfectly
## valid and still show nothing.
cv_ink_js <- function(canvas_id = "cv-cv-a") {
  paste0(
    "(function () {\n",
    "  var cv = document.getElementById('",
    canvas_id,
    "');\n",
    "  var d = cv.getContext('2d')\n",
    "    .getImageData(0, 0, cv.width, cv.height).data;\n",
    "  var k = 0, s = 0;\n",
    "  for (var i = 0; i < d.length; i += 4 * 17) {\n",
    "    s++;\n",
    "    if (d[i + 3] > 0 && (d[i] < 240 || d[i+1] < 240 || d[i+2] < 240)) k++;\n",
    "  }\n",
    "  return Math.round(k / s * 1000) / 10;\n",
    "})();"
  )
}

test_that("multiple spatial sections become independent linked panels", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_multi_spatial")
  on.exit(app$stop(), add = TRUE)

  samples_js <- paste0(
    "['A tissue','B tissue','C tissue'].map(function(name, sampleIndex) {",
    " return { name:name, label:name + ' (spatial)',",
    " x:blob(sampleIndex * 4), y:blob(sampleIndex * 3),",
    " images: sampleIndex === 0 ? [",
    " {id:'rose',label:'Rose H&E',uri:'data:image/png;base64,iVBORw0KGgo=',",
    " bounds:{xmin:-2,xmax:2,ymin:-2,ymax:2},preset:{}},",
    " {id:'blue',label:'Blue H&E',uri:'data:image/png;base64,iVBORw0KGgo=',",
    " bounds:{xmin:-2,xmax:2,ymin:-2,ymax:2},preset:{}}] : [",
    " {id:'embedded',label:'Embedded histology',",
    " uri:'data:image/png;base64,iVBORw0KGgo=',",
    " bounds:{xmin:-2,xmax:2,ymin:-2,ymax:2},preset:{}}] }; })"
  )
  app$run_js(cv_bundle_js(paste0(
    "{ spaces:[",
    " {id:'umap',label:'umap (expression)',x:blob(0),y:blob(0)},",
    " {id:'spatial',label:'A tissue (spatial)',x:blob(0),y:blob(0),",
    "  samples:",
    samples_js,
    "},",
    " {id:'trekker',label:'Physical (Trekker)',x:blob(1),y:blob(2)},",
    " {id:'clone',label:'Clonal expansion (TCR)',x:blob(2),y:blob(1)}",
    "] }"
  )))
  app$wait_for_js(
    paste0(
      "(function(){var e=document.getElementById('cv-pick-spatial');",
      "return e && (e.selectize ? Object.keys(e.selectize.options).length : ",
      "e.options.length) === 3;})()"
    ),
    timeout = 15000
  )

  expect_true(app$get_js(
    "document.getElementById('cv-pick-spatial').multiple"
  ))
  app$run_js(paste0(
    "(function(){var e=document.getElementById('cv-pick-spatial');",
    "e.selectize.clear();e.selectize.focus();e.selectize.open();})()"
  ))
  app$wait_for_js(
    paste0(
      "(function(){var e=document.getElementById('cv-pick-spatial');",
      "return e.selectize.$control_input.attr('placeholder') === 'Select…' && ",
      "e.selectize.$dropdown.is(':visible') && ",
      "!Array.from(document.querySelectorAll('.cv-pane:not(.cv-hidden) .cv-ptitle'))",
      ".some(function(x){return /\\(spatial\\)$/.test(x.textContent.trim());});})()"
    ),
    timeout = 15000
  )
  expect_equal(
    app$get_js("document.querySelectorAll('.cv-pane:not(.cv-hidden)').length"),
    3
  )
  expect_true(app$get_js(paste0(
    "(function(){var e=document.getElementById('cv-pick-spatial');",
    "var d=e.selectize.$dropdown[0], opts=d.querySelectorAll('.option');",
    "return Array.from(opts).every(function(o){return o.scrollWidth <= ",
    "o.clientWidth + 1;}) && d.getBoundingClientRect().right <= ",
    "window.innerWidth + 1;})()"
  )))
  app$run_js(paste0(
    "(function(){var e=document.getElementById('cv-pick-proj');",
    "e.selectize.clear();e.selectize.focus();})()"
  ))
  app$wait_for_js(
    paste0(
      "(function(){var e=document.getElementById('cv-pick-proj');",
      "return e.selectize.items.length===0 && ",
      "!Array.from(document.querySelectorAll('.cv-pane:not(.cv-hidden) .cv-ptitle'))",
      ".some(function(x){return /\\(expression\\)$/.test(x.textContent.trim());});})()"
    ),
    timeout = 15000
  )
  app$run_js(
    "document.getElementById('cv-pick-proj').selectize.setValue(['umap']);"
  )
  app$run_js(paste0(
    "(function(){var e=document.getElementById('cv-pick-spatial');",
    "if(e.selectize){e.selectize.setValue(['A tissue','B tissue','C tissue']);}",
    "else{Array.from(e.options).forEach(function(o){o.selected=true;});",
    "e.dispatchEvent(new Event('change',{bubbles:true}));}})();"
  ))
  app$wait_for_js(
    "document.querySelectorAll('.cv-pane:not(.cv-hidden)').length === 6",
    timeout = 15000
  )

  titles <- unlist(app$get_js(paste0(
    "Array.from(document.querySelectorAll(",
    "'.cv-pane:not(.cv-hidden) .cv-ptitle'))",
    ".map(function(x){return x.textContent.trim();})"
  )))
  expect_equal(
    titles,
    c(
      "umap (expression)",
      "A tissue (spatial)",
      "B tissue (spatial)",
      "C tissue (spatial)",
      "Physical (Trekker)",
      "Clonal expansion (TCR)"
    )
  )
  expect_gte(
    app$get_js("document.querySelectorAll('.cv-panes > .cv-pane').length"),
    6
  )
  expect_true(app$get_js(paste0(
    "Array.from(document.querySelectorAll('.cv-pane:not(.cv-hidden)'))",
    ".every(function(x){return x.getBoundingClientRect().width >= 300;})"
  )))
  expect_equal(
    app$get_js("document.querySelectorAll('.cv-bg-row').length"),
    1
  )
  expect_equal(
    app$get_js("document.querySelectorAll('.cv-bg-row select').length"),
    1
  )
  expect_equal(
    app$get_js(
      "document.querySelectorAll('.cv-bg-space-tab').length"
    ),
    3
  )
  expect_equal(
    app$get_js(
      "document.querySelectorAll('.cv-pane.cv-active-spatial').length"
    ),
    1
  )

  app$stop()
})


test_that("a continuous colouring explains its spatial pattern on each card", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_spatial_moran")
  on.exit(app$stop(), add = TRUE)

  app$run_js(cv_bundle_js(
    paste0(
      "{ fields: { 'meta:signal': { label:'Signal', ",
      "v:Array.from({length:n},function(_,i){return i < n/2 ? 0 : 255;}), ",
      "min:0,max:1,scale:255 } },",
      " spaces:[{id:'umap',label:'umap (expression)',x:blob(0),y:blob(0)},",
      "{id:'spatial',label:'slide (spatial)',",
      "x:Array.from({length:n},function(_,i){return i;}),",
      "y:Array.from({length:n},function(_,i){return (i%5)*0.01;})}] }"
    ),
    n = 80
  ))
  app$wait_for_js(
    "document.querySelector('.cv-pane:not(.cv-hidden) .cv-moran-badge') !== null",
    timeout = 15000
  )
  app$run_js(cv_set_color_js("__field__meta:signal"))
  app$wait_for_js(
    paste0(
      "Array.from(document.querySelectorAll('.cv-pane:not(.cv-hidden)'))",
      ".some(function(p){var t=p.querySelector('.cv-ptitle');",
      "var b=p.querySelector('.cv-moran-badge');",
      "return /slide/.test(t.textContent) && /Moran/.test(b.textContent);})"
    ),
    timeout = 10000
  )
  expect_true(app$get_js(paste0(
    "(function(){var p=Array.from(document.querySelectorAll('.cv-pane'))",
    ".find(function(x){return /slide/.test(x.querySelector('.cv-ptitle').textContent);});",
    "var b=p.querySelector('.cv-moran-badge');",
    "return Number.isFinite(Number(b.dataset.value)) && Number(b.dataset.value) > 0.5;})()"
  )))
  expect_match(
    app$get_js(paste0(
      "Array.from(document.querySelectorAll('.cv-pane')).find(function(x){",
      "return /slide/.test(x.querySelector('.cv-ptitle').textContent);})",
      ".querySelector('.cv-moran-badge').getAttribute('title')"
    )),
    "six nearest spatial neighbours",
    fixed = TRUE
  )

  app$run_js(cv_set_color_js("cluster"))
  app$wait_for_js(
    paste0(
      "Array.from(document.querySelectorAll('.cv-moran-badge'))",
      ".every(function(b){return getComputedStyle(b).display==='none';})"
    ),
    timeout = 5000
  )
})


test_that("a Trekker field explains itself in the contextual readout", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_trekker_field_summary")
  on.exit(app$stop(), add = TRUE)

  app$run_js(cv_bundle_js(paste0(
    "{ fields:{purity:{label:'Spatial purity',source:'trekker',",
    "desc:'Fraction of physical neighbours sharing the cell type.',",
    "v:Array.from({length:n},function(_,i){return (i%3)*120;}),",
    "min:0,max:1,scale:255,by_type:[",
    "{type:'a',median:0.1},{type:'b',median:0.5},{type:'c',median:0.9}] }},",
    "trekker:{evidence:null},",
    "spaces:[{id:'umap',label:'umap',x:blob(0),y:blob(0)},",
    "{id:'trekker',label:'Physical (Trekker)',x:blob(1),y:blob(1)}] }"
  )))
  app$wait_for_js(
    "document.getElementById('cv-pick-color').selectize !== undefined"
  )
  app$run_js(cv_set_color_js("__field__purity"))
  app$wait_for_js(
    "document.querySelector('#cv-readout .cv-field-summary') !== null",
    timeout = 5000
  )
  summary <- app$get_js("document.getElementById('cv-readout').textContent")
  expect_match(summary, "Spatial purity")
  expect_match(summary, "Fraction of physical neighbours")
  expect_match(summary, "Median by cell type")
})

test_that("Trekker insights are discoverable but collapsed on first render", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_trekker_insights_default")
  on.exit(app$stop(), add = TRUE)

  app$run_js(cv_bundle_js(paste0(
    "{ trekker:{qc:{sample_id:'s1'},moran:[],",
    "evidence:Array.from({length:n},function(_,i){return i<3?1:0;})},",
    "spaces:[{id:'umap',label:'umap',x:blob(0),y:blob(0)},",
    "{id:'trekker',label:'Physical (Trekker)',x:blob(1),y:blob(1)}] }"
  )))
  app$wait_for_js(
    "getComputedStyle(document.getElementById('cv-tk-insights')).display !== 'none'",
    timeout = 15000
  )

  expect_equal(
    app$get_js(
      "document.getElementById('cv-tk-insights-toggle').getAttribute('aria-expanded')"
    ),
    "false"
  )
  expect_equal(
    app$get_js(
      "getComputedStyle(document.getElementById('cv-tk-insights-body')).display"
    ),
    "none"
  )
  expect_true(app$get_js("document.getElementById('cv-evidence').checked"))

  app$run_js("document.getElementById('cv-tk-insights-toggle').click();")
  app$wait_for_js(
    "getComputedStyle(document.getElementById('cv-tk-insights-body')).display !== 'none'",
    timeout = 5000
  )
  expect_true(app$get_js(
    "document.getElementById('cv-tk-tab-cell').classList.contains('is-active')"
  ))
})


test_that("the tab renders a pushed bundle and offers every meta column", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_render")

  # A data set whose meta data spans all three colouring sources: registered
  # groups, an unregistered categorical column, a numeric column, and one with
  # too many levels to colour by.
  app$run_js(cv_bundle_js(
    paste0(
      "{ cat_extra: { orig_ident: { values: vals.map(function (v) ",
      "{ return v % 2; }), levels: ['s1', 's2'], ",
      "colors: ['#111111', '#222222'] } },",
      " cat_skipped: { barcode_like: 791 },",
      " fields: { 'meta:nUMI': { label: 'nUMI', v: vals.map(function (v) ",
      "{ return v * 400; }), min: 100, max: 9000, scale: 1000 } } }"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('800 cells') >= 0",
    timeout = 15000
  )

  # every source reaches the picker, and the uncolourable column is listed
  # (disabled) rather than silently dropped
  opts <- app$get_js(paste0(
    "Object.values(document.getElementById('cv-pick-color').selectize.options)",
    ".map(function(o){return (o.disabled?'x:':'')+(o.text||o.label);});"
  ))
  expect_true(any(opts == "cluster"))
  expect_true(any(opts == "orig_ident"))
  expect_true(any(opts == "nUMI"))
  expect_true(any(grepl("^x:barcode_like", opts)))
  expect_true(any(opts == "Gene expression"))

  # the panel actually drew something
  expect_gt(app$get_js(cv_ink_js()), 1)

  app$stop()
})

test_that("a palette patch recolours the current bundle without replacing it", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_palette_patch")
  on.exit(app$stop(), add = TRUE)

  app$run_js(cv_bundle_js("{ dataset_id: 'palette-test' }"))
  app$wait_for_js(
    "document.querySelector('#cv-legend .cv-dot') !== null",
    timeout = 15000
  )
  app$run_js(paste0(
    "Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: { ",
    "coordviews_colors: { dataset_id: 'palette-test', ",
    "groups: { cluster: ['#010203', '#040506', '#070809'] }, ",
    "cat_extra: {} } } }));"
  ))
  app$wait_for_js(
    "getComputedStyle(document.querySelector('#cv-legend .cv-dot')).backgroundColor === 'rgb(1, 2, 3)'",
    timeout = 5000
  )
  expect_identical(
    app$get_js(
      "getComputedStyle(document.querySelector('#cv-legend .cv-dot')).backgroundColor"
    ),
    "rgb(1, 2, 3)"
  )
})

test_that("Trekker insight tabs resize smoothly without losing their anchor", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_trekker_insights_transition")
  on.exit(app$stop(), add = TRUE)
  app$set_window_size(width = 1440, height = 900)

  app$run_js(cv_bundle_js(paste0(
    "{ trekker:{qc:{sample_id:'s1'},moran:[{gene:'G1',i:.4}]},",
    "spaces:[{id:'umap',label:'umap',x:blob(0),y:blob(0)},",
    "{id:'trekker',label:'Physical (Trekker)',x:blob(1),y:blob(1)}] }"
  )))
  app$wait_for_js(
    "getComputedStyle(document.getElementById('cv-tk-insights')).display !== 'none'",
    timeout = 15000
  )
  app$run_js(paste0(
    "document.getElementById('cv-tk-insights-toggle').click();",
    "document.getElementById('cv-tk-panel-cell').style.minHeight='760px';",
    "document.getElementById('cv-tk-panel-qc').style.minHeight='360px';",
    "window.__cvTkAnchorReady=false;",
    "document.getElementById('cv-tk-insights').scrollIntoView(",
    "{behavior:'instant',block:'start'});",
    "requestAnimationFrame(function(){",
    "window.__cvTkAnchor=document.getElementById('cv-tk-insights')",
    ".getBoundingClientRect().top;window.__cvTkAnchorReady=true;});"
  ))
  app$wait_for_js("window.__cvTkAnchorReady === true", timeout = 3000)
  expect_lt(abs(app$get_js("window.__cvTkAnchor")), 36)
  app$run_js("document.getElementById('cv-tk-tab-qc').click();")
  app$wait_for_js(
    "document.getElementById('cv-tk-panel-stage').classList.contains('is-switching')",
    timeout = 3000
  )
  expect_match(
    app$get_js("document.getElementById('cv-tk-panel-stage').style.height"),
    "px$"
  )
  app$wait_for_js(
    "!document.getElementById('cv-tk-panel-stage').classList.contains('is-switching')",
    timeout = 5000
  )
  expect_lt(
    abs(app$get_js(paste0(
      "document.getElementById('cv-tk-insights').getBoundingClientRect().top-",
      "window.__cvTkAnchor"
    ))),
    36
  )
  expect_true(app$get_js(
    "document.getElementById('cv-tk-tab-qc').classList.contains('is-active')"
  ))

  app$stop()
})


test_that("multiple projections become independent responsive linked panels", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_projection_reset")

  # Two projections whose points do not overlap AT ALL: a viewport kept from one
  # necessarily lands on empty space in the other, which is exactly the failure.
  app$run_js(cv_bundle_js(
    paste0(
      "{ projections: { umap: { x: blob(-9), y: blob(-9), ndim: 2 },",
      " tsne: { x: blob(9), y: blob(9), ndim: 2 } },",
      " spaces: [{ id: 'umap', label: 'umap (expression)',",
      " x: blob(-9), y: blob(-9) }] }"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-pick-proj').selectize != null",
    timeout = 15000
  )

  app$run_js(
    "document.getElementById('cv-pick-proj').selectize.setValue(['umap','tsne']);"
  )
  app$wait_for_js(
    paste0(
      "Array.from(document.querySelectorAll('.cv-pane'))",
      ".filter(function(p){return !p.classList.contains('cv-hidden');}).length === 2"
    ),
    timeout = 10000
  )
  expect_equal(
    unlist(app$get_js(paste0(
      "Array.from(document.querySelectorAll('.cv-pane:not(.cv-hidden) .cv-ptitle'))",
      ".map(function(x){return x.textContent;})"
    ))),
    c("umap (expression)", "tsne (expression)")
  )
  expect_true(app$get_js(paste0(
    "Array.from(document.querySelectorAll('.cv-pane:not(.cv-hidden) canvas[id^=cv-cv-]'))",
    ".every(function(x){return x.getBoundingClientRect().width >= 300;})"
  )))

  # zoom in hard with the toolbar (the wheel no longer zooms), and leave a
  # committed lasso behind
  app$run_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  var zin = document.querySelector(\n",
      "    '.cv-tbtn[data-act=\"zin\"][data-panel=\"A\"]');\n",
      "  for (var k = 0; k < 8; k++) zin.click();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 60, clientY: r.top + 60, bubbles: true }));\n",
      "  for (var s = 40; s <= 200; s += 40)\n",
      "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "      { clientX: r.left + 60 + s, clientY: r.top + 60 + s,\n",
      "        bubbles: true }));\n",
      "  window.dispatchEvent(new MouseEvent('mouseup', { bubbles: true }));\n",
      "})();"
    )
  )
  # the minimap only shows while a view is set, so it is the state's readout
  app$wait_for_js(
    "document.getElementById('cv-mini-a').classList.contains('is-on')",
    timeout = 10000
  )

  app$run_js(
    paste0(
      "document.getElementById('cv-pick-proj').selectize.setValue(['tsne']);"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-title-a').textContent.indexOf('tsne') >= 0",
    timeout = 10000
  )

  # view cleared -> minimap gone, and the panel is full of data again rather
  # than showing the old viewport's (now empty) corner
  expect_false(
    app$get_js(
      "document.getElementById('cv-mini-a').classList.contains('is-on');"
    )
  )
  expect_gt(app$get_js(cv_ink_js()), 1)

  app$stop()
})


test_that("group-filter menus open, exclude each other, and dismiss", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_filter_menus")

  app$run_js(cv_bundle_js(
    paste0(
      "{ groups: { cluster: { values: vals, levels: ['a', 'b', 'c'],",
      " colors: ['#636EFA', '#EF553B', '#00CC96'] },",
      " sample: { values: vals.map(function (v) { return (v + 1) % 3; }),",
      " levels: ['s1', 's2', 's3'],",
      " colors: ['#111111', '#222222', '#333333'] } } }"
    )
  ))
  app$wait_for_js(
    "document.querySelectorAll('.cv-filt-btn').length === 2",
    timeout = 15000
  )

  # The drawer scrolls internally, so its menus must still be fully hit-testable.
  app$run_js("document.getElementById('cv-more-btn').click();")
  app$wait_for_js(
    "document.getElementById('cv-more').classList.contains('is-open')",
    timeout = 10000
  )

  # Expressions, not statements: these get wrapped in `(...) === n` for
  # wait_for_js as well as read directly, and a trailing `;` breaks the wrap.
  open_count <- paste0(
    "Array.from(document.querySelectorAll('.cv-filt-menu'))",
    ".filter(function (m) { return getComputedStyle(m).display !== 'none'; })",
    ".length"
  )
  lit_count <- "document.querySelectorAll('.cv-filt-btn.is-open').length"
  # is the menu genuinely hit-testable, or merely display:block somewhere off
  # in a clipped region? elementFromPoint is the only honest answer.
  hittable <- paste0(
    "(function () {\n",
    "  var m = document.querySelector('.cv-filt-menu');\n",
    "  if (!m || getComputedStyle(m).display === 'none') return false;\n",
    "  var r = m.getBoundingClientRect();\n",
    "  var el = document.elementFromPoint(r.left + 20, r.top + 12);\n",
    "  return !!(el && m.contains(el));\n",
    "})();"
  )

  app$run_js(paste0(
    "document.querySelectorAll('.cv-filt-btn')[0]",
    ".scrollIntoView({block:'center',behavior:'auto'});",
    "document.querySelectorAll('.cv-filt-btn')[0].click();"
  ))
  app$wait_for_js(paste0("(", open_count, ") === 1"), timeout = 8000)
  app$wait_for_js(hittable, timeout = 8000)
  expect_true(app$get_js(hittable))
  expect_equal(app$get_js(lit_count), 1)

  # opening the second must REPLACE the first, not add to it
  app$run_js("document.querySelectorAll('.cv-filt-btn')[1].click();")
  app$wait_for_js(
    paste0(
      "document.querySelectorAll('.cv-filt-btn')[1]",
      ".classList.contains('is-open')"
    ),
    timeout = 8000
  )
  expect_equal(app$get_js(open_count), 1)
  expect_equal(app$get_js(lit_count), 1)

  # ticking a level inside the menu must not close it, and must filter
  app$run_js(
    paste0(
      "document.querySelectorAll('.cv-filt-menu')[1]",
      ".querySelectorAll('.cv-filt-item')[0].click();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-shown').textContent.indexOf('showing') >= 0",
    timeout = 8000
  )
  expect_equal(app$get_js(open_count), 1)

  # a click outside dismisses it — closing only via the chip made it a trap
  app$run_js("document.querySelector('.cv-meta').click();")
  app$wait_for_js(paste0("(", open_count, ") === 0"), timeout = 8000)
  expect_equal(app$get_js(lit_count), 0)

  app$stop()
})


test_that("values from the data set cannot inject markup", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_escaping")

  # The payload increments a counter, so "did it run" is a number rather than a
  # judgement about what the DOM looks like.
  app$run_js("window.__cvXss = 0;")
  app$run_js(cv_bundle_js(
    paste0(
      "(function () {\n",
      "  var evil = '<img src=x onerror=\"window.__cvXss=",
      "(window.__cvXss||0)+1\">';\n",
      "  var g = {}; g['grp' + evil] = { values: vals,\n",
      "    levels: ['lvl' + evil, 'ok', 'fine'],\n",
      "    colors: ['#636EFA', '#EF553B', '#00CC96'] };\n",
      "  var sk = {}; sk['ident' + evil] = 1203;\n",
      "  var pr = {}; pr['proj' + evil] = { x: blob(0), y: blob(0), ndim: 2 };\n",
      "  return { groups: g, cat_skipped: sk, projections: pr,\n",
      "    default_group: 'grp' + evil, default_projection: 'proj' + evil };\n",
      "})()"
    )
  ))
  app$wait_for_js(
    "document.querySelectorAll('.cv-filt-btn').length === 1",
    timeout = 15000
  )
  app$run_js("document.getElementById('cv-more-btn').click();")
  app$wait_for_js(
    "document.getElementById('cv-more').classList.contains('is-open')",
    timeout = 10000
  )
  app$run_js("document.querySelectorAll('.cv-filt-btn')[0].click();")
  app$wait_for_js(
    "document.querySelectorAll('.cv-filt-item').length === 3",
    timeout = 8000
  )

  expect_equal(app$get_js("window.__cvXss;"), 0)
  expect_equal(
    app$get_js(
      "document.querySelectorAll('.coordviews-page img[src=\"x\"]').length;"
    ),
    0
  )
  # shown literally, tags and all
  expect_true(app$get_js(
    paste0(
      "document.querySelector('.cv-filt-item').textContent",
      ".indexOf('<img') >= 0;"
    )
  ))

  app$stop()
})


test_that("a 3-D embedding can be rotated, and a 2-D one cannot", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_three_d")

  # Sparse on purpose: 60 points per blob, spread so they do not overlap. A
  # dense cloud hides the depth cue — points grow into each other and the pixel
  # count stops tracking their area, which is what made an early measurement of
  # this read 1.03x when the sizes really did differ.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var per = 60, n = per * 3;\n",
      "  var x = [], y = [], z = [], cells = [], vals = [];\n",
      "  for (var k = 0; k < 3; k++)\n",
      "    for (var j = 0; j < per; j++) {\n",
      "      x.push((k - 1) * 9 + (j % 8) * 0.9);\n",
      "      y.push(Math.floor(j / 8) * 1.2 - 4);\n",
      "      z.push((k - 1) * 6);\n",
      "      cells.push('c' + (k * per + j)); vals.push(k);\n",
      "    }\n",
      "  var flat = x.map(function (v) { return v; });\n",
      "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
      "    coordviews_data: {\n",
      "      cells: cells, n: n,\n",
      "      groups: { cluster: { values: vals, levels: ['a', 'b', 'c'],\n",
      "        colors: ['#636EFA', '#EF553B', '#00CC96'] } },\n",
      "      cat_extra: {}, cat_skipped: {}, fields: {},\n",
      "      default_group: 'cluster',\n",
      "      projections: { umap_3D: { x: x, y: y, z: z, ndim: 3 },\n",
      "        umap_2D: { x: flat, y: y, ndim: 2 } },\n",
      "      default_projection: 'umap_3D',\n",
      "      spaces: [{ id: 'umap', label: 'umap_3D (expression, 3-D)',\n",
      "        x: x, y: y, z: z }],\n",
      "      clone: null, trekker: null\n",
      "    } } }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-pick-proj').selectize && ",
      "Object.keys(document.getElementById('cv-pick-proj').selectize.options).length === 2"
    ),
    timeout = 15000
  )

  # the third dimension is announced, and the tool to use it is offered
  expect_match(
    app$get_js("document.getElementById('cv-title-a').textContent"),
    "3-D"
  )
  # Exactly three components reads as "3-D"; more than three has to say that
  # only the first three are drawn, or a 50-component PCA would claim a view
  # nothing here provides.
  expect_equal(
    app$get_js(
      paste0(
        "Object.keys(document.getElementById('cv-pick-proj').selectize.options)",
        ".map(function (k) { return ",
        "document.getElementById('cv-pick-proj').selectize.options[k].text; })"
      )
    ),
    list("umap_3D (3-D)", "umap_2D")
  )
  orbit_shown <- paste0(
    "getComputedStyle(document.querySelector(",
    "'.cv-pane:first-child .cv-orbit-btn')).display !== 'none'"
  )
  expect_true(app$get_js(orbit_shown))

  # Depth reads as size: same number of points per blob, so pixels per colour
  # measures how large each is drawn. Unrotated, z is the depth directly.
  blob_px <- paste0(
    "(function () {\n",
    "  var cv = document.getElementById('cv-cv-a');\n",
    "  var d = cv.getContext('2d')\n",
    "    .getImageData(0, 0, cv.width, cv.height).data;\n",
    "  var blue = 0, teal = 0;\n",
    "  for (var i = 0; i < d.length; i += 4) {\n",
    "    if (d[i + 3] === 0) continue;\n",
    "    var R = d[i], G = d[i+1], B = d[i+2];\n",
    "    if (B > 180 && R < 140 && G < 140) blue++;\n",
    "    else if (G > 170 && R < 110 && B > 110 && B < 200) teal++;\n",
    "  }\n",
    "  return [blue, teal];\n",
    "})()"
  )
  px <- app$get_js(blob_px)
  expect_gt(max(unlist(px)) / min(unlist(px)), 1.6)

  # dragging with the rotate tool must actually turn it
  centroid <- paste0(
    "(function () {\n",
    "  var cv = document.getElementById('cv-cv-a');\n",
    "  var d = cv.getContext('2d')\n",
    "    .getImageData(0, 0, cv.width, cv.height).data;\n",
    "  var sx = 0, sy = 0, k = 0;\n",
    "  for (var yy = 0; yy < cv.height; yy += 4)\n",
    "    for (var xx = 0; xx < cv.width; xx += 4) {\n",
    "      var i = (yy * cv.width + xx) * 4;\n",
    "      if (d[i] < 100 && d[i+1] > 180 && d[i+2] > 130) {\n",
    "        sx += xx; sy += yy; k++; }\n",
    "    }\n",
    "  return k ? [Math.round(sx / k), Math.round(sy / k)] : null;\n",
    "})()"
  )
  before <- unlist(app$get_js(centroid))
  overlay_before <- unlist(app$get_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var d = cv.getContext('2d')\n",
      "    .getImageData(0, 0, cv.width, cv.height).data;\n",
      "  var sx = 0, sy = 0, k = 0;\n",
      "  for (var yy = 0; yy < cv.height; yy += 2)\n",
      "    for (var xx = 0; xx < cv.width; xx += 2) {\n",
      "      var i = (yy * cv.width + xx) * 4;\n",
      "      if (d[i] < 60 && d[i+1] < 60 && d[i+2] < 70 && d[i+3] > 0) {\n",
      "        sx += xx; sy += yy; k++; }\n",
      "    }\n",
      "  return k ? [Math.round(sx / k), Math.round(sy / k)] : null;\n",
      "})()"
    )
  ))
  app$run_js(
    paste0(
      "(function () {\n",
      "  document.querySelector(",
      "'.cv-tbtn[data-act=\"orbit\"][data-panel=\"A\"]').click();\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 260, clientY: r.top + 260, bubbles: true }));\n",
      "  for (var s = 20; s <= 160; s += 20)\n",
      "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "      { clientX: r.left + 260 + s, clientY: r.top + 260,\n",
      "        bubbles: true }));\n",
      "  window.dispatchEvent(new MouseEvent('mouseup', { bubbles: true }));\n",
      "})();"
    )
  )
  app$wait_for_idle(timeout = 5000)
  after <- unlist(app$get_js(centroid))
  expect_gt(sqrt(sum((after - before)^2)), 20)

  # reset returns it to the starting angle. Compared with a tolerance because
  # the centroid is sampled off the canvas on a 4px lattice, so it carries a
  # pixel of rounding — this is "back where it was", not "bit-identical".
  # A strict 5px cut-off is itself unstable when the sampled x/y both round by
  # a few pixels, so keep the rejection boundary just above that diagonal.
  app$run_js(
    "document.querySelector('.cv-tbtn[data-act=\"reset\"][data-panel=\"A\"]').click();"
  )
  app$wait_for_idle(timeout = 5000)
  back <- unlist(app$get_js(centroid))
  expect_lt(sqrt(sum((back - before)^2)), 6)

  # switching to the flat projection retires the tool — it would have nothing
  # to turn, and leaving it offered implies a dimension that is not there
  app$run_js(
    "document.getElementById('cv-pick-proj').selectize.setValue(['umap_2D']);"
  )
  app$wait_for_js(paste0("!(", orbit_shown, ")"), timeout = 8000)
  expect_false(app$get_js(orbit_shown))

  app$stop()
})


test_that("a 3-D panel navigates but cannot be selected on", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_three_d_navigate_only")

  # A 3-D expression space AND a flat clonal one. The pairing is the point: it
  # lets a selection mode be armed on the flat panel and then carried onto the
  # 3-D one, which is the case hiding the buttons cannot cover on its own.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var n = 1200;\n",
      "  var x = [], y = [], z = [], cx = [], cy = [];\n",
      "  var cells = [], vals = [], cid = [];\n",
      "  for (var i = 0; i < n; i++) {\n",
      "    var k = i % 3;\n",
      "    x.push((k - 1) * 4 + Math.random());\n",
      "    y.push(Math.random() * 2);\n",
      "    z.push((k - 1) * 5 + Math.random());\n",
      "    cx.push(i % 40); cy.push(Math.floor(i / 40) % 30);\n",
      "    cells.push('c' + i); vals.push(k); cid.push(i % 20);\n",
      "  }\n",
      "  var lab = [], sz = [];\n",
      "  for (var q = 0; q < 20; q++) { lab.push('CASS' + q); sz.push(60 - q); }\n",
      "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
      "    coordviews_data: {\n",
      "      cells: cells, n: n,\n",
      "      groups: { cluster: { values: vals, levels: ['a', 'b', 'c'],\n",
      "        colors: ['#636EFA', '#EF553B', '#00CC96'] } },\n",
      "      cat_extra: {}, cat_skipped: {}, fields: {},\n",
      "      default_group: 'cluster',\n",
      "      projections: { umap_3D: { x: x, y: y, z: z, ndim: 3 } },\n",
      "      default_projection: 'umap_3D',\n",
      "      spaces: [\n",
      "        { id: 'umap', label: 'umap_3D (expression, 3-D)',\n",
      "          x: x, y: y, z: z },\n",
      "        { id: 'clone', label: 'Clonal expansion', x: cx, y: cy }],\n",
      "      clone: { id: cid, label: lab, size: sz,\n",
      "        n_clones: 20, n_receptor: n },\n",
      "      trekker: null\n",
      "    } } }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-title-b').textContent.indexOf('Clonal') >= 0",
    timeout = 15000
  )

  vis <- function(key, act) {
    paste0(
      "getComputedStyle(document.getElementById('cv-cv-",
      key,
      "')",
      ".closest('.cv-pane').querySelector('.cv-tbtn[data-act=\"",
      act,
      "\"]')).display !== 'none'"
    )
  }
  # navigation stays, selection goes
  expect_false(app$get_js(vis("a", "lasso")))
  expect_false(app$get_js(vis("a", "box")))
  expect_true(app$get_js(vis("a", "orbit")))
  expect_true(app$get_js(vis("a", "pan")))
  expect_true(app$get_js(vis("a", "reset")))
  expect_true(app$get_js(vis("a", "png")))
  # and the flat panel is the mirror image
  expect_true(app$get_js(vis("b", "lasso")))
  expect_false(app$get_js(vis("b", "orbit")))

  # THE case buttons cannot cover: arm lasso on the flat panel, then drag on
  # the 3-D one. selectMode is global, so without an override at the event this
  # still draws a selection nobody could verify.
  app$run_js(
    "document.querySelector('.cv-tbtn[data-act=\"lasso\"][data-panel=\"B\"]').click();"
  )
  app$run_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 120, clientY: r.top + 120, bubbles: true }));\n",
      "  for (var s = 30; s <= 210; s += 30)\n",
      "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "      { clientX: r.left + 120 + s, clientY: r.top + 120 + s * 0.7,\n",
      "        bubbles: true }));\n",
      "  window.dispatchEvent(new MouseEvent('mouseup', { bubbles: true }));\n",
      "})();"
    )
  )
  app$wait_for_idle(timeout = 5000)
  expect_false(
    app$get_js(
      "!document.getElementById('cv-selbar').classList.contains('cv-collapse')"
    )
  )
  # the drag was not swallowed either — it turned the cloud
  expect_true(app$get_js("document.getElementById('cv-mini-a') !== null"))

  # clicking a single cell there must not pick one, for the same reason
  app$run_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 260, clientY: r.top + 260, bubbles: true }));\n",
      "  window.dispatchEvent(new MouseEvent('mouseup',\n",
      "    { clientX: r.left + 260, clientY: r.top + 260, bubbles: true }));\n",
      "})();"
    )
  )
  app$wait_for_idle(timeout = 5000)
  # A pick reveals the selection actions (that is what makes it clearable), so
  # that bar is the observable. The card used to be the observable here, which
  # stopped meaning anything once a click no longer opens one -- the assertion
  # would have passed against a 3-D panel that picked freely.
  expect_false(
    app$get_js(
      "!document.getElementById('cv-selbar').classList.contains('cv-collapse')"
    )
  )
  expect_false(
    app$get_js(
      "document.getElementById('cv-card').classList.contains('is-open')"
    )
  )

  # selecting on the FLAT panel still works, and reaches the 3-D one
  app$run_js(
    paste0(
      "(function () {\n",
      "  document.querySelector(",
      "'.cv-tbtn[data-act=\"box\"][data-panel=\"B\"]').click();\n",
      "  var cv = document.getElementById('cv-cv-b');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 8, clientY: r.top + 8, bubbles: true }));\n",
      "  for (var s = 40; s <= r.width - 20; s += 40)\n",
      "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "      { clientX: r.left + 8 + s,\n",
      "        clientY: r.top + 8 + s * (r.height / r.width), bubbles: true }));\n",
      "  window.dispatchEvent(new MouseEvent('mouseup',\n",
      "    { clientX: r.right - 10, clientY: r.bottom - 10, bubbles: true }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-seltext').textContent.indexOf('Selected') >= 0",
    timeout = 10000
  )
  expect_match(
    app$get_js("document.getElementById('cv-seltext').textContent"),
    "coordinated across all panels"
  )

  # The top bar's "Zoom to selection" defaults to the expression panel, which
  # here is the 3-D one. A zoom is a rectangle in screen space, and on a rotated
  # cloud that rectangle belongs to the current angle only -- turn it afterwards
  # and the cells it was fitted to are elsewhere, possibly off screen. The
  # per-panel button is hidden on a 3-D panel for exactly this reason, so the
  # top-bar one must not quietly do it instead.
  expect_false(
    app$get_js(
      "getComputedStyle(document.getElementById('cv-zoom')).display !== 'none'"
    )
  )
  app$run_js("document.getElementById('cv-zoom').click();")
  app$wait_for_idle(timeout = 5000)
  expect_false(
    app$get_js(
      "document.getElementById('cv-mini-a').classList.contains('is-on')"
    )
  )

  app$stop()
})


test_that("an all-3-D data set says where selection has gone", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_three_d_only")

  app$run_js(cv_bundle_js(
    paste0(
      "(function () {\n",
      "  var zz = blob(0);\n",
      "  return { projections: { umap_3D: { x: blob(0), y: blob(0), z: zz,\n",
      "      ndim: 3 } },\n",
      "    default_projection: 'umap_3D',\n",
      "    spaces: [{ id: 'umap', label: 'umap_3D (expression, 3-D)',\n",
      "      x: blob(0), y: blob(0), z: zz }] };\n",
      "})()"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-title-a').textContent.indexOf('3-D') >= 0",
    timeout = 15000
  )

  # The empty readout normally says "lasso-drag in any panel". With nothing but
  # rotatable panels that instruction is false, so it must not be the one shown.
  readout <- app$get_js("document.getElementById('cv-readout').textContent")
  expect_false(grepl("Lasso-drag in any panel", readout, fixed = TRUE))
  expect_match(readout, "turning and looking")
  # This data set carries no flat embedding at all, so there is no advice to
  # give. Sending the user to the Projection tab was the wrong answer: its 3-D
  # scatter is no more lassoable than these panels are.
  expect_match(readout, "does not carry")
  expect_false(grepl("Projection", readout, fixed = TRUE))

  # and the toolbar cannot be sitting in a mode no panel offers
  expect_true(
    app$get_js(
      "document.querySelector('.cv-pane:first-child .cv-tbtn.is-on')
       .getAttribute('data-act') === 'orbit'"
    )
  )

  # When the data set DOES carry a flat embedding and is merely showing the 3-D
  # one, there is something to say: the picker is the way back to selecting.
  app$run_js(cv_bundle_js(
    paste0(
      "(function () {\n",
      "  var zz = blob(0);\n",
      "  return { projections: { umap_3D: { x: blob(0), y: blob(0), z: zz,\n",
      "      ndim: 3 }, umap_2D: { x: blob(0), y: blob(0), ndim: 2 } },\n",
      "    default_projection: 'umap_3D',\n",
      "    spaces: [{ id: 'umap', label: 'umap_3D (expression, 3-D)',\n",
      "      x: blob(0), y: blob(0), z: zz }] };\n",
      "})()"
    )
  ))
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-readout').textContent",
      ".indexOf('turning and looking') >= 0"
    ),
    timeout = 15000
  )
  readout <- app$get_js("document.getElementById('cv-readout').textContent")
  expect_match(readout, "pick a 2-D projection above")
  expect_false(grepl("does not carry", readout, fixed = TRUE))
  ## Both halves of the sentence describe the same data set, so the opening
  ## claim has to change with the advice: "the only embedding is 3-D" followed
  ## by "pick a 2-D one" contradicts itself, and a reader acting on the first
  ## half gives up on a data set that can in fact be selected in.
  expect_false(grepl("only embedding is 3-D", readout, fixed = TRUE))
  expect_match(readout, "Every panel is showing a 3-D embedding")

  app$stop()
})


test_that("a data set with no linked views blanks the workspace", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_unavailable")

  app$run_js(cv_bundle_js())
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('800 cells') >= 0",
    timeout = 15000
  )

  # Staying silent here used to leave the PREVIOUS data set's panels on screen,
  # presenting one data set's cells as another's.
  app$run_js(
    paste0(
      "Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
      "  coordviews_data: { error: 'TEST: nothing to link on.' } } }));"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('TEST:') >= 0",
    timeout = 10000
  )

  expect_equal(
    app$get_js(
      paste0(
        "Array.from(document.querySelectorAll('.cv-pane'))",
        ".filter(function (p) ",
        "{ return !p.classList.contains('cv-hidden'); }).length;"
      )
    ),
    0
  )
  expect_equal(
    app$get_js("document.getElementById('cv-legend').innerHTML;"),
    ""
  )

  app$stop()
})

## Every string in the bundle originates in a `.crb` the user opened, so each one
## interpolated into innerHTML is an injection point, and there are more of them
## than any one screen shows: the meta line, the legend (categorical and RGB),
## the composition header, the clonotype table, the group filters. A test that
## checks one sink says nothing about the others, so each is driven and asserted
## here -- the composition and clonotype paths went unescaped precisely because
## the earlier version of this test never made a selection and so never rendered
## them.
##
## Colours are the second kind. They land in a `style` attribute, where escaping
## buys nothing: `red;position:fixed;inset:0` never leaves the attribute, it just
## appends declarations. Those are validated against the browser's own colour
## parser instead, once, as the bundle lands.
test_that("data-set strings cannot inject markup into the workspace", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_escaping")

  ## No quotes in the payload, so it survives the R -> JS -> JSON trip unaltered
  ## and any difference on screen is the app's doing rather than the harness's.
  payload <- "<img src=x onerror=window.__cvXss=1>"
  ## Needs no quote to escape: it stays inside the attribute and adds its own
  ## declarations, which is why esc() is not the tool for a colour.
  css_payload <- "red;position:fixed;inset:0;z-index:9999"
  ## Four bad colours of three different kinds, because each kind gets past a
  ## different validator: the CSS injection defeats escaping, `notacolor` defeats
  ## a permissive hand-written grammar, and `inherit` / `var(--x)` defeat
  ## CSS.supports('color', ...) -- legal CSS values that a canvas will not paint.
  bad_colors <- "'{CSS}', 'notacolor', 'inherit', 'var(--cv-x)'"

  app$run_js(cv_bundle_js(
    extra = paste0(
      "{ groups: { '",
      payload,
      "': { values: cells.map(function (c, i) ",
      "{ return i % 4; }),\n",
      "  levels: ['",
      payload,
      "', 'b', 'c', 'd'],\n",
      "  colors: [",
      sub("{CSS}", css_payload, bad_colors, fixed = TRUE),
      "] } },\n",
      "  default_group: '",
      payload,
      "',\n",
      "  projections: { '",
      payload,
      "': { x: blob(0), y: blob(0), ndim: 2 } },\n",
      "  default_projection: '",
      payload,
      "',\n",
      "  spaces: [{ id: 'umap', label: '",
      payload,
      "',\n",
      "    x: blob(0), y: blob(0) }],\n",
      "  rgb: { genes: ['",
      payload,
      "', 'GeneB', 'GeneC'] },\n",
      "  clone: { id: vals.map(function (v) { return v % 2; }),\n",
      "    label: ['",
      payload,
      "', 'CASSIRSSYEQYF'],\n",
      "    size: [400, 400], n_receptor: 800, n_clones: 2 } }"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.length > 0",
    timeout = 15000
  )

  ## Select everything, so the composition and clonotype readouts render.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 2, clientY: r.top + 2, bubbles: true }));\n",
      "  var pts = [[r.width - 2, 2], [r.width - 2, r.height - 2],\n",
      "    [2, r.height - 2], [2, 2]];\n",
      "  pts.forEach(function (q) {\n",
      "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "      { clientX: r.left + q[0], clientY: r.top + q[1], bubbles: true }));\n",
      "  });\n",
      "  window.dispatchEvent(new MouseEvent('mouseup', { bubbles: true }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-readout').textContent",
      ".indexOf('Composition') >= 0"
    ),
    timeout = 10000
  )

  ## Each sink separately: the payload has to be ON SCREEN as text. Asserting
  ## only "no <img> element" would pass against a build that renders nothing.
  sinks <- list(
    meta = "cv-meta",
    legend = "cv-legend",
    readout = "cv-readout"
  )
  for (nm in names(sinks)) {
    expect_true(
      app$get_js(paste0(
        "document.getElementById('",
        sinks[[nm]],
        "')",
        ".textContent.indexOf('<img') >= 0;"
      )),
      info = nm
    )
  }
  ## The composition header interpolates the grouping COLUMN NAME, which the
  ## level names inside the same readout would otherwise mask: those are escaped
  ## already, so `#cv-readout` keeps showing the payload as text even when the
  ## header has turned it into an element. Assert on the header itself.
  expect_true(app$get_js(
    paste0(
      "(function () { var e = document.querySelector('.cv-read-sub');\n",
      "  return !!e && e.textContent.indexOf('<img') >= 0; })();"
    )
  ))
  ## The clonotype table is its own builder inside the readout.
  expect_true(app$get_js(
    paste0(
      "(function () { var e = document.querySelector('.cv-cdr3');\n",
      "  return !!e && e.textContent.indexOf('<img') >= 0; })();"
    )
  ))
  ## ... as is the group-filter list, which renders whether or not it is open.
  expect_true(app$get_js(
    paste0(
      "(function () { var e = document.querySelector('.cv-filt-item');\n",
      "  return !!e && e.textContent.indexOf('<img') >= 0; })();"
    )
  ))

  ## Colours, while the CATEGORICAL legend is still up. Checking after the switch
  ## to RGB inspects that legend's hard-coded channel swatches instead, which are
  ## a colour whatever the group colours did -- an assertion that cannot fail.
  expect_equal(
    app$get_js(
      paste0(
        "Array.from(document.querySelectorAll('.cv-dot, .cv-bar-fl'))\n",
        "  .map(function (e) { return e.getAttribute('style') || ''; })\n",
        "  .filter(function (s) { return /position:fixed|notacolor|inherit/\n",
        "    .test(s) || s.indexOf('var(') >= 0; }).length;"
      )
    ),
    0
  )
  ## An invalid colour is REPLACED, not merely dropped: a canvas keeps its
  ## PREVIOUS fillStyle on a bad assignment, so a point whose colour silently
  ## went missing inherits its neighbour's rather than showing as unpainted.
  ## Every swatch has to be a colour the canvas itself accepted.
  expect_true(app$get_js(
    paste0(
      "(function () {\n",
      "  var ctx = document.createElement('canvas').getContext('2d');\n",
      "  var sw = Array.from(document.querySelectorAll(\n",
      "    '#cv-legend .cv-dot, .cv-filt-item .cv-dot, .cv-bar-fl'));\n",
      "  if (!sw.length) return false;\n",
      "  return sw.every(function (e) {\n",
      "    var c = (e.getAttribute('style') || '').split(':').pop();\n",
      "    ctx.fillStyle = '#000000'; ctx.fillStyle = c;\n",
      "    var a = ctx.fillStyle;\n",
      "    ctx.fillStyle = '#ffffff'; ctx.fillStyle = c;\n",
      "    return a === ctx.fillStyle;\n",
      "  });\n",
      "})();"
    )
  ))

  ## The RGB channel legend is a separate builder again.
  app$run_js(cv_set_color_js("__rgb__"))
  expect_true(app$get_js(
    "document.getElementById('cv-legend').textContent.indexOf('<img') >= 0;"
  ))

  ## Nowhere did any of it become an element, and no handler ran.
  expect_null(app$get_js("window.__cvXss || null;"))
  expect_equal(
    app$get_js("document.querySelectorAll('img[src=\"x\"]').length;"),
    0
  )

  app$stop()
})

## A panel remembers how it is looking at its space: the view rectangle, the
## lasso, and -- once a 3-D embedding exists -- the rotation, the depth buffer and
## the cached minimap. Handing a panel a different data set dropped the first two
## and kept the rest, so switching between two 3-D data sets opened the new one at
## the old one's angle, with a depth buffer computed for cells that were gone.
test_that("a data-set switch does not carry the rotation over", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_switch_rot")

  ## Deterministic coordinates, so the same bundle pushed twice must draw the
  ## same pixels. With Math.random() the comparison could not tell a carried-over
  ## rotation from a different cloud.
  push_3d <- paste0(
    "(function () {\n",
    "  var per = 60, n = per * 3;\n",
    "  var x = [], y = [], z = [], cells = [], vals = [];\n",
    "  for (var k = 0; k < 3; k++)\n",
    "    for (var j = 0; j < per; j++) {\n",
    "      x.push((k - 1) * 9 + (j % 8) * 0.9);\n",
    "      y.push(Math.floor(j / 8) * 1.2 - 4);\n",
    "      z.push((k - 1) * 6);\n",
    "      cells.push('c' + (k * per + j)); vals.push(k);\n",
    "    }\n",
    "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
    "    coordviews_data: {\n",
    "      cells: cells, n: n,\n",
    "      groups: { cluster: { values: vals, levels: ['a', 'b', 'c'],\n",
    "        colors: ['#636EFA', '#EF553B', '#00CC96'] } },\n",
    "      cat_extra: {}, cat_skipped: {}, fields: {},\n",
    "      default_group: 'cluster',\n",
    "      projections: { umap_3D: { x: x, y: y, z: z, ndim: 3 } },\n",
    "      default_projection: 'umap_3D',\n",
    "      spaces: [{ id: 'umap', label: 'umap_3D (expression, 3-D)',\n",
    "        x: x, y: y, z: z }],\n",
    "      clone: null, trekker: null\n",
    "    } } }));\n",
    "})();"
  )
  centroid <- paste0(
    "(function () {\n",
    "  var cv = document.getElementById('cv-cv-a');\n",
    "  var d = cv.getContext('2d')\n",
    "    .getImageData(0, 0, cv.width, cv.height).data;\n",
    "  var sx = 0, sy = 0, k = 0;\n",
    "  for (var yy = 0; yy < cv.height; yy += 2)\n",
    "    for (var xx = 0; xx < cv.width; xx += 2) {\n",
    "      var i = (yy * cv.width + xx) * 4;\n",
    "      if (d[i + 3] > 0 && d[i] > 60 && d[i] < 140 &&\n",
    "          d[i + 2] > 200) { sx += xx; sy += yy; k++; }\n",
    "    }\n",
    "  return k ? [Math.round(sx / k), Math.round(sy / k)] : null;\n",
    "})()"
  )

  app$run_js(push_3d)
  app$wait_for_js(
    "document.getElementById('cv-title-a').textContent.indexOf('3-D') >= 0",
    timeout = 15000
  )
  app$wait_for_idle(timeout = 5000)
  before <- unlist(app$get_js(centroid))
  expect_false(is.null(before))

  app$run_js(
    paste0(
      "(function () {\n",
      "  document.querySelector(",
      "'.cv-tbtn[data-act=\"orbit\"][data-panel=\"A\"]').click();\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + 260, clientY: r.top + 260, bubbles: true }));\n",
      "  for (var s = 20; s <= 160; s += 20)\n",
      "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "      { clientX: r.left + 260 + s, clientY: r.top + 260,\n",
      "        bubbles: true }));\n",
      "  window.dispatchEvent(new MouseEvent('mouseup', { bubbles: true }));\n",
      "})();"
    )
  )
  app$wait_for_idle(timeout = 5000)
  rotated <- unlist(app$get_js(centroid))
  expect_gt(sqrt(sum((rotated - before)^2)), 20)

  ## The switch. Identical coordinates, so anything but the starting angle is
  ## state the previous data set left behind.
  app$run_js(push_3d)
  app$wait_for_idle(timeout = 5000)
  after <- unlist(app$get_js(centroid))
  expect_lt(sqrt(sum((after - before)^2)), 5)

  app$stop()
})

## Linked views is one tab among eighteen, and building its bundle means walking
## every cell of the loaded object -- reductions, spatial coordinates, the immune
## repertoire -- into ~156 KB of payload. That used to happen on connect, for
## every session, whether or not anyone opened the tab. The full bundle must
## remain lazy even after a palette edit while the tab is hidden; the colour
## patch itself is cheap and applies when the workspace returns.
test_that("the bundle is built only while the workspace is on screen", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "cv_browser_lazy",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]') !== null",
    timeout = 30000
  )

  ## The tab exists and its data set is loaded -- and still nothing has been
  ## built. Waiting for the link first matters: asserting before the conditional
  ## tabs are inserted would pass against an eager build that had not run yet.
  expect_equal(app$get_value(export = "coordviews_bundles_built"), 0)

  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]').click();"
  )
  app$wait_for_idle(timeout = 20000)

  ## Opening it builds it once, and the workspace really is populated -- a gate
  ## that never opens would also report "0 before, 1 after" if the assertion
  ## stopped at the counter.
  expect_equal(app$get_value(export = "coordviews_bundles_built"), 1)
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.length > 0",
    timeout = 20000
  )
  expect_gt(app$get_js(cv_ink_js()), 1)

  ## Leave for Color management and change a group colour while the workspace
  ## is hidden. It must not build the full bundle.
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-color_management\"]').click();"
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').offsetParent === null",
    timeout = 10000
  )
  ## Wait for the SERVER to know it is hidden, not just the DOM. The client
  ## reports on the way out and again on a poll; asserting before that lands
  ## would be asserting against a window in which the server still believes the
  ## workspace is on screen.
  app$wait_for_value(
    input = "coordviews_visible",
    ignore = list(TRUE, NULL),
    timeout = 10000
  )
  app$wait_for_idle(timeout = 10000)
  app$wait_for_js(
    "document.querySelector('[id^=\"color_\"]') !== null",
    timeout = 20000
  )
  app$run_js(
    paste0(
      "(function () {\n",
      "  var el = document.querySelector('[id^=\"color_\"]');\n",
      "  Shiny.setInputValue(el.id, '#123456');\n",
      "})();"
    )
  )
  app$wait_for_idle(timeout = 15000)
  expect_equal(app$get_value(export = "coordviews_bundles_built"), 1)

  ## Coming back applies the queued palette without rebuilding the full bundle.
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]').click();"
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').offsetParent !== null",
    timeout = 10000
  )
  app$wait_for_idle(timeout = 20000)
  expect_equal(app$get_value(export = "coordviews_bundles_built"), 1)

  app$stop()
})

## "Every linked panel on screen at once" is the whole premise of the layout: a
## panel below the fold is one the user cannot compare against. The squares were
## floored at the size below which a panel stops being COMFORTABLE, which on a
## 1366x768 screen is more height than three or four panels have -- so the last
## row fell past the bottom and the page scrolled, on exactly the layouts that
## most need to be seen together.
test_that("three and four panels keep a 300px floor and wrap on a small screen", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_viewport_fit")
  ## A 1366x768 laptop, the smallest screen this is expected to work on.
  app$set_window_size(width = 1366, height = 768)
  app$wait_for_idle(timeout = 10000)

  spaces <- function(n) {
    ids <- c("umap", "spatial", "trekker", "clone")[seq_len(n)]
    paste0(
      "[",
      paste(
        vapply(
          ids,
          function(id) {
            paste0(
              "{ id: '",
              id,
              "', label: '",
              id,
              "',",
              " x: blob(0), y: blob(0) }"
            )
          },
          character(1)
        ),
        collapse = ", "
      ),
      "]"
    )
  }
  ## The panels' own bottom edge, not documentElement.scrollHeight: the grid is
  ## clipped rather than allowed to extend the document, so a row past the fold
  ## does not lengthen the page -- it simply cannot be reached. scrollHeight
  ## reads 768 either way, which is why an earlier version of this test passed
  ## against the very layout it was written to catch.
  overflow <- paste0(
    "(function () {\n",
    "  var panes = document.querySelector('.cv-panes');\n",
    "  return Math.round(panes.getBoundingClientRect().bottom) -\n",
    "    window.innerHeight;\n",
    "})();"
  )

  for (n in c(3, 4)) {
    app$run_js(cv_bundle_js(paste0("{ spaces: ", spaces(n), " }")))
    app$wait_for_js(
      paste0(
        "document.querySelectorAll('.cv-pane:not(.cv-hidden)').length === ",
        n
      ),
      timeout = 15000
    )
    app$wait_for_idle(timeout = 10000)

    ## Every panel drew, so this is not "fits because nothing is there".
    expect_equal(
      app$get_js(
        paste0(
          "Array.from(document.querySelectorAll('.cv-pane:not(.cv-hidden) canvas'))",
          ".filter(function (c) { return c.width > 0 && c.height > 0; }).length"
        )
      ),
      n * 2, # each pane carries its canvas and its minimap
      info = paste(n, "panels")
    )
    ## The requested 300px floor wins over squeezing every row into one screen;
    ## additional rows remain reachable by normal page scrolling.
    expect_gte(
      app$get_js("document.getElementById('cv-cv-a').clientWidth"),
      300
    )
    expect_lte(
      app$get_js(paste0(
        "Math.round(document.querySelector('.cv-panes').getBoundingClientRect().right)",
        " - document.documentElement.clientWidth"
      )),
      0
    )
    if (app$get_js(overflow) > 0) {
      expect_gt(
        app$get_js("document.querySelector('.content-wrapper').scrollHeight"),
        app$get_js("document.querySelector('.content-wrapper').clientHeight")
      )
    }
  }

  app$stop()
})

## Clicking a cell used to throw the full detail card over the middle of the
## workspace. On a Trekker data set that is the same click that picks a nucleus
## to read its niche, so the answer arrived buried under a card covering the
## panels it was about.
##
## Three depths now, each entered deliberately: hover gives a short read that
## follows the cursor; a click PINS that tooltip, which is what makes its buttons
## clickable at all -- one that tracks the pointer moves out from under any
## attempt to press it -- and adds Details and Close; Details opens the card.
test_that("a Trekker click opens the inspector without covering linked views", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_detail_button")

  ## A regular grid, so a probe lands unambiguously on one cell.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var n = 81;\n",
      "  var x = [], y = [], cells = [], vals = [];\n",
      "  for (var j = 0; j < n; j++) {\n",
      "    x.push((j % 9) - 4);\n",
      "    y.push(Math.floor(j / 9) - 4);\n",
      "    cells.push('c' + j); vals.push(j % 2);\n",
      "  }\n",
      "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
      "    coordviews_data: {\n",
      "      cells: cells, n: n,\n",
      "      groups: { cluster: { values: vals, levels: ['a', 'b'],\n",
      "        colors: ['#636EFA', '#EF553B'] },\n",
      "        sample: { values: vals, levels: ['s1', 's2'],\n",
      "          colors: ['#111111', '#222222'] } },\n",
      "      cat_extra: {}, cat_skipped: {}, fields: {},\n",
      "      default_group: 'cluster',\n",
      "      projections: { umap: { x: x, y: y, ndim: 2 } },\n",
      "      default_projection: 'umap',\n",
      "      spaces: [{ id: 'umap', label: 'umap (expression)', x: x, y: y },\n",
      "        { id: 'trekker', label: 'Trekker (physical)',\n",
      "          x: x.map(function (v) { return v * 60; }),\n",
      "          y: y.map(function (v) { return v * 60; }), unit: 'um' }],\n",
      "      clone: null, trekker: { qc: null }\n",
      "    } } }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('81 cells') >= 0",
    timeout = 15000
  )

  card_open <- "document.getElementById('cv-card').classList.contains('is-open')"
  tip_txt <- "document.getElementById('cv-tip-a').textContent"
  hover <- function() {
    app$run_js(paste0(
      "(function () { var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "    { clientX: r.left + r.width / 2, clientY: r.top + r.height / 2,\n",
      "      bubbles: true })); })();"
    ))
  }

  ## Hover: a short read, and no controls -- they would be unreachable anyway,
  ## since the tooltip follows the cursor.
  hover()
  app$wait_for_js(
    "getComputedStyle(document.getElementById('cv-tip-a')).opacity === '1'",
    timeout = 5000
  )
  expect_null(app$get_js(
    "document.querySelector('#cv-tip-a .cv-tip-btn') || null"
  ))
  hover_text <- app$get_js(tip_txt)
  ## The grouping variables are not part of the glance.
  expect_false(grepl("sample", hover_text, fixed = TRUE))

  ## Click: the tooltip pins, gains both actions, and no card appears.
  app$run_js(paste0(
    "(function () { var cv = document.getElementById('cv-cv-a');\n",
    "  var r = cv.getBoundingClientRect();\n",
    "  var x = r.left + r.width / 2, y = r.top + r.height / 2;\n",
    "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
    "    { clientX: x, clientY: y, bubbles: true }));\n",
    "  window.dispatchEvent(new MouseEvent('mouseup',\n",
    "    { clientX: x, clientY: y, bubbles: true })); })();"
  ))
  app$wait_for_js(
    "document.querySelector('#cv-tip-a .cv-tip-details') !== null",
    timeout = 10000
  )
  expect_false(app$get_js(card_open))
  expect_true(app$get_js(
    "document.querySelector('#cv-tip-a .cv-tip-close') !== null"
  ))
  expect_true(app$get_js(
    "document.getElementById('cv-tip-a').classList.contains('cv-tip-pinned')"
  ))
  ## Pinned, it says more than the glance did.
  expect_match(app$get_js(tip_txt), "sample")
  ## The pick happened: on a Trekker data set that is what the click is for.
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-readout').textContent",
      ".indexOf('Niche of picked nucleus') >= 0"
    ),
    timeout = 10000
  )
  ## A single-cell pick is also shared workspace state. It lives in the same
  ## status surface as a lasso cohort, but says what it actually is and keeps
  ## its Clear action reachable.
  expect_true(app$get_js(
    "getComputedStyle(document.getElementById('cv-selbar')).display !== 'none'"
  ))
  expect_equal(
    app$get_js("document.getElementById('cv-sel-kicker').textContent"),
    "Active cell"
  )
  expect_match(
    app$get_js("document.getElementById('cv-seltext').textContent"),
    "Picked nucleus"
  )
  expect_true(app$get_js(
    "getComputedStyle(document.getElementById('cv-clear')).display !== 'none'"
  ))
  app$run_js(paste0(
    "(function () { var r = document.getElementById('cv-niche');",
    "r.value = '50'; r.dispatchEvent(new Event('input', { bubbles: true })); })();"
  ))
  app$wait_for_js(
    "document.getElementById('cv-selcoverage').textContent.indexOf('50 µm') >= 0",
    timeout = 5000
  )

  ## It stays put when the pointer moves away -- otherwise the buttons could not
  ## be reached, which is the whole reason for pinning.
  app$run_js(paste0(
    "(function () { var cv = document.getElementById('cv-cv-a');\n",
    "  cv.dispatchEvent(new MouseEvent('mouseleave', { bubbles: true })); })();"
  ))
  app$wait_for_idle(timeout = 5000)
  expect_equal(
    app$get_js("getComputedStyle(document.getElementById('cv-tip-a')).opacity"),
    "1"
  )
  ## Once pinned, the tooltip must become a hit target so the delegated click
  ## handler can receive Details/Close clicks instead of the event falling
  ## through to the canvas.
  expect_equal(
    app$get_js(
      "getComputedStyle(document.getElementById('cv-tip-a')).pointerEvents"
    ),
    "auto"
  )
  expect_equal(
    app$get_js(
      paste0(
        "getComputedStyle(document.querySelector('#cv-tip-a .cv-tip-details'))",
        ".pointerEvents"
      )
    ),
    "auto"
  )

  ## Trekker clicks open the shared inspector below the linked grid rather than
  ## covering the canvases with the generic floating card.
  app$wait_for_js(
    "document.getElementById('cv-tk-insights-toggle').getAttribute('aria-expanded') === 'true'",
    timeout = 10000
  )
  expect_true(app$get_js(
    "document.getElementById('cv-tk-tab-cell').classList.contains('is-active')"
  ))
  expect_gt(
    app$get_js("document.getElementById('cv-tk-cell-body').textContent.length"),
    0
  )
  expect_false(app$get_js(card_open))

  ## The pinned tooltip's Details action routes to the same inspector; it does
  ## not create a second presentation of the same cell.
  app$run_js("document.querySelector('#cv-tip-a .cv-tip-details').click();")
  app$wait_for_js(
    "document.getElementById('cv-tk-tab-cell').classList.contains('is-active')",
    timeout = 10000
  )
  expect_false(app$get_js(card_open))

  ## Close dismisses the tooltip.
  app$run_js("document.querySelector('#cv-tip-a .cv-tip-close').click();")
  app$wait_for_idle(timeout = 5000)
  expect_equal(
    app$get_js("getComputedStyle(document.getElementById('cv-tip-a')).opacity"),
    "0"
  )
  expect_false(app$get_js(
    "document.getElementById('cv-tip-a').classList.contains('cv-tip-pinned')"
  ))
  ## ... but NOT the pick behind it. The ring and, on a Trekker data set, the
  ## niche readout are what the cell was clicked for; putting the tooltip away
  ## is not a reason to give them up. Clicking the cell again drops the pick.
  expect_true(app$get_js(
    paste0(
      "document.getElementById('cv-readout').textContent",
      ".indexOf('Niche of picked nucleus') >= 0"
    )
  ))

  ## Escape closes the open details card and releases its active cell; every
  ## surface that represents that state must clear with it.
  app$run_js(
    "document.dispatchEvent(new KeyboardEvent('keydown', { key: 'Escape', bubbles: true }));"
  )
  app$wait_for_js(
    "document.getElementById('cv-selbar').classList.contains('cv-collapse')",
    timeout = 5000
  )
  app$wait_for_js(
    "getComputedStyle(document.getElementById('cv-selactions')).display === 'none'",
    timeout = 5000
  )
  expect_equal(
    app$get_js(
      "getComputedStyle(document.getElementById('cv-selactions')).display"
    ),
    "none"
  )

  app$stop()
})

## Index order is arbitrary with respect to expression, so a cell painted late
## covers whatever it overlaps regardless of what either is worth. Where cells
## overlap -- which is everywhere on a real embedding -- that turned a focus of
## high expression into whatever its low-expressing neighbours happened to be.
## Continuous colourings are now painted low value first.
test_that("high values are painted over low ones", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_paint_order")

  ## Two cells at the SAME position: a high-expressing one early in the array
  ## and a low-expressing one after it. Whatever is drawn second wins the pixel,
  ## so the colour there names the order. The rest of the cloud sits far away so
  ## it cannot contribute to the probe.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var n = 40, x = [], y = [], cells = [], vals = [], gv = [];\n",
      "  for (var j = 0; j < n; j++) {\n",
      "    x.push(j === 0 || j === 1 ? 0 : 40 + (j % 5));\n",
      "    y.push(j === 0 || j === 1 ? 0 : 40 + Math.floor(j / 5));\n",
      "    cells.push('c' + j); vals.push(0);\n",
      ## cell 0 high, cell 1 at the SAME spot low
      "    gv.push(j === 0 ? 255 : 0);\n",
      "  }\n",
      "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
      "    coordviews_data: {\n",
      "      cells: cells, n: n,\n",
      "      groups: { cluster: { values: vals, levels: ['a'],\n",
      "        colors: ['#636EFA'] } },\n",
      "      cat_extra: {}, cat_skipped: {},\n",
      "      fields: { 'meta:score': { label: 'score', v: gv,\n",
      "        min: 0, max: 255, scale: 255 } },\n",
      "      default_group: 'cluster',\n",
      "      projections: { umap: { x: x, y: y, ndim: 2 } },\n",
      "      default_projection: 'umap',\n",
      "      spaces: [{ id: 'umap', label: 'umap (expression)', x: x, y: y }],\n",
      "      clone: null, trekker: null\n",
      "    } } }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('40 cells') >= 0",
    timeout = 15000
  )

  ## Colour by the numeric field, so both cells take a viridis colour.
  app$run_js(cv_set_color_js("__field__meta:score"))
  app$wait_for_idle(timeout = 10000)

  ## The two cells coincide exactly and are drawn at the same radius, so the one
  ## painted second hides the other. Viridis runs dark blue-purple at 0 to yellow
  ## at 1 and every other cell here sits at 0, so anything yellow can only be the
  ## high one. Measured both ways rather than assumed: painted low-last leaves 4
  ## yellow pixels of anti-aliased rim and a brightest red of 161, painted
  ## low-first leaves 128 and 222. "Any yellow at all" therefore proves nothing --
  ## the rim survives either way -- so both the area and the purity are asserted.
  stats <- unlist(app$get_js(paste0(
    "(function () {\n",
    "  var cv = document.getElementById('cv-cv-a');\n",
    "  var d = cv.getContext('2d').getImageData(0, 0, cv.width, cv.height).data;\n",
    "  var yellow = 0, maxr = 0;\n",
    "  for (var i = 0; i < d.length; i += 4) {\n",
    "    if (d[i + 3] === 0) continue;\n",
    "    if (d[i] > 245 && d[i+1] > 245 && d[i+2] > 245) continue;\n",
    "    if (d[i] > d[i+2] + 40 && d[i+1] > 120) yellow++;\n",
    "    if (d[i] > maxr) maxr = d[i];\n",
    "  }\n",
    "  return [yellow, maxr];\n",
    "})();"
  )))
  expect_gt(stats[1], 50)
  expect_gt(stats[2], 200)

  app$stop()
})

## One extreme value owns the top of a full min-max scale and presses every other
## cell into the bottom few percent of the colour map, where the differences that
## matter are differences nobody can see. Trimming the tails is the default; the
## trimmed cells are not hidden, they saturate, and the colourbar says so.
test_that("an outlier does not flatten the colour scale", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_colour_clip")

  ## 199 cells spread evenly across the bottom 6% of the field's range, plus one
  ## at the top. Against the full scale all 199 collapse onto viridis' darkest
  ## end. 200 cells so that a 1% tail is two of them: with fewer, the single
  ## outlier IS the first percentile and trimming would not reach it.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var n = 200, x = [], y = [], cells = [], vals = [], fv = [];\n",
      "  for (var j = 0; j < n; j++) {\n",
      "    x.push((j % 20) * 2); y.push(Math.floor(j / 20) * 2);\n",
      "    cells.push('c' + j); vals.push(0);\n",
      "    fv.push(j === n - 1 ? 1000 : Math.round(j / (n - 2) * 60));\n",
      "  }\n",
      "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
      "    coordviews_data: {\n",
      "      cells: cells, n: n,\n",
      "      groups: { cluster: { values: vals, levels: ['a'],\n",
      "        colors: ['#636EFA'] } },\n",
      "      cat_extra: {}, cat_skipped: {},\n",
      "      fields: { 'meta:score': { label: 'score', v: fv,\n",
      "        min: 0, max: 1000, scale: 1000 } },\n",
      "      default_group: 'cluster',\n",
      "      projections: { umap: { x: x, y: y, ndim: 2 } },\n",
      "      default_projection: 'umap',\n",
      "      spaces: [{ id: 'umap', label: 'umap (expression)', x: x, y: y }],\n",
      "      clone: null, trekker: null\n",
      "    } } }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('200 cells') >= 0",
    timeout = 15000
  )
  app$run_js(cv_set_color_js("__field__meta:score"))
  app$wait_for_idle(timeout = 10000)

  ## How many distinct colours the cloud is drawn in: with the scale owned by the
  ## outlier the 60 all collapse onto viridis' darkest end.
  distinct <- paste0(
    "(function () {\n",
    "  var cv = document.getElementById('cv-cv-a');\n",
    "  var d = cv.getContext('2d').getImageData(0, 0, cv.width, cv.height).data;\n",
    "  var seen = {}, k = 0;\n",
    "  for (var i = 0; i < d.length; i += 4) {\n",
    "    if (d[i + 3] === 0) continue;\n",
    "    if (d[i] > 245 && d[i+1] > 245 && d[i+2] > 245) continue;\n",
    "    var key = (d[i] >> 3) + ',' + (d[i+1] >> 3) + ',' + (d[i+2] >> 3);\n",
    "    if (!seen[key]) { seen[key] = 1; k++; }\n",
    "  }\n",
    "  return k;\n",
    "})();"
  )
  clipped <- app$get_js(distinct)

  ## The control is offered for a continuous colouring, and turning it off puts
  ## the flat picture back -- which is what makes the number above meaningful.
  expect_true(app$get_js(
    "getComputedStyle(document.getElementById('cv-clip-ctl')).display !== 'none'"
  ))
  app$run_js(paste0(
    "(function () { var s = document.getElementById('cv-clip');\n",
    "  s.value = '0'; s.dispatchEvent(new Event('change', { bubbles: true }));",
    " })();"
  ))
  app$wait_for_idle(timeout = 10000)
  full <- app$get_js(distinct)
  expect_gt(clipped, full)

  ## The bar must report the range the COLOURS span, not the data's, or every
  ## saturated cell is misread as the maximum.
  app$run_js(paste0(
    "(function () { var s = document.getElementById('cv-clip');\n",
    "  s.value = '0.05'; s.dispatchEvent(new Event('change', { bubbles: true }));",
    " })();"
  ))
  app$wait_for_idle(timeout = 10000)
  expect_match(
    app$get_js("document.getElementById('cv-cb1').textContent"),
    "^≥"
  )

  app$stop()
})

## The Moran's I table names the genes whose expression is spatially structured.
## Reading one and then hunting for it in the gene picker is the gap between a
## number and the map that makes it mean something, so each row links straight
## to colouring by that gene. The table was built unlinked back when this
## workspace had no gene mode to send it to.
test_that("a Moran's I row colours the panels by that gene", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_moran_link")

  app$run_js(
    paste0(
      "(function () {\n",
      "  var n = 40, x = [], y = [], cells = [], vals = [];\n",
      "  for (var j = 0; j < n; j++) {\n",
      "    x.push((j % 8) * 2); y.push(Math.floor(j / 8) * 2);\n",
      "    cells.push('c' + j); vals.push(0);\n",
      "  }\n",
      "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
      "    coordviews_data: {\n",
      "      cells: cells, n: n,\n",
      "      groups: { cluster: { values: vals, levels: ['a'],\n",
      "        colors: ['#636EFA'] } },\n",
      "      cat_extra: {}, cat_skipped: {}, fields: {},\n",
      "      default_group: 'cluster',\n",
      "      projections: { umap: { x: x, y: y, ndim: 2 } },\n",
      "      default_projection: 'umap',\n",
      "      spaces: [{ id: 'umap', label: 'umap (expression)', x: x, y: y },\n",
      "        { id: 'trekker', label: 'Trekker (physical)', x: x, y: y,\n",
      "          unit: 'um' }],\n",
      "      clone: null,\n",
      "      trekker: { qc: { sample_id: 's1' },\n",
      "        moran: [{ rank: 1, gene: 'GENE1', I: 0.42 },\n",
      "          { rank: 2, gene: 'GENE2', I: 0.31 }] }\n",
      "    } } }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('40 cells') >= 0",
    timeout = 15000
  )

  ## Open the unified Trekker insights region and switch to Moran's I.
  app$run_js(
    paste0(
      "(function () { var b = document.querySelector(",
      "'.cv-tbtn[data-act=\"trekker-info\"]:not([style*=\"none\"])');\n",
      "  (b || document.querySelector('.cv-tbtn[data-act=\"trekker-info\"]'))",
      ".click(); })();"
    )
  )
  app$wait_for_js(
    "document.querySelectorAll('#cv-tk-morantbl a[data-g]').length === 2",
    timeout = 10000
  )
  expect_equal(
    app$get_js(
      "document.getElementById('cv-tk-insights-toggle').getAttribute('aria-expanded')"
    ),
    "true"
  )
  app$run_js("document.getElementById('cv-tk-tab-moran').click();")
  app$wait_for_js(
    "getComputedStyle(document.getElementById('cv-tk-panel-moran')).display !== 'none'",
    timeout = 5000
  )

  ## Clicking a row switches to gene colouring and asks for that gene while the
  ## insights region remains available for reading the other ranked genes.
  app$run_js(
    "document.querySelector('#cv-tk-morantbl a[data-g=\"GENE2\"]').click();"
  )
  app$wait_for_idle(timeout = 10000)
  expect_equal(
    app$get_js(
      "document.getElementById('cv-tk-insights-toggle').getAttribute('aria-expanded')"
    ),
    "true"
  )
  expect_equal(
    app$get_js("document.getElementById('cv-pick-color').value"),
    "__gene__"
  )
  expect_equal(
    app$get_js(
      paste0(
        "(function () { var el = document.getElementById('coordviews_gene');\n",
        "  return (el && el.selectize) ? el.selectize.getValue() : null; })();"
      )
    ),
    "GENE2"
  )

  app$stop()
})

## Focusing is a change of emphasis, not a trip into a different workspace. The
## chosen lens grows, while every other lens stays visible as linked context.
## Selection identity and provenance remain explicit throughout the transition.
test_that("a panel can become the focus without losing linked context", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_focus")

  blob4 <- paste0(
    "{ spaces: [",
    "{ id: 'umap', label: 'umap', x: blob(0), y: blob(0) },",
    "{ id: 'spatial', label: 'spatial', x: blob(0), y: blob(0) },",
    "{ id: 'trekker', label: 'trekker', x: blob(0), y: blob(0) },",
    "{ id: 'clone', label: 'clone', x: blob(0), y: blob(0) }] }"
  )
  app$run_js(cv_bundle_js(blob4))
  app$wait_for_js(
    "document.querySelectorAll('.cv-pane:not(.cv-hidden)').length === 4",
    timeout = 15000
  )
  app$wait_for_idle(timeout = 10000)

  ## On a narrow card the plotting tools must stay attached to the card's
  ## right edge. Focus owns the title row; the vertical tool strip begins
  ## beneath it instead of being pushed inward to make room.
  app$wait_for_js(
    "document.querySelector('.cv-pane.cv-narrow .cv-panebar') !== null",
    timeout = 10000
  )
  expect_equal(
    app$get_js(
      "getComputedStyle(document.querySelector('.cv-pane.cv-narrow .cv-panebar')).right"
    ),
    "10px"
  )
  expect_equal(
    app$get_js(
      "getComputedStyle(document.querySelector('.cv-pane.cv-narrow .cv-panebar')).top"
    ),
    "44px"
  )

  ## The linked-workspace model must be visible before the user discovers a
  ## hidden gesture. The persistent guide explains the two core actions, and
  ## every card exposes Focus as a labelled control rather than a hover icon.
  expect_true(app$get_js(
    "getComputedStyle(document.getElementById('cv-workspace-guide')).display !== 'none'"
  ))
  expect_match(
    app$get_js(
      "document.getElementById('cv-workspace-guide-text').textContent"
    ),
    "Drag in any view"
  )
  expect_equal(
    app$get_js(
      "document.querySelector('.cv-focus-btn[data-panel=\"A\"] .cv-focus-label').textContent.trim()"
    ),
    "Focus"
  )
  expect_equal(
    app$get_js(
      "getComputedStyle(document.getElementById('cv-role-a')).display"
    ),
    "none"
  )

  ## Focus has an explicit round trip before any cohort exists: the guide names
  ## the focused lens, cards name their roles, and the same panel's Exit focus
  ## action returns to the equal workspace without a duplicate global action.
  app$run_js(
    "document.querySelector('.cv-focus-btn[data-panel=\"A\"]').click();"
  )
  expect_true(app$get_js(
    "document.querySelector('.cv-panes').classList.contains('cv-focus-transitioning')"
  ))
  app$wait_for_idle(timeout = 10000)
  expect_match(
    app$get_js(
      "document.getElementById('cv-workspace-guide-text').textContent"
    ),
    "Focused view: umap"
  )
  expect_equal(
    app$get_js("document.getElementById('cv-role-a').textContent"),
    "FOCUS"
  )
  expect_equal(
    app$get_js("document.getElementById('cv-role-b').textContent"),
    "CONTEXT"
  )
  expect_equal(
    app$get_js(
      "document.querySelector('.cv-focus-btn[data-panel=\"A\"] .cv-focus-label').textContent.trim()"
    ),
    "Exit focus"
  )
  expect_equal(
    app$get_js(
      "document.querySelector('.cv-focus-btn[data-panel=\"A\"]').getAttribute('aria-label')"
    ),
    "Exit focus and return to overview"
  )
  expect_null(app$get_js("document.getElementById('cv-workspace-overview')"))
  app$wait_for_js(
    "!document.querySelector('.cv-panes').classList.contains('cv-focus-transitioning')",
    timeout = 10000
  )
  expect_true(app$get_js(
    "Array.from(document.querySelectorAll('.cv-pane')).every(function (p) { return p.style.transform === ''; })"
  ))
  app$run_js(
    "document.querySelector('.cv-focus-btn[data-panel=\"A\"]').click();"
  )
  app$wait_for_idle(timeout = 10000)
  app$wait_for_js(
    paste0(
      "!document.querySelector('.cv-panes')",
      ".classList.contains('cv-focus-transitioning')"
    ),
    timeout = 10000
  )
  expect_equal(
    app$get_js("document.querySelectorAll('.cv-focus-primary').length"),
    0
  )
  expect_match(
    app$get_js(
      "document.getElementById('cv-workspace-guide-text').textContent"
    ),
    "Drag in any view"
  )

  small <- app$get_js("document.getElementById('cv-cv-a').clientWidth")

  ## Select something first, so the claim about keeping it can be tested. Keep
  ## the brush well inside the plotting square: Focus changes the canvas size,
  ## so this also proves the committed outline remains registered to the data
  ## rather than to its old pixel coordinates.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var r = cv.getBoundingClientRect();\n",
      "  cv.dispatchEvent(new MouseEvent('mousedown',\n",
      "    { clientX: r.left + r.width * .2, clientY: r.top + r.height * .2, bubbles: true }));\n",
      "  var pts = [[r.width * .8, r.height * .2], [r.width * .8, r.height * .8],\n",
      "    [r.width * .2, r.height * .8], [r.width * .2, r.height * .2]];\n",
      "  pts.forEach(function (q) {\n",
      "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
      "      { clientX: r.left + q[0], clientY: r.top + q[1], bubbles: true }));\n",
      "  });\n",
      "  window.dispatchEvent(new MouseEvent('mouseup', { bubbles: true }));\n",
      "})();"
    )
  )
  app$wait_for_js(
    "document.getElementById('cv-seltext').textContent.indexOf('Selected') >= 0",
    timeout = 10000
  )
  app$wait_for_js(
    "document.getElementById('cv-workspace-guide').classList.contains('cv-collapse')",
    timeout = 10000
  )
  expect_true(app$get_js(
    "document.getElementById('cv-workspace-guide').classList.contains('cv-collapse')"
  ))
  before <- app$get_js("document.getElementById('cv-seltext').textContent")
  expect_equal(
    app$get_js("document.getElementById('cv-sel-kicker').textContent"),
    "Active cohort"
  )
  expect_match(
    app$get_js("document.getElementById('cv-selorigin').textContent"),
    "umap",
    ignore.case = TRUE
  )
  expect_true(nzchar(
    app$get_js("document.getElementById('cv-selprofile').textContent")
  ))

  ## Any workspace with another linked lens offers the focus affordance.
  expect_true(app$get_js(
    paste0(
      "getComputedStyle(document.querySelector(",
      "'.cv-focus-btn[data-panel=\"A\"]')).display !== 'none'"
    )
  ))

  ## Focus panel A. The linked contexts stay on screen, but A becomes larger.
  app$run_js(
    "document.querySelector('.cv-focus-btn[data-panel=\"A\"]').click();"
  )
  app$wait_for_idle(timeout = 10000)
  app$wait_for_js(
    paste0(
      "!document.querySelector('.cv-panes')",
      ".classList.contains('cv-focus-transitioning')"
    ),
    timeout = 10000
  )
  expect_equal(
    app$get_js(
      "document.querySelectorAll('.cv-pane:not(.cv-hidden)').length"
    ),
    4
  )
  expect_equal(
    app$get_js("document.querySelectorAll('.cv-focus-primary').length"),
    1
  )
  expect_equal(
    app$get_js("document.querySelectorAll('.cv-focus-context').length"),
    3
  )
  expect_gt(app$get_js("document.getElementById('cv-cv-a').clientWidth"), small)
  expect_gt(
    app$get_js(
      "document.getElementById('cv-cv-a').clientWidth"
    ),
    app$get_js("document.getElementById('cv-cv-b').clientWidth")
  )
  app$run_js(paste0(
    "(function(){window.__cvFocusRasterReady=false;var previous=null;",
    "var settle=function(){var cv=document.getElementById('cv-cv-a');",
    "var current=[cv.clientWidth,cv.clientHeight,cv.width,cv.height].join('|');",
    "if(current===previous){window.__cvFocusRasterReady=true;return;}",
    "previous=current;requestAnimationFrame(settle);};",
    "requestAnimationFrame(settle);})()"
  ))
  app$wait_for_js("window.__cvFocusRasterReady === true", timeout = 3000)
  ## The outline is stored in the data coordinate system, so after A grows it
  ## lands at its new projected position (not at the old 20%-of-canvas pixel).
  ## Test a small neighbourhood around the first corner: the dashed blue stroke
  ## is intentionally anti-aliased, hence a colour tolerance rather than an
  ## exact one-pixel match.
  outline_corner <- app$get_js(paste0(
    "(function () {",
    " var cv=document.getElementById('cv-cv-a'), cssW=cv.clientWidth;",
    " var oldW=",
    small,
    ", oldX=oldW*.2, oldS=oldW-32;",
    " var ux=(oldX-16)/oldS, wantCss=16+ux*(cssW-32);",
    " var sx=cv.width/cssW, sy=cv.height/cv.clientHeight;",
    " var wantX=wantCss*sx, wantY=wantCss*sy;",
    " var radius=Math.ceil(7*Math.max(sx,sy));",
    " var c=cv.getContext('2d'), image=c.getImageData(0,0,cv.width,cv.height),",
    " d=image.data, found=false;",
    " for(var y=Math.max(0,Math.floor(wantY)-radius);",
    " y<=Math.min(image.height-1,Math.ceil(wantY)+radius);y++)",
    "  for(var x=Math.max(0,Math.floor(wantX)-radius);",
    " x<=Math.min(image.width-1,Math.ceil(wantX)+radius);x++){",
    "   var i=(y*image.width+x)*4;",
    "   if(d[i]<80 && d[i+1]>70 && d[i+2]>150){found=true;break;}",
    "  }",
    " return {found:found,cssWidth:cssW,backingWidth:cv.width,",
    " scaleX:sx,scaleY:sy,wantCss:wantCss};",
    "})()"
  ))
  expect_true(
    isTRUE(outline_corner$found),
    info = jsonlite::toJSON(outline_corner, auto_unbox = TRUE)
  )
  ## Zooming the cohort in and back out is a view change, not a new selection:
  ## its committed outline must still be drawn afterwards. This is the same
  ## round trip as the top-level "Zoom to selection" action in the UI.
  app$run_js("document.getElementById('cv-zoom').click();")
  app$wait_for_js(
    "document.getElementById('cv-zoom').textContent.indexOf('Zoom back') >= 0",
    timeout = 10000
  )
  app$run_js("document.getElementById('cv-zoom').click();")
  app$wait_for_js(
    "document.getElementById('cv-zoom').textContent.indexOf('Zoom to selection') >= 0",
    timeout = 10000
  )
  outline_after_zoom_back <- app$get_js(paste0(
    "(function () {",
    " var cv=document.getElementById('cv-cv-a'), sx=cv.width/cv.clientWidth,",
    " sy=cv.height/cv.clientHeight, x=0.2*cv.width, y=0.2*cv.height,",
    " r=Math.ceil(7*Math.max(sx,sy)), d=cv.getContext('2d').getImageData(0,0,cv.width,cv.height).data;",
    " for(var yy=Math.max(0,Math.floor(y)-r);yy<=Math.min(cv.height-1,Math.ceil(y)+r);yy++)",
    "  for(var xx=Math.max(0,Math.floor(x)-r);xx<=Math.min(cv.width-1,Math.ceil(x)+r);xx++){",
    "   var i=(yy*cv.width+xx)*4;if(d[i]<80&&d[i+1]>70&&d[i+2]>150)return true;",
    "  }",
    " return false;",
    "})()"
  ))
  expect_true(isTRUE(outline_after_zoom_back))
  ## The selection is untouched -- this is magnification, not a reset.
  expect_equal(
    app$get_js("document.getElementById('cv-seltext').textContent"),
    before
  )

  ## Clicking a context title promotes that lens without disturbing selection.
  ## Ordinary plotting gestures do not reflow it.
  app$run_js(
    "document.getElementById('cv-title-b').click();"
  )
  app$wait_for_idle(timeout = 10000)
  expect_true(
    app$get_js(
      "document.getElementById('cv-cv-b').closest('.cv-pane').classList.contains('cv-focus-primary')"
    )
  )
  expect_equal(
    app$get_js("document.getElementById('cv-seltext').textContent"),
    before
  )

  ## Clicking the focused panel's button returns to the equal overview.
  app$run_js(
    "document.querySelector('.cv-focus-btn[data-panel=\"B\"]').click();"
  )
  app$wait_for_idle(timeout = 10000)
  expect_equal(
    app$get_js("document.querySelectorAll('.cv-focus-primary').length"),
    0
  )
  expect_equal(
    app$get_js("document.querySelectorAll('.cv-focus-context').length"),
    0
  )

  ## Double-clicking a plot is the second deliberate promotion gesture.
  app$run_js(
    paste0(
      "document.getElementById('cv-cv-c').dispatchEvent(",
      "new MouseEvent('dblclick', { bubbles: true }));"
    )
  )
  app$wait_for_idle(timeout = 10000)
  expect_true(
    app$get_js(
      "document.getElementById('cv-cv-c').closest('.cv-pane').classList.contains('cv-focus-primary')"
    )
  )
  app$run_js(
    "document.querySelector('.cv-focus-btn[data-panel=\"C\"]').click();"
  )
  app$wait_for_idle(timeout = 10000)

  ## Per-panel zoom-to-selection: offered while a selection exists, and it moves
  ## only the panel that asked. The top bar's button does the expression panel,
  ## which is no help for getting in close on the tissue.
  expect_true(app$get_js(
    paste0(
      "getComputedStyle(document.querySelector(",
      "'.cv-zsel-btn[data-panel=\"B\"]')).display !== 'none'"
    )
  ))
  app$run_js(
    "document.querySelector('.cv-zsel-btn[data-panel=\"B\"]').click();"
  )
  app$wait_for_idle(timeout = 10000)
  expect_true(app$get_js(
    "document.getElementById('cv-mini-b').classList.contains('is-on')"
  ))
  expect_false(app$get_js(
    "document.getElementById('cv-mini-a').classList.contains('is-on')"
  ))

  ## Two panels can still benefit from explicit emphasis: focusing is no longer
  ## an all-or-nothing maximise action, so the affordance is present whenever a
  ## second linked lens exists, at both wide and narrow widths.
  app$run_js(cv_bundle_js(
    paste0(
      "{ spaces: [",
      "{ id: 'umap', label: 'umap', x: blob(0), y: blob(0) },",
      "{ id: 'spatial', label: 'spatial', x: blob(0), y: blob(0) }] }"
    )
  ))
  app$wait_for_js(
    "document.querySelectorAll('.cv-pane:not(.cv-hidden)').length === 2",
    timeout = 15000
  )
  app$wait_for_idle(timeout = 10000)
  focus_shown <- paste0(
    "getComputedStyle(document.querySelector(",
    "'.cv-focus-btn[data-panel=\"A\"]')).display !== 'none'"
  )
  expect_true(app$get_js(focus_shown))

  ## Narrow the window until those same two panels stack, and it comes back.
  app$set_window_size(width = 700, height = 900)
  app$wait_for_idle(timeout = 10000)
  expect_true(app$get_js(focus_shown))

  app$stop()
})

## A physical position is an inference, not a measurement, so the card that
## describes a nucleus has to say how much to trust the one being read.
##
## The payload here is the REAL builder's, read out of the Trekker demo rather
## than written by hand. The first version of this test invented a
## `fields.bead_noise` that no builder produces, so it proved a contract that did
## not exist -- and missed that the real one lists position_confidence as a
## field, which the card was also printing from `conf`, twice under two names.
test_that("the Trekker inspector reports what is known about a position", {
  local_app_support(inst_dir)
  skip_if_not(nzchar(inst_dir))
  trekker_crb <- file.path(inst_dir, "extdata/examples/demo_trekker.crb")
  skip_if_not(file.exists(trekker_crb))

  ## Build the bundle exactly as the app does, then hand the client that.
  bundle_file <- file.path(inst_dir, "viewer/coordinated_views/bundle.R")
  contract_file <- file.path(inst_dir, "viewer/clone_contract.R")
  skip_if_not(file.exists(bundle_file) && file.exists(contract_file))
  benv <- new.env()
  sys.source(contract_file, envir = benv)
  sys.source(bundle_file, envir = benv)
  b <- benv$cv_build_bundle(readRDS(trekker_crb))
  skip_if_not(!is.null(b) && !is.null(b$trekker))
  evidence <- Filter(Negate(is.null), unclass(b$trekker$evidence_img))
  skip_if_not(length(evidence) > 0)
  ## Whichever positioned cell the geometry probe reaches should exercise the
  ## evidence UI; the bundle contract itself is tested separately above.
  b$trekker$evidence_img <- I(rep(evidence[[1]], b$n))

  app <- cv_app("cv_browser_card_trekker")
  app$set_window_size(width = 1440, height = 900)
  app$run_js(paste0(
    "Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
    "  coordviews_data: ",
    jsonlite::toJSON(b, auto_unbox = TRUE, null = "null", digits = 6),
    " } }));"
  ))
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.length > 0",
    timeout = 20000
  )
  app$wait_for_idle(timeout = 10000)

  ## Click a cell in the Trekker panel and ask for its details. Probing outward
  ## from the centre: a real tissue section is not a grid, so the exact middle
  ## need not carry a nucleus.
  app$run_js(paste0(
    "(function () {\n",
    "  var cv = document.getElementById('cv-cv-a');\n",
    "  var r = cv.getBoundingClientRect();\n",
    "  for (var d = 0; d < 200; d += 6) {\n",
    "    var x = r.left + r.width / 2 + d, y = r.top + r.height / 2;\n",
    "    cv.dispatchEvent(new MouseEvent('mousemove',\n",
    "      { clientX: x, clientY: y, bubbles: true }));\n",
    ## The handler writes the INLINE opacity synchronously; the computed one is
    ## mid-transition and still reads 0 inside a loop like this.
    "    if (document.getElementById('cv-tip-a').style.opacity === '1') {\n",
    "      cv.dispatchEvent(new MouseEvent('mousedown',\n",
    "        { clientX: x, clientY: y, bubbles: true }));\n",
    "      window.dispatchEvent(new MouseEvent('mouseup',\n",
    "        { clientX: x, clientY: y, bubbles: true }));\n",
    "      return;\n",
    "    }\n",
    "  }\n",
    "})();"
  ))
  ## A Trekker cell click opens the inspector directly, matching the old page.
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-tk-insights-toggle')",
      ".getAttribute('aria-expanded') === 'true' && ",
      "document.getElementById('cv-tk-cell-body').textContent.length > 0"
    ),
    timeout = 10000
  )

  card <- app$get_js("document.getElementById('cv-tk-cell-body').textContent")
  expect_match(card, "Positioning")
  ## The field labels are the builder's own -- "Position confidence", not a name
  ## chosen here -- which is the point of driving this from the real bundle.
  expect_match(card, "Position confidence")
  expect_match(card, "Spatial purity")
  ## The two numbers the dedicated page prints beside confidence, named as it
  ## names them, so a reader moving between the two reads the same quantities.
  expect_match(card, "bead noise")
  expect_match(card, "spatial barcodes")
  ## ... and confidence appears ONCE. It is a field AND it is in `conf`; the card
  ## printed both, twice under two names.
  expect_equal(
    app$get_js(
      paste0(
        "(document.getElementById('cv-tk-cell-body').textContent",
        ".match(/onfidence/g) || []).length"
      )
    ),
    1
  )
  expect_equal(
    app$get_js(
      "document.querySelectorAll('#cv-tk-cell-body .cv-evidence-thumb img').length"
    ),
    1
  )
  expect_equal(
    app$get_js(
      "document.querySelectorAll('#cv-tk-cell-body > .cv-tk-cell-block').length"
    ),
    4
  )
  expect_equal(
    app$get_js(paste0(
      "new Set(Array.from(document.querySelectorAll(",
      "'#cv-tk-cell-body > .cv-tk-cell-block')).map(function(el){",
      "return Math.round(el.getBoundingClientRect().top); })).size"
    )),
    1
  )
  app$run_js(
    "document.querySelector('#cv-tk-cell-body .cv-evidence-thumb').click();"
  )
  app$wait_for_js(
    "document.getElementById('cv-evidence-modal').open",
    timeout = 5000
  )
  expect_match(
    app$get_js("document.querySelector('#cv-evidence-modal img').src"),
    "data:image/"
  )

  app$stop()
})

## A histology preset can carry scaleX != scaleY: a calibration that is genuinely
## non-uniform. One slider had to pick a single number for both, and every
## control in the bar rewrote the pair from it -- so nudging the opacity squared
## the image up and threw the calibration away without saying so.
##
## The bar itself is server-rendered and only appears for a data set carrying an
## image, which the app under test does not have; test-coordinated-views.R pins
## what the server emits. What is exercised here is the client behaviour, with
## the controls put in place directly -- the preset still arrives the way it
## normally does, in the bundle.
test_that("a non-uniform image calibration survives the other controls", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_img_scale")

  ## A 1x1 transparent PNG is enough: this is about the transform, not the pixels.
  png1 <- paste0(
    "data:image/png;base64,",
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8",
    "z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  )
  app$run_js(cv_bundle_js(
    paste0(
      "{ spaces: [{ id: 'umap', label: 'umap', x: blob(0), y: blob(0) },\n",
      "  { id: 'spatial', label: 'spatial', x: blob(0), y: blob(0),\n",
      "    image: { uri: '",
      png1,
      "',\n",
      "      preset: { scaleX: 1.4, scaleY: 0.7, opacity: 0.6 } },\n",
      "    coord_span: [400, 400] }] }"
    )
  ))
  app$wait_for_js(
    "document.querySelectorAll('.cv-pane:not(.cv-hidden)').length === 2",
    timeout = 15000
  )

  ## Put the bar's controls in place, seeded from the preset exactly as the
  ## server seeds them.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var host = document.createElement('div');\n",
      "  host.id = 'cv-test-imgbar';\n",
      "  host.innerHTML =\n",
      "    '<input type=\"range\" id=\"cv-img-opacity\" min=\"0\" max=\"1\"' +\n",
      "    ' step=\"0.05\" value=\"0.6\">' +\n",
      "    '<input type=\"range\" id=\"cv-img-scalex\" min=\"0.3\" max=\"3\"' +\n",
      "    ' step=\"0.02\" value=\"1.4\">' +\n",
      "    '<input type=\"range\" id=\"cv-img-scaley\" min=\"0.3\" max=\"3\"' +\n",
      "    ' step=\"0.02\" value=\"0.7\">' +\n",
      "    '<input type=\"checkbox\" id=\"cv-img-lock\">' +\n",
      "    '<input type=\"range\" id=\"cv-img-rotate\" min=\"-180\" max=\"180\"' +\n",
      "    ' step=\"1\" value=\"0\">' +\n",
      "    '<button type=\"button\" id=\"cv-img-reset\">Reset</button>';\n",
      "  document.body.appendChild(host);\n",
      "})();"
    )
  )

  scales <- function() {
    unlist(app$get_js(
      paste0(
        "[parseFloat(document.getElementById('cv-img-scalex').value),\n",
        " parseFloat(document.getElementById('cv-img-scaley').value)]"
      )
    ))
  }

  ## Moving an unrelated control must leave both scales alone. This is the
  ## regression: one slider for two axes meant any input rewrote the pair.
  app$run_js(
    paste0(
      "(function () { var el = document.getElementById('cv-img-opacity');\n",
      "  el.value = '0.3';\n",
      "  el.dispatchEvent(new Event('input', { bubbles: true })); })();"
    )
  )
  app$wait_for_idle(timeout = 5000)
  expect_equal(scales(), c(1.4, 0.7))

  ## With the aspect locked, one axis follows the other -- the common case, and
  ## the reason a single slider existed at all.
  app$run_js(
    paste0(
      "(function () { var l = document.getElementById('cv-img-lock');\n",
      "  l.checked = true;\n",
      "  l.dispatchEvent(new Event('change', { bubbles: true }));\n",
      "  var x = document.getElementById('cv-img-scalex');\n",
      "  x.value = '2';\n",
      "  x.dispatchEvent(new Event('input', { bubbles: true })); })();"
    )
  )
  app$wait_for_idle(timeout = 5000)
  expect_equal(scales(), c(2, 2))

  ## And the preset is reachable again without reloading the page.
  app$run_js("document.getElementById('cv-img-reset').click();")
  app$wait_for_idle(timeout = 5000)
  expect_equal(scales(), c(1.4, 0.7))
  expect_false(app$get_js("document.getElementById('cv-img-lock').checked"))

  app$stop()
})


## Switching spatial section is where the alignment bar goes wrong, and the
## earlier test could not see it: it exercised one section only. The bundle here
## carries two real sections with different presets and the switch goes through
## setSpatialSample() exactly as the picker drives it.
##
## The bar's MARKUP is server-rendered from the server's own bundle, so a pushed
## one cannot produce it; it is put in place here seeded as the server seeds it,
## from the section showing at the time. What is under test is what happens to it
## on the switch. That the server renders a bar at all when only a LATER section
## carries the image is pinned in test-coordinated-views.R.
test_that("the image controls follow the spatial section", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_sample_switch")

  png1 <- paste0(
    "data:image/png;base64,",
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8",
    "z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  )
  app$run_js(cv_bundle_js(
    paste0(
      "{ spaces: [{ id: 'umap', label: 'umap', x: blob(0), y: blob(0) },\n",
      "  { id: 'spatial', label: 'A (spatial)', x: blob(0), y: blob(0),\n",
      "    coord_span: [400, 400],\n",
      "    image: { uri: '",
      png1,
      "',\n",
      "      preset: { scaleX: 1, scaleY: 1, opacity: 0.6, offsetX: 0 } },\n",
      "    samples: [\n",
      "      { name: 'A', label: 'A (spatial)', x: blob(0), y: blob(0),\n",
      "        image: { uri: '",
      png1,
      "',\n",
      "          preset: { scaleX: 1, scaleY: 1, opacity: 0.6, offsetX: 0 } } },\n",
      "      { name: 'B', label: 'B (spatial)', x: blob(0), y: blob(0),\n",
      "        image: { uri: '",
      png1,
      "',\n",
      "          preset: { scaleX: 1.4, scaleY: 0.7, opacity: 0.5,\n",
      "            offsetX: 30, flipX: true } } }] }] }"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-pick-spatial') !== null",
    timeout = 15000
  )

  ## The bar as the server would have rendered it for section A.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var host = document.createElement('div');\n",
      "  host.innerHTML =\n",
      "    '<input type=\"range\" id=\"cv-img-opacity\" min=\"0\" max=\"1\"' +\n",
      "    ' step=\"0.05\" value=\"0.6\">' +\n",
      "    '<input type=\"range\" id=\"cv-img-offx\" min=\"-480\" max=\"480\"' +\n",
      "    ' step=\"2\" value=\"0\">' +\n",
      "    '<input type=\"range\" id=\"cv-img-offy\" min=\"-480\" max=\"480\"' +\n",
      "    ' step=\"2\" value=\"0\">' +\n",
      "    '<input type=\"range\" id=\"cv-img-scalex\" min=\"0.3\" max=\"3\"' +\n",
      "    ' step=\"0.02\" value=\"1\">' +\n",
      "    '<input type=\"range\" id=\"cv-img-scaley\" min=\"0.3\" max=\"3\"' +\n",
      "    ' step=\"0.02\" value=\"1\">' +\n",
      "    '<input type=\"checkbox\" id=\"cv-img-lock\" checked>' +\n",
      "    '<input type=\"range\" id=\"cv-img-rotate\" min=\"-180\" max=\"180\"' +\n",
      "    ' step=\"1\" value=\"0\">' +\n",
      "    '<input type=\"checkbox\" id=\"cv-img-flipx\">' +\n",
      "    '<input type=\"checkbox\" id=\"cv-img-flipy\">' +\n",
      "    '<input type=\"checkbox\" id=\"cv-img-show\" checked>' +\n",
      "    '<button type=\"button\" id=\"cv-img-reset\">Reset</button>';\n",
      "  document.body.appendChild(host);\n",
      "})();"
    )
  )

  scales <- function() {
    unlist(app$get_js(
      paste0(
        "[parseFloat(document.getElementById('cv-img-scalex').value),\n",
        " parseFloat(document.getElementById('cv-img-scaley').value)]"
      )
    ))
  }
  expect_equal(scales(), c(1, 1))

  ## Switch to section B, whose calibration is non-uniform.
  app$run_js(paste0(
    "(function () { var s = document.getElementById('cv-pick-spatial');\n",
    "  if (s.selectize) s.selectize.setValue(['B']); else {",
    "s.value = 'B'; s.dispatchEvent(new Event('change'));} })();"
  ))
  app$wait_for_idle(timeout = 10000)

  ## The controls read B now. They used to still read A, so the first nudge to
  ## any of them wrote A's alignment back over B's.
  expect_equal(scales(), c(1.4, 0.7))
  expect_true(app$get_js("document.getElementById('cv-img-flipx').checked"))
  expect_equal(
    app$get_js("parseFloat(document.getElementById('cv-img-offx').value)"),
    30
  )

  ## ... and a nudge to something unrelated leaves B's calibration alone.
  app$run_js(
    paste0(
      "(function () { var el = document.getElementById('cv-img-opacity');\n",
      "  el.value = '0.3';\n",
      "  el.dispatchEvent(new Event('input', { bubbles: true })); })();"
    )
  )
  app$wait_for_idle(timeout = 5000)
  expect_equal(scales(), c(1.4, 0.7))

  app$stop()
})

## Between asking for a gene and being answered, the picker already showed the
## new name while the points and the colourbar still showed the OLD gene -- the
## workspace naming one thing and drawing another. And a reply the server could
## not fulfil was dropped, so a gene that does not exist left the previous one's
## colours on screen under its name.
test_that("a gene request does not leave the previous gene on screen", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_gene_request")

  app$run_js(cv_bundle_js())
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('800 cells') >= 0",
    timeout = 15000
  )

  ## Draw a gene by hand: push the vector the server would have sent.
  app$run_js(paste0(
    "(function () {\n",
    "  var v = []; for (var i = 0; i < 800; i++) v.push(i % 256);\n",
    "  var s = document.getElementById('cv-pick-color');\n",
    "  if(s.selectize)s.selectize.setValue('__gene__');\n",
    "  else{s.value='__gene__';s.dispatchEvent(new Event('change',{bubbles:true}));}\n",
    "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
    "    coordviews_geneval: { gene: 'GENE1', ok: true, v: v, max: 5 } } }));\n",
    "})();"
  ))
  app$wait_for_idle(timeout = 10000)
  ink <- cv_ink_js()
  drawn <- app$get_js(ink)
  expect_gt(drawn, 1)
  expect_match(
    app$get_js("document.getElementById('cv-cbar-note').textContent"),
    "GENE1"
  )

  ## Now ask for one the server cannot provide. The failure reply must clear the
  ## previous gene rather than be ignored.
  app$run_js(paste0(
    "(function () {\n",
    "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
    "    coordviews_geneval: { gene: 'NOPE', ok: false } } }));\n",
    "})();"
  ))
  app$wait_for_idle(timeout = 10000)
  expect_match(
    app$get_js("document.getElementById('cv-cbar-note').textContent"),
    "not available"
  )
  ## GENE1's colours are gone: what remains is the neutral no-gene grey, which
  ## is a different picture from the viridis one measured above.
  expect_false(app$get_js(
    paste0(
      "(function () {\n",
      "  var cv = document.getElementById('cv-cv-a');\n",
      "  var d = cv.getContext('2d')\n",
      "    .getImageData(0, 0, cv.width, cv.height).data;\n",
      "  for (var i = 0; i < d.length; i += 4) {\n",
      "    if (d[i + 3] === 0) continue;\n",
      "    if (d[i] > d[i + 2] + 40 && d[i + 1] > 120) return true;\n",
      "  }\n",
      "  return false;\n",
      "})();"
    )
  ))

  ## A late reply for a gene nobody is waiting for any more is ignored.
  app$run_js(paste0(
    "(function () {\n",
    "  var s = document.getElementById('coordviews_gene');\n",
    "  if (s && s.selectize) {\n",
    "    s.selectize.addOption({ value: 'GENE3', label: 'GENE3' });\n",
    "    s.selectize.setValue('GENE3', false);\n",
    "  }\n",
    "  var v = []; for (var i = 0; i < 800; i++) v.push(200);\n",
    "  Shiny.shinyapp.dispatchMessage(JSON.stringify({ custom: {\n",
    "    coordviews_geneval: { gene: 'GENE1', ok: true, v: v, max: 5 } } }));\n",
    "})();"
  ))
  app$wait_for_idle(timeout = 10000)
  expect_false(grepl(
    "GENE1",
    app$get_js("document.getElementById('cv-cbar-note').textContent"),
    fixed = TRUE
  ))

  app$stop()
})


## Alignment belongs to a (section, background image) PAIR. One shared state let
## the numbers follow the user from one slide to another, so a calibration ended
## up describing an image it was never made for. Two sections, two backgrounds
## each, all four with different presets: the test is that nothing crosses over
## and that going back returns what was left behind.
test_that("each section and background keeps its own alignment", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_img_identity")

  png1 <- paste0(
    "data:image/png;base64,",
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8",
    "z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  )
  img <- function(id, label, sx, sy) {
    paste0(
      "{ id: '",
      id,
      "', label: '",
      label,
      "', uri: '",
      png1,
      "',\n",
      "  coord_span: [400, 400],\n",
      "  preset: { scaleX: ",
      sx,
      ", scaleY: ",
      sy,
      ", opacity: 0.6,\n",
      "    offsetX: 0, offsetY: 0 } }"
    )
  }
  app$run_js(cv_bundle_js(
    paste0(
      "{ spaces: [{ id: 'umap', label: 'umap', x: blob(0), y: blob(0) },\n",
      "  { id: 'spatial', label: 'A (spatial)', x: blob(0), y: blob(0),\n",
      "    images: [",
      img("a1", "A one", 1.0, 1.0),
      ",\n",
      "             ",
      img("a2", "A two", 2.0, 2.0),
      "],\n",
      "    samples: [\n",
      "      { name: 'A', label: 'A (spatial)', x: blob(0), y: blob(0),\n",
      "        images: [",
      img("a1", "A one", 1.0, 1.0),
      ",\n",
      "                 ",
      img("a2", "A two", 2.0, 2.0),
      "] },\n",
      "      { name: 'B', label: 'B (spatial)', x: blob(0), y: blob(0),\n",
      "        images: [",
      img("b1", "B one", 1.4, 0.7),
      "] }] }] }"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-pick-spatial') !== null",
    timeout = 15000
  )

  ## Section A has two backgrounds; both are listed, after the standing option
  ## of showing none.
  expect_true(app$get_js(
    paste0(
      "getComputedStyle(document.getElementById('cv-img-pick-ctl'))",
      ".display !== 'none'"
    )
  ))
  expect_equal(
    app$get_js(
      paste0(
        "Array.from(document.getElementById('cv-img-pick').options)",
        ".map(function (o) { return o.value; })"
      )
    ),
    list("__none__", "a1", "a2")
  )

  ## The bar itself is server-rendered, so put it in place seeded from a1.
  app$run_js(
    paste0(
      "(function () {\n",
      "  var host = document.createElement('div');\n",
      "  host.innerHTML =\n",
      "    '<input type=\"range\" id=\"cv-img-opacity\" min=\"0\" max=\"1\"' +\n",
      "    ' step=\"0.05\" value=\"0.6\">' +\n",
      "    '<input type=\"range\" id=\"cv-img-offx\" min=\"-480\" max=\"480\"' +\n",
      "    ' step=\"2\" value=\"0\">' +\n",
      "    '<input type=\"range\" id=\"cv-img-offy\" min=\"-480\" max=\"480\"' +\n",
      "    ' step=\"2\" value=\"0\">' +\n",
      "    '<input type=\"range\" id=\"cv-img-scalex\" min=\"0.3\" max=\"3\"' +\n",
      "    ' step=\"0.02\" value=\"1\">' +\n",
      "    '<input type=\"range\" id=\"cv-img-scaley\" min=\"0.3\" max=\"3\"' +\n",
      "    ' step=\"0.02\" value=\"1\">' +\n",
      "    '<input type=\"checkbox\" id=\"cv-img-lock\" checked>' +\n",
      "    '<input type=\"range\" id=\"cv-img-rotate\" min=\"-180\" max=\"180\"' +\n",
      "    ' step=\"1\" value=\"0\">' +\n",
      "    '<input type=\"checkbox\" id=\"cv-img-flipx\">' +\n",
      "    '<input type=\"checkbox\" id=\"cv-img-flipy\">' +\n",
      "    '<input type=\"checkbox\" id=\"cv-img-show\" checked>' +\n",
      "    '<button type=\"button\" id=\"cv-img-reset\">Reset</button>';\n",
      "  document.body.appendChild(host);\n",
      "})();"
    )
  )
  scales <- function() {
    unlist(app$get_js(
      paste0(
        "[parseFloat(document.getElementById('cv-img-scalex').value),\n",
        " parseFloat(document.getElementById('cv-img-scaley').value)]"
      )
    ))
  }
  pick_img <- function(id) {
    app$run_js(paste0(
      "(function () { var s = document.getElementById('cv-img-pick');\n",
      "  s.value = '",
      id,
      "'; s.onchange(); })();"
    ))
    app$wait_for_idle(timeout = 8000)
  }
  pick_sample <- function(name) {
    app$run_js(paste0(
      "(function () { var s = document.getElementById('cv-pick-spatial');\n",
      "  if (s.selectize) s.selectize.setValue(['",
      name,
      "']); else {s.value = '",
      name,
      "'; s.dispatchEvent(new Event('change'));} })();"
    ))
    app$wait_for_idle(timeout = 8000)
  }
  nudge <- function(v) {
    app$run_js(paste0(
      "(function () { var x = document.getElementById('cv-img-scalex');\n",
      "  x.value = '",
      v,
      "';\n",
      "  x.dispatchEvent(new Event('input', { bubbles: true })); })();"
    ))
    app$wait_for_idle(timeout = 5000)
  }

  ## Adjust A/a1, then switch background within the same section.
  nudge(2.5)
  expect_equal(scales(), c(2.5, 2.5))
  pick_img("a2")
  ## a2's own preset, not a1's adjustment.
  expect_equal(scales(), c(2, 2))

  ## Back to a1: the work done to it is returned, not thrown away. Comparing two
  ## backgrounds would otherwise mean re-aligning on every switch.
  pick_img("a1")
  expect_equal(scales(), c(2.5, 2.5))

  ## Switch section. B's single background has its own, non-uniform calibration.
  pick_sample("B")
  expect_equal(scales(), c(1.4, 0.7))
  ## One background, and still a choice: show it or show none.
  expect_equal(
    app$get_js(
      paste0(
        "Array.from(document.getElementById('cv-img-pick').options)",
        ".map(function (o) { return o.value; })"
      )
    ),
    list("__none__", "b1")
  )

  ## Back to A: still on a1, still carrying its adjustment.
  pick_sample("A")
  expect_equal(scales(), c(2.5, 2.5))
  expect_equal(app$get_js("document.getElementById('cv-img-pick').value"), "a1")

  ## Reset touches only the pair on screen: a1 goes back to its preset, a2 keeps
  ## what it had.
  app$run_js("document.getElementById('cv-img-reset').click();")
  app$wait_for_idle(timeout = 5000)
  expect_equal(scales(), c(1, 1))
  pick_img("a2")
  expect_equal(scales(), c(2, 2))

  app$stop()
})


## Multi-section bundles keep the large image payloads on each sample instead
## of duplicating the opening sample's base64 data at the top level. The opening
## section must nevertheless expose its backgrounds before the user changes the
## Spatial data picker once.
test_that("the opening spatial sample exposes its nested backgrounds", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_img_nested_initial")

  png1 <- paste0(
    "data:image/png;base64,",
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8",
    "z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  )
  image <- function(id, with_uri = TRUE) {
    paste0(
      "{ id: '",
      id,
      "', label: 'Embedded histology'",
      if (with_uri) paste0(", uri: '", png1, "'") else "",
      ", coord_span: [400, 400],",
      " preset: { scaleX: 1, scaleY: 1, opacity: 0.6 } }"
    )
  }
  app$run_js(cv_bundle_js(
    paste0(
      "{ spaces: [{ id: 'umap', label: 'umap', x: blob(0), y: blob(0) },\n",
      "  { id: 'spatial', label: 'A (spatial)', x: blob(0), y: blob(0),\n",
      "    image: ",
      image("embedded", FALSE),
      ",\n",
      "    samples: [\n",
      "      { name: 'A', label: 'A (spatial)', x: blob(0), y: blob(0),\n",
      "        image: ",
      image("embedded", FALSE),
      ",\n",
      "        images: [",
      image("embedded"),
      "] },\n",
      "      { name: 'B', label: 'B (spatial)', x: blob(0), y: blob(0),\n",
      "        image: ",
      image("embedded", FALSE),
      ",\n",
      "        images: [",
      image("embedded"),
      "] }] }] }"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-img-pick').options.length > 0",
    timeout = 15000
  )

  expect_equal(
    app$get_js(
      paste0(
        "Array.from(document.getElementById('cv-img-pick').options)",
        ".map(function (o) { return o.value; })"
      )
    ),
    list("__none__", "embedded")
  )
  expect_false(app$get_js("document.getElementById('cv-img-pick').disabled"))
  expect_equal(
    app$get_js("document.getElementById('cv-img-pick').value"),
    "embedded"
  )

  app$stop()
})


## The picker used to hide itself when a section had fewer than two backgrounds,
## which is exactly when a reader wonders where the control went -- and left no
## way to turn a single image off from the same place it is chosen. "None" is a
## real answer: the tissue photo can be the thing in the way of seeing the cells.
test_that("the background picker is offered even with one image or none", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_img_none")

  png1 <- paste0(
    "data:image/png;base64,",
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8",
    "z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  )
  pick_opts <- paste0(
    "Array.from(document.getElementById('cv-img-pick').options)",
    ".map(function (o) { return o.value; })"
  )
  pick_shown <- paste0(
    "getComputedStyle(document.getElementById('cv-img-pick-ctl'))",
    ".display !== 'none'"
  )

  ## One background: still offered, with None beside it.
  app$run_js(cv_bundle_js(
    paste0(
      "{ spaces: [{ id: 'umap', label: 'umap', x: blob(0), y: blob(0) },\n",
      "  { id: 'spatial', label: 'A (spatial)', x: blob(0), y: blob(0),\n",
      "    images: [{ id: 'only', label: 'Only one', uri: '",
      png1,
      "',\n",
      "      coord_span: [400, 400],\n",
      "      preset: { scaleX: 1, scaleY: 1, opacity: 0.6 } }] }] }"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-img-pick').options.length > 0",
    timeout = 15000
  )
  expect_true(app$get_js(pick_shown))
  expect_equal(app$get_js(pick_opts), list("__none__", "only"))
  expect_equal(
    app$get_js("document.getElementById('cv-img-pick').value"),
    "only"
  )

  ## Choosing None is accepted and held. That it also takes the alignment bar
  ## away is not assertable here -- the bar is server-rendered from the SERVER's
  ## bundle, and this app's data set has no image, so the container is empty
  ## rather than hidden either way. The rule is pinned in
  ## test-coordinated-views.R and was checked on the running app.
  app$run_js(paste0(
    "(function () { var s = document.getElementById('cv-img-pick');\n",
    "  s.value = '__none__'; s.onchange(); })();"
  ))
  app$wait_for_idle(timeout = 8000)
  expect_equal(
    app$get_js("document.getElementById('cv-img-pick').value"),
    "__none__"
  )

  ## A section with no background at all still shows the control, saying so.
  app$run_js(cv_bundle_js(
    paste0(
      "{ spaces: [{ id: 'umap', label: 'umap', x: blob(0), y: blob(0) },\n",
      "  { id: 'spatial', label: 'A (spatial)', x: blob(0), y: blob(0) }] }"
    )
  ))
  app$wait_for_js(
    "document.querySelectorAll('.cv-pane:not(.cv-hidden)').length === 2",
    timeout = 15000
  )
  expect_true(app$get_js(pick_shown))
  expect_equal(app$get_js(pick_opts), list("__none__"))
  expect_true(app$get_js("document.getElementById('cv-img-pick').disabled"))

  app$stop()
})

test_that("point size sits with point opacity", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_ps_row")
  app$run_js(cv_bundle_js())
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('800 cells') >= 0",
    timeout = 15000
  )
  ## They adjust the same marks in the same way, so they belong together rather
  ## than one in the always-visible bar and the other behind "More".
  expect_true(app$get_js(
    paste0(
      "(function () {\n",
      "  var ps = document.getElementById('cv-ps');\n",
      "  var op = document.getElementById('cv-opacity');\n",
      "  if (!ps || !op) return false;\n",
      "  return ps.closest('.cv-more-inner') === op.closest('.cv-more-inner') &&\n",
      "    ps.closest('.cv-more-inner') !== null;\n",
      "})();"
    )
  ))
  ## ... and it still drives the drawing.
  app$run_js(paste0(
    "(function () { var ps = document.getElementById('cv-ps');\n",
    "  ps.value = '7';\n",
    "  ps.dispatchEvent(new Event('input', { bubbles: true })); })();"
  ))
  app$wait_for_idle(timeout = 5000)
  expect_gt(app$get_js(cv_ink_js()), 1)

  app$stop()
})

test_that("More settings overlays the workspace and groups point and histology controls", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_more_overlay")
  on.exit(app$stop(), add = TRUE)

  app$run_js(cv_bundle_js(paste0(
    "{ spaces: [{ id: 'umap', label: 'umap', x: blob(0), y: blob(0) },",
    "{ id: 'spatial', label: 'A (spatial)', x: blob(0), y: blob(0),",
    "images: [{ id: 'one', label: 'H&E', uri: 'data:image/png;base64,iVBORw0KGgo=',",
    "preset: { opacity: 0.6 } }] }] }"
  )))
  app$wait_for_js(
    "document.getElementById('cv-more-btn') !== null",
    timeout = 15000
  )
  app$set_window_size(width = 1619, height = 700)
  app$run_js("document.querySelector('.content-wrapper').scrollTop=120;")
  scroll_before <- app$get_js(
    "document.querySelector('.content-wrapper').scrollTop"
  )
  panel_before <- unlist(app$get_js(paste0(
    "(function(){var r=document.getElementById('cv-cv-a').getBoundingClientRect();",
    "return [r.left,r.top,r.width,r.height];})()"
  )))
  app$run_js("document.getElementById('cv-more-btn').click();")
  app$wait_for_js(
    paste0(
      "(function(){var x=document.getElementById('cv-more'); return x && ",
      "x.classList.contains('is-open') && getComputedStyle(x).transform === 'none';})()"
    ),
    timeout = 5000
  )

  expect_equal(
    app$get_js("getComputedStyle(document.getElementById('cv-more')).position"),
    "fixed"
  )
  expect_true(app$get_js(paste0(
    "(function(){var r=document.getElementById('cv-more').getBoundingClientRect();",
    "return Math.abs(r.right-innerWidth) <= 1 && Math.abs(r.top) <= 1 && ",
    "Math.abs(r.height-innerHeight) <= 1 && r.width >= 360 && r.width <= 440;})()"
  )))
  expect_true(app$get_js(
    "document.querySelector('#cv-more [data-cv-bg-mode]') !== null"
  ))
  expect_true(app$get_js(
    "document.querySelector('#cv-more .cv-more-points #cv-ps') !== null"
  ))
  panel_after <- unlist(app$get_js(paste0(
    "(function(){var r=document.getElementById('cv-cv-a').getBoundingClientRect();",
    "return [r.left,r.top,r.width,r.height];})()"
  )))
  expect_equal(panel_after, panel_before, tolerance = 1)
  expect_equal(
    app$get_js("document.querySelector('.content-wrapper').scrollTop"),
    scroll_before
  )
})

test_that("top pickers and More sliders keep one shared control geometry", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_control_geometry")
  on.exit(app$stop(), add = TRUE)

  app$run_js(cv_bundle_js(paste0(
    "{ projections:{umap:{x:blob(0),y:blob(0),ndim:2},",
    "tsne:{x:blob(1),y:blob(1),ndim:2}},",
    "spaces:[{id:'umap',label:'umap',x:blob(0),y:blob(0)},",
    "{id:'spatial',label:'A tissue (spatial)',x:blob(0),y:blob(0),",
    "samples:[{name:'A tissue',label:'A tissue (spatial)',x:blob(0),y:blob(0),images:[]},",
    "{name:'B tissue',label:'B tissue (spatial)',x:blob(1),y:blob(1),images:[]}]},",
    "{id:'trekker',label:'Physical (Trekker)',x:blob(0),y:blob(0)}],",
    "trekker:{conf:new Array(n).fill(1),evidence:new Array(n).fill(1)}}"
  )))
  app$wait_for_js(
    "document.getElementById('cv-pick-proj').selectize != null",
    timeout = 15000
  )
  app$run_js(
    "document.getElementById('cv-pick-proj').selectize.setValue(['umap','tsne']);"
  )
  app$run_js(
    "document.getElementById('cv-pick-spatial').selectize.setValue(['A tissue']);"
  )
  app$wait_for_js(
    "getComputedStyle(document.getElementById('cv-spatial-ctl')).display !== 'none'",
    timeout = 5000
  )
  app$run_js("document.getElementById('cv-more-btn').click();")
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-more').classList.contains('is-open') && ",
      "getComputedStyle(document.getElementById('cv-more')).transform === 'none'"
    ),
    timeout = 5000
  )

  heights <- unlist(app$get_js(paste0(
    "['#cv-pick-color + .selectize-control .selectize-input',",
    "'#cv-proj-ctl .selectize-input',",
    "'#cv-spatial-ctl .selectize-input'].map(function(q){",
    "return Math.round(document.querySelector(q).getBoundingClientRect().height);})"
  )))
  expect_lte(max(heights) - min(heights), 1)
  expect_equal(
    app$get_js(
      "document.querySelectorAll('.cv-more-points .cv-ctl-range').length"
    ),
    3
  )
  label_lefts <- unlist(app$get_js(paste0(
    "Array.from(document.querySelectorAll('.cv-more-points .cv-ctl-range > label'))",
    ".map(function(x){return Math.round(x.getBoundingClientRect().left);})"
  )))
  expect_lte(max(label_lefts) - min(label_lefts), 1)
  expect_true(app$get_js(paste0(
    "Array.from(document.querySelectorAll('.cv-range input[type=range]'))",
    ".every(function(x){return getComputedStyle(x)",
    ".getPropertyValue('--cv-range-fill').trim() !== '';})"
  )))
})

test_that("More settings becomes a full-screen settings page on narrow viewports", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_more_responsive")
  on.exit(app$stop(), add = TRUE)

  app$run_js(cv_bundle_js())
  app$wait_for_js(
    "document.getElementById('cv-more-btn') !== null",
    timeout = 15000
  )
  app$run_js("document.getElementById('cv-more-btn').click();")
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-more').classList.contains('is-open') && ",
      "getComputedStyle(document.getElementById('cv-more')).transform === 'none'"
    ),
    timeout = 5000
  )

  expect_equal(
    app$get_js("document.activeElement && document.activeElement.id"),
    "cv-more-close"
  )
  expect_equal(
    app$get_js(
      "document.getElementById('cv-more').getAttribute('aria-hidden')"
    ),
    "false"
  )
  app$run_js(paste0(
    "(function(){var x=document.getElementById('cv-opacity');",
    "x.value='0.35';x.dispatchEvent(new Event('input',{bubbles:true}));})()"
  ))

  app$run_js(paste0(
    "document.dispatchEvent(new KeyboardEvent('keydown',",
    "{key:'Escape',bubbles:true}));"
  ))
  app$wait_for_js(
    "document.getElementById('cv-more').getAttribute('aria-hidden') === 'true'",
    timeout = 5000
  )
  expect_equal(
    app$get_js("document.activeElement && document.activeElement.id"),
    "cv-more-btn"
  )
  app$run_js("document.getElementById('cv-more-btn').click();")
  app$wait_for_js(
    "document.getElementById('cv-more').classList.contains('is-open')",
    timeout = 5000
  )
  expect_equal(
    app$get_js("document.getElementById('cv-opacity').value"),
    "0.35"
  )

  app$set_window_size(width = 768, height = 900)
  expect_equal(
    app$get_js("document.getElementById('cv-more').getAttribute('aria-modal')"),
    "true"
  )
  expect_true(app$get_js(
    "getComputedStyle(document.getElementById('cv-more-close')).display !== 'none'"
  ))
  expect_true(app$get_js(paste0(
    "(function(){var r=document.getElementById('cv-more').getBoundingClientRect();",
    "return Math.abs(r.left) <= 1 && Math.abs(r.top) <= 1 && ",
    "Math.abs(r.width-innerWidth) <= 1 && Math.abs(r.height-innerHeight) <= 1 && ",
    "getComputedStyle(document.querySelector('#cv-more .cv-more-clip')).overflowY === 'auto';})()"
  )))

  app$set_window_size(width = 390, height = 844)
  expect_true(app$get_js(paste0(
    "(function(){var r=document.getElementById('cv-more').getBoundingClientRect();",
    "return Math.abs(r.width-innerWidth) <= 1 && Math.abs(r.height-innerHeight) <= 1 && ",
    "document.documentElement.scrollWidth <= innerWidth;})()"
  )))
})


## A bundle can be re-sent on returning to the tab without changing the data
## set. Clearing the per-image alignment on every push meant the user's work
## survived only until they looked away -- the state was keyed on "a message
## arrived" rather than on which data set it described.
test_that("re-sending the same data set keeps the image adjustments", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_img_repush")

  png1 <- paste0(
    "data:image/png;base64,",
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8",
    "z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  )
  bundle <- function(id) {
    cv_bundle_js(paste0(
      "{ dataset_id: '",
      id,
      "',\n",
      "  spaces: [{ id: 'umap', label: 'umap', x: blob(0), y: blob(0) },\n",
      "  { id: 'spatial', label: 'A (spatial)', x: blob(0), y: blob(0),\n",
      "    images: [{ id: 'one', label: 'One', uri: '",
      png1,
      "',\n",
      "      coord_span: [400, 400],\n",
      "      preset: { scaleX: 1, scaleY: 1, opacity: 0.6 } }] }] }"
    ))
  }
  app$run_js(bundle("ds-A"))
  app$wait_for_js(
    "document.getElementById('cv-img-pick').options.length === 2",
    timeout = 15000
  )
  app$run_js(
    paste0(
      "(function () {\n",
      "  var host = document.createElement('div');\n",
      "  host.innerHTML =\n",
      "    '<input type=\"range\" id=\"cv-img-scalex\" min=\"0.3\" max=\"3\"' +\n",
      "    ' step=\"0.02\" value=\"1\">' +\n",
      "    '<input type=\"range\" id=\"cv-img-scaley\" min=\"0.3\" max=\"3\"' +\n",
      "    ' step=\"0.02\" value=\"1\">' +\n",
      "    '<input type=\"checkbox\" id=\"cv-img-lock\" checked>';\n",
      "  document.body.appendChild(host);\n",
      "})();"
    )
  )
  scalex <- "parseFloat(document.getElementById('cv-img-scalex').value)"
  app$run_js(
    paste0(
      "(function () { var x = document.getElementById('cv-img-scalex');\n",
      "  x.value = '2.5';\n",
      "  x.dispatchEvent(new Event('input', { bubbles: true })); })();"
    )
  )
  app$wait_for_idle(timeout = 5000)
  expect_equal(app$get_js(scalex), 2.5)

  ## The SAME data set arriving again -- what leaving and returning to the tab
  ## produces -- must not undo it.
  app$run_js(bundle("ds-A"))
  app$wait_for_idle(timeout = 10000)
  expect_equal(app$get_js(scalex), 2.5)

  ## A different data set must, though: the ids the alignments are stored under
  ## belong to the object that produced them.
  app$run_js(bundle("ds-B"))
  app$wait_for_idle(timeout = 10000)
  expect_equal(app$get_js(scalex), 1)

  app$stop()
})

test_that("Builder point appearance is per-space until the shared override", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_point_appearance_repush")
  on.exit(app$stop(), add = TRUE)

  app$run_js(paste0(
    "(function(){var p=CanvasRenderingContext2D.prototype;",
    "window.__cvPointPaint={radii:{},alphas:{}};",
    "if(!p.__cvOriginalArc){p.__cvOriginalArc=p.arc;",
    "p.arc=function(x,y,r){var id=this.canvas&&this.canvas.id;",
    "if(id&&id.indexOf('cv-cv-')===0){",
    "(__cvPointPaint.radii[id]||(__cvPointPaint.radii[id]=[])).push(r);}",
    "return p.__cvOriginalArc.apply(this,arguments);};}",
    "if(!p.__cvOriginalFill){p.__cvOriginalFill=p.fill;",
    "p.fill=function(){var id=this.canvas&&this.canvas.id;",
    "if(id&&id.indexOf('cv-cv-')===0){",
    "(__cvPointPaint.alphas[id]||(__cvPointPaint.alphas[id]=[]))",
    ".push(this.globalAlpha);}",
    "return p.__cvOriginalFill.apply(this,arguments);};}})();"
  ))
  bundle <- function(id, trekker_size = 9) {
    cv_bundle_js(
      paste0(
        "{dataset_id:'",
        id,
        "',default_point_size:5,",
        "default_point_opacity:0.8,spaces:[",
        "{id:'umap',label:'umap',x:blob(0),y:blob(0)},",
        "{id:'trekker',label:'Trekker',x:blob(1),y:blob(1),",
        "builder_point_size:",
        trekker_size,
        ",builder_point_opacity:0.65}]}"
      ),
      n = 12
    )
  }
  redraw <- paste0(
    "window.__cvPointPaint={radii:{},alphas:{}};",
    cv_refresh_color_js
  )
  has_value <- function(kind, canvas, value) {
    app$get_js(paste0(
      "(__cvPointPaint.",
      kind,
      "['",
      canvas,
      "']||[])",
      ".some(function(x){return Math.abs(x-",
      value,
      ")<0.001;})"
    ))
  }

  app$run_js(bundle("style-A"))
  app$wait_for_js(
    "document.querySelectorAll('.cv-pane:not(.cv-hidden)').length===2",
    timeout = 15000
  )
  app$run_js(redraw)
  app$wait_for_idle(timeout = 5000)
  expect_true(has_value("radii", "cv-cv-a", 5))
  expect_true(has_value("radii", "cv-cv-b", 9))
  expect_true(has_value("alphas", "cv-cv-a", 0.8))
  expect_true(has_value("alphas", "cv-cv-b", 0.65))

  app$run_js(paste0(
    "(function(){var s=document.getElementById('cv-ps');s.value='12';",
    "s.dispatchEvent(new Event('input',{bubbles:true}));",
    "var o=document.getElementById('cv-opacity');o.value='0.4';",
    "o.dispatchEvent(new Event('input',{bubbles:true}));})();"
  ))
  app$run_js(bundle("style-A"))
  app$wait_for_idle(timeout = 10000)
  expect_equal(
    app$get_js("Number(document.getElementById('cv-ps').value)"),
    12
  )
  expect_equal(
    app$get_js("Number(document.getElementById('cv-opacity').value)"),
    0.4
  )
  app$run_js(redraw)
  app$wait_for_idle(timeout = 5000)
  expect_true(has_value("radii", "cv-cv-a", 12))
  expect_true(has_value("radii", "cv-cv-b", 12))
  expect_true(has_value("alphas", "cv-cv-a", 0.4))
  expect_true(has_value("alphas", "cv-cv-b", 0.4))

  app$run_js(bundle("style-B"))
  app$wait_for_idle(timeout = 10000)
  expect_equal(
    app$get_js("Number(document.getElementById('cv-ps').value)"),
    5
  )
  expect_equal(
    app$get_js("Number(document.getElementById('cv-opacity').value)"),
    0.8
  )
  app$run_js(redraw)
  app$wait_for_idle(timeout = 5000)
  expect_true(has_value("radii", "cv-cv-a", 5))
  expect_true(has_value("radii", "cv-cv-b", 9))
  expect_true(has_value("alphas", "cv-cv-a", 0.8))
  expect_true(has_value("alphas", "cv-cv-b", 0.65))

  ## Zero is a valid Builder value: it intentionally hides the points while
  ## leaving an aligned tissue image visible. Do not treat it as "unset".
  app$run_js(bundle("style-C", trekker_size = 0))
  app$wait_for_idle(timeout = 10000)
  app$run_js(redraw)
  app$wait_for_idle(timeout = 5000)
  expect_true(has_value("radii", "cv-cv-a", 5))
  expect_true(has_value("radii", "cv-cv-b", 0))

  app$stop()
})

## The line above the panels names the spaces on screen. Switching spatial
## section left it naming the section that had just been left, so the header and
## the panel title disagreed about what was being shown.
test_that("the summary line follows the spatial section", {
  local_app_support(inst_dir)
  app <- cv_app("cv_browser_meta_section")

  app$run_js(cv_bundle_js(
    paste0(
      "{ spaces: [{ id: 'umap', label: 'umap', x: blob(0), y: blob(0) },\n",
      "  { id: 'spatial', label: 'A (spatial)', x: blob(0), y: blob(0),\n",
      "    samples: [\n",
      "      { name: 'A', label: 'A (spatial)', x: blob(0), y: blob(0) },\n",
      "      { name: 'B', label: 'B (spatial)', x: blob(0), y: blob(0) }] }] }"
    )
  ))
  app$wait_for_js(
    "document.getElementById('cv-pick-spatial') !== null",
    timeout = 15000
  )
  expect_match(
    app$get_js("document.getElementById('cv-meta').textContent"),
    "A (spatial)",
    fixed = TRUE
  )

  app$run_js(paste0(
    "(function () { var s = document.getElementById('cv-pick-spatial');\n",
    "  if (s.selectize) s.selectize.setValue(['B']); else {",
    "s.value = 'B'; s.dispatchEvent(new Event('change'));} })();"
  ))
  app$wait_for_idle(timeout = 10000)
  meta <- app$get_js("document.getElementById('cv-meta').textContent")
  expect_match(meta, "B (spatial)", fixed = TRUE)
  expect_false(grepl("A (spatial)", meta, fixed = TRUE))
  ## ... and it agrees with the panel that is showing it.
  expect_match(
    app$get_js("document.getElementById('cv-title-b').textContent"),
    "B (spatial)",
    fixed = TRUE
  )

  app$stop()
})

test_that("background state keys namespace FOV and direct modalities", {
  js <- paste(
    readLines(
      file.path(inst_dir, "viewer", "www", "coordviews.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(js, "function backgroundStateKey\\(sp\\)")
  expect_match(js, "return 'fov:' \\+ \\(sp.id \\|\\| spatialName\\(sp\\)\\)")
  expect_match(js, "return 'space:' \\+ \\(sp.id \\|\\| 'unknown'\\)")
})

test_that("same-dataset refresh preserves percentage and group filters", {
  js <- paste(
    readLines(
      file.path(inst_dir, "viewer", "www", "coordviews.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    js,
    "if \\(dataChanged\\) \\{[\\s\\S]{0,1200}rebuildPctMask\\(\\)",
    perl = TRUE
  )
  expect_no_match(
    js,
    "pctMask = null; groupFilter = \\{\\};[[:space:]]*rebuildPctMask\\(\\)"
  )
})
