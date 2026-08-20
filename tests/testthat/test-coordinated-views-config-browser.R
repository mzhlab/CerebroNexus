# test-coordinated-views-config-browser.R — real-browser configuration state.

library(shinytest2)

config_browser_inst <- system.file(package = "CerebroNexus")
if (
  !nzchar(config_browser_inst) ||
    !file.exists(file.path(config_browser_inst, "app.R"))
) {
  config_browser_inst <- testthat::test_path("../../inst")
}

config_browser_app <- function() {
  app <- AppDriver$new(
    config_browser_inst,
    name = "cv_config_browser",
    height = 900,
    width = 1440
  )
  app$wait_for_idle(timeout = 30000)
  app$wait_for_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]') !== null",
    timeout = 30000
  )
  app$run_js(paste0(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]')",
    ".click();"
  ))
  app$wait_for_idle(timeout = 20000)
  app
}

config_browser_bundle_js <- function() {
  paste0(
    "(function(){",
    "var cells=['c0','c1','c2','c3','c4','c5','c6','c7','c8'];",
    "var x=[-2,-1,0,1,2,-2,0,2,0],y=[-2,-1,0,1,2,2,1,-2,-1];",
    "var z=[-1,-.5,0,.5,1,-.7,.2,.8,-.2];",
    "var values=[0,1,2,0,1,2,0,1,2];",
    "var b={dataset_id:'adapter-v1',",
    "dataset_fingerprint:'md5-cell-set-v1:0123456789abcdef0123456789abcdef',",
    "cells:cells,n:cells.length,groups:{cluster:{values:values,",
    "levels:['a','b','c'],colors:['#636EFA','#EF553B','#00CC96']}},",
    "cat_extra:{},cat_skipped:{},fields:{},default_group:'cluster',",
    "projections:{umap:{x:x,y:y,ndim:2},pca:{x:x,y:y,z:z,ndim:3}},",
    "default_projection:'umap',spaces:[",
    "{id:'umap',label:'umap (expression)',x:x,y:y},",
    "{id:'spatial',label:'section-a (spatial)',x:x,y:y,samples:[",
    "{name:'section-a',label:'section-a (spatial)',x:x,y:y,images:[",
    "{id:'he',label:'H&E',uri:'data:image/png;base64,iVBORw0KGgo=',",
    "bounds:{xmin:-2,xmax:2,ymin:-2,ymax:2},preset:{}}]}]},",
    "{id:'trekker',label:'Physical (Trekker)',x:x,y:y}],clone:null,",
    "trekker:{conf:[.1,.2,.3,.4,.5,.6,.7,.8,.9],",
    "evidence:[0,1,0,1,0,1,0,1,0]}};",
    "Shiny.shinyapp.dispatchMessage(JSON.stringify({custom:{coordviews_data:b}}));",
    "})();"
  )
}

test_that("the browser adapter round-trips a brushed cohort transactionally", {
  local_app_support(config_browser_inst)
  app <- config_browser_app()
  on.exit(app$stop(), add = TRUE)

  app$run_js(config_browser_bundle_js())
  app$wait_for_js(
    "document.getElementById('cv-meta').textContent.indexOf('9 cells') >= 0",
    timeout = 15000
  )
  app$wait_for_js(
    "window.cerebroLinkedViewsState && window.cerebroLinkedViewsState.ready()",
    timeout = 10000
  )
  app$wait_for_js(
    "document.getElementById('cv-config-open') !== null",
    timeout = 10000
  )
  expect_false(app$get_js("document.getElementById('cv-config-open').disabled"))
  expect_match(
    app$get_js("document.getElementById('cv-config-open').title"),
    "Save, open, import, export, or share"
  )
  app$run_js("document.getElementById('cv-config-open').click();")
  app$wait_for_js("document.getElementById('cv-config-dialog').open")
  expect_true(app$get_js(
    "document.getElementById('cv-config-download').disabled"
  ))
  expect_true(app$get_js("document.getElementById('cv-config-copy').disabled"))
  app$run_js("document.getElementById('cv-config-close').click();")

  app$run_js(paste0(
    "(function(){",
    "document.querySelector('.cv-tbtn[data-act=\"box\"]').click();",
    "var cv=document.getElementById('cv-cv-a'),r=cv.getBoundingClientRect();",
    "cv.dispatchEvent(new MouseEvent('mousedown',{clientX:r.left+8,",
    "clientY:r.top+8,bubbles:true}));",
    "cv.dispatchEvent(new MouseEvent('mousemove',{clientX:r.right-8,",
    "clientY:r.bottom-8,bubbles:true}));",
    "window.dispatchEvent(new MouseEvent('mouseup',{clientX:r.right-8,",
    "clientY:r.bottom-8,bubbles:true}));",
    "})();"
  ))
  app$wait_for_js(
    "window.cerebroLinkedViewsState.summary().selectedCells > 0",
    timeout = 10000
  )
  app$wait_for_js(
    "!document.getElementById('cv-config-open').disabled",
    timeout = 10000
  )
  app$run_js(paste0(
    "document.getElementById('cv-config-open').focus();",
    "document.getElementById('cv-config-open').click();"
  ))
  app$wait_for_js("document.getElementById('cv-config-dialog').open")
  expect_true(app$get_js(paste0(
    "document.getElementById('cv-config-dialog').contains(",
    "document.activeElement)"
  )))
  expect_identical(
    app$get_js(
      "document.getElementById('cv-config-status').getAttribute('aria-live')"
    ),
    "polite"
  )
  expect_false(app$get_js("document.getElementById('cv-config-share').hidden"))
  app$wait_for_js(
    "document.getElementById('cv-share-create').disabled === false",
    timeout = 10000
  )
  app$run_js("document.getElementById('cv-share-create').click();")
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-config-status').textContent",
      ".indexOf('Share link ready.')>=0"
    ),
    timeout = 20000
  )
  expect_no_match(
    app$get_js("document.getElementById('cv-config-status').textContent"),
    "Administrator access is required"
  )
  expect_true(app$get_js(
    "document.querySelector('#cv-share-list button') !== null"
  ))
  actions <- app$get_js(paste0(
    "(function(){",
    "var ids=['cv-config-download','cv-config-copy'];",
    "var nodes=ids.map(function(id){return document.getElementById(id);});",
    "nodes.push(document.querySelector('.cv-config-upload .btn-file'));",
    "var heights=nodes.map(function(node){return node.getBoundingClientRect().height;});",
    "var widths=nodes.map(function(node){return node.getBoundingClientRect().width;});",
    "var styles=nodes.map(function(node){var s=getComputedStyle(node);return {",
    "family:s.fontFamily,size:s.fontSize,weight:s.fontWeight};});",
    "var primary=getComputedStyle(nodes[0]);",
    "var launcher=getComputedStyle(document.getElementById('cv-config-open'));",
    "return {heights:heights,widths:widths,styles:styles,icon:!!document.querySelector(",
    "'.cv-config-upload [class*=\"fa-folder-open\"]'),",
    "background:primary.backgroundColor,foreground:primary.color,",
    "launcherBackground:launcher.backgroundColor,",
    "launcherBorder:launcher.borderColor,launcherColor:launcher.color};",
    "})()"
  ))
  expect_lte(max(unlist(actions$heights)) - min(unlist(actions$heights)), 1)
  expect_lte(max(unlist(actions$widths)) - min(unlist(actions$widths)), 1)
  expect_true(actions$icon)
  expect_identical(actions$styles[[3L]], actions$styles[[2L]])
  expect_identical(actions$background, "rgb(255, 244, 236)")
  expect_identical(actions$foreground, "rgb(28, 28, 30)")
  expect_identical(actions$launcherBackground, "rgb(255, 244, 236)")
  expect_identical(actions$launcherBorder, "rgb(255, 178, 122)")
  expect_identical(actions$launcherColor, "rgb(200, 90, 14)")
  app$run_js(paste0(
    "window.__cvStatusBefore=document.getElementById('cv-config-status').textContent;",
    "Shiny.shinyapp.dispatchMessage(JSON.stringify({custom:{",
    "coordviews_config_result:{nonce:'stale',action:'apply',ok:false,",
    "message:'must be ignored'}}}));"
  ))
  expect_no_match(
    app$get_js("document.getElementById('cv-config-status').textContent"),
    "must be ignored"
  )
  app$run_js(
    "document.dispatchEvent(new KeyboardEvent('keydown',{key:'Escape',bubbles:true}));"
  )
  app$wait_for_js("!document.getElementById('cv-config-dialog').open")
  expect_true(app$get_js(
    "document.activeElement === document.getElementById('cv-config-open')"
  ))

  app$run_js("window.__cvSaved = window.cerebroLinkedViewsState.capture();")
  selected <- unlist(app$get_js("window.__cvSaved.selection.cells"))
  saved_geometry <- app$get_js(
    "JSON.stringify(window.__cvSaved.selection.geometry)"
  )
  expect_gt(length(selected), 0L)
  expect_identical(
    app$get_js("window.__cvSaved.selection.geometry.mode"),
    "box"
  )
  expect_length(
    app$get_js("window.__cvSaved.selection.geometry.polygon"),
    4L
  )
  app$run_js("window.dispatchEvent(new Event('resize'));")
  app$wait_for_js("window.cerebroLinkedViewsState.ready()", timeout = 10000)
  expect_identical(
    app$get_js(paste0(
      "JSON.stringify(window.cerebroLinkedViewsState.capture()",
      ".selection.geometry)"
    )),
    saved_geometry
  )
  expect_identical(
    app$get_js("window.__cvSaved.dataset.cell_fingerprint"),
    "md5-cell-set-v1:0123456789abcdef0123456789abcdef"
  )

  app$run_js("document.getElementById('cv-clear').click();")
  app$wait_for_js(
    "window.cerebroLinkedViewsState.summary().selectedCells === 0",
    timeout = 10000
  )
  app$wait_for_js("!document.getElementById('cv-config-open').disabled")
  app$run_js("window.cerebroLinkedViewsState.apply(window.__cvSaved);")
  app$wait_for_js(
    paste0(
      "window.cerebroLinkedViewsState.summary().selectedCells === ",
      length(selected)
    ),
    timeout = 10000
  )
  expect_equal(
    unlist(app$get_value(input = "coordviews_selection")),
    selected
  )
  expect_identical(
    app$get_js(paste0(
      "JSON.stringify(window.cerebroLinkedViewsState.capture()",
      ".selection.geometry)"
    )),
    saved_geometry
  )

  app$run_js(paste0(
    "window.__cvBefore=JSON.stringify(window.cerebroLinkedViewsState.summary());",
    "window.__cvBad=JSON.parse(JSON.stringify(window.__cvSaved));",
    "window.__cvBad.dataset.cell_fingerprint='md5-cell-set-v1:",
    "00000000000000000000000000000000';",
    "try{window.cerebroLinkedViewsState.apply(window.__cvBad);}",
    "catch(error){window.__cvApplyError=error.message;}"
  ))
  expect_match(app$get_js("window.__cvApplyError"), "cell population")
  expect_identical(
    app$get_js("JSON.stringify(window.cerebroLinkedViewsState.summary())"),
    app$get_js("window.__cvBefore")
  )

  app$run_js(paste0(
    "window.__cvBadGeometry=JSON.parse(JSON.stringify(window.__cvSaved));",
    "window.__cvBadGeometry.selection.geometry.space='projection::pca';",
    "window.__cvBadGeometry.view.projections.push('pca');",
    "window.__cvBadGeometry.view.lenses.push({space:'projection::pca',",
    "viewport:{cx:.5,cy:.5,span:1},rotation:{rx:0,ry:0}});",
    "window.__cvBeforeGeometry=JSON.stringify(window.cerebroLinkedViewsState.summary());",
    "try{window.cerebroLinkedViewsState.apply(window.__cvBadGeometry);}",
    "catch(error){window.__cvGeometryError=error.message;}"
  ))
  expect_match(app$get_js("window.__cvGeometryError"), "two-dimensional")
  expect_identical(
    app$get_js("JSON.stringify(window.cerebroLinkedViewsState.summary())"),
    app$get_js("window.__cvBeforeGeometry")
  )

  app$run_js(paste0(
    "window.__cvContext=JSON.parse(JSON.stringify(window.__cvSaved));",
    "window.__cvContext.selection.cells=['c0','c1'];",
    "window.__cvContext.view.projections=['umap','pca'];",
    "window.__cvContext.view.filters={cluster:['a','b']};",
    "window.__cvContext.view.hidden_levels=[{group:'cluster',levels:['c']}];",
    "window.__cvContext.view.display={percentage_cells:100,point_size:2.2,",
    "point_opacity:.35,group_labels:false,selection_mode:'box',",
    "clone_layout:'stack'};",
    "window.__cvContext.view.focus_space='projection::pca';",
    "window.__cvContext.view.lenses=window.__cvContext.view.lenses.filter(",
    "function(x){return x.space!=='projection::pca';});",
    "window.__cvContext.view.lenses[0].viewport={cx:.45,cy:.55,span:.7};",
    "window.__cvContext.view.lenses.push({space:'projection::pca',",
    "viewport:{cx:.5,cy:.5,span:1},rotation:{rx:.2,ry:.3}});",
    "window.__cvContext.view.spatial_backgrounds[0].mode='image';",
    "window.__cvContext.view.spatial_backgrounds[0].image_id='he';",
    "window.__cvContext.view.spatial_backgrounds[0].opacity=.4;",
    "window.__cvContext.view.spatial_backgrounds[0].alignment.scale_x=1.2;",
    "window.__cvContext.view.spatial_backgrounds[0].alignment.scale_y=.8;",
    "window.__cvContext.view.spatial_backgrounds[0].alignment.lock_aspect=false;",
    "window.__cvContext.view.trekker={dissolve_percentage:25,evidence:false,",
    "niche_radius:300};",
    "window.cerebroLinkedViewsState.apply(window.__cvContext);",
    "window.__cvContextAfter=window.cerebroLinkedViewsState.capture();"
  ))
  app$wait_for_js(
    "window.cerebroLinkedViewsState.summary().selectedCells === 2",
    timeout = 10000
  )
  expect_true(app$get_js(paste0(
    "window.__cvContextAfter.view.projections.join(',')==='umap,pca'&&",
    "window.__cvContextAfter.view.focus_space==='projection::pca'&&",
    "window.__cvContextAfter.view.display.point_size===2.2&&",
    "window.__cvContextAfter.view.display.point_opacity===.35&&",
    "window.__cvContextAfter.view.display.group_labels===false&&",
    "window.__cvContextAfter.view.filters.cluster.join(',')==='a,b'&&",
    "window.__cvContextAfter.view.hidden_levels[0].levels[0]==='c'&&",
    "window.__cvContextAfter.view.trekker.dissolve_percentage===25&&",
    "window.__cvContextAfter.view.trekker.evidence===false&&",
    "window.__cvContextAfter.view.trekker.niche_radius===300"
  )))
  expect_true(app$get_js(paste0(
    "(function(){var p=window.__cvContextAfter.view.lenses.filter(",
    "function(x){return x.space==='projection::pca';})[0];",
    "var b=window.__cvContextAfter.view.spatial_backgrounds[0];",
    "return p&&p.rotation&&p.rotation.rx===.2&&p.rotation.ry===.3&&",
    "b.mode==='image'&&b.image_id==='he'&&b.opacity===.4&&",
    "b.alignment.scale_x===1.2&&b.alignment.scale_y===.8&&",
    "b.alignment.lock_aspect===false;})()"
  )))

  app$run_js(paste0(
    "window.__cvRgbEvents=0;",
    "['coordviews_gene_r','coordviews_gene_g','coordviews_gene_b']",
    ".forEach(function(id){window.jQuery('#'+id).on('change.cv-config-test',",
    "function(){window.__cvRgbEvents+=1;});});",
    "window.__cvRgb=JSON.parse(JSON.stringify(window.__cvContext));",
    "window.__cvRgb.view.colour.mode='__rgb__';",
    "window.__cvRgb.view.colour.gene=null;",
    "window.__cvRgb.view.colour.rgb_genes=['G1','G2','G3'];",
    "window.__cvRgb.view.hidden_levels=[];",
    "window.cerebroLinkedViewsState.apply(window.__cvRgb);"
  ))
  app$wait_for_js("window.__cvRgbEvents === 3", timeout = 10000)
  expect_true(app$get_js(paste0(
    "['coordviews_gene_r','coordviews_gene_g','coordviews_gene_b']",
    ".map(function(id){return document.getElementById(id).selectize.getValue();})",
    ".join(',')==='G1,G2,G3'"
  )))
  app$run_js(paste0(
    "window.__cvRgbEvents=0;",
    "window.__cvRgbData={mode:'__rgb__',genes:['G1','G2','G3'],",
    "r:[0,1,2,3,4,5,6,7,8],g:[8,7,6,5,4,3,2,1,0],",
    "b:[1,1,1,1,1,1,1,1,1]};",
    "window.cerebroLinkedViewsState.apply(window.__cvRgb,window.__cvRgbData);",
    "window.__cvRgbPrefetched=window.cerebroLinkedViewsState.capture();"
  ))
  expect_equal(app$get_js("window.__cvRgbEvents"), 0)
  expect_identical(
    unlist(app$get_js("window.__cvRgbPrefetched.view.colour.rgb_genes")),
    c("G1", "G2", "G3")
  )
  app$run_js("window.cerebroLinkedViewsState.apply(window.__cvContext);")

  app$run_js(paste0(
    "window.__cvCapabilityBefore=window.cerebroLinkedViewsState.capture();",
    "window.__cvCapabilityBad=JSON.parse(JSON.stringify(",
    "window.__cvCapabilityBefore));",
    "window.__cvCapabilityBad.view.projections.push('missing-projection');",
    "try{window.cerebroLinkedViewsState.apply(window.__cvCapabilityBad);}",
    "catch(error){window.__cvCapabilityError=error.message;}",
    "window.__cvCapabilityAfter=window.cerebroLinkedViewsState.capture();",
    "window.__cvCapabilityAfter.created_at=window.__cvCapabilityBefore.created_at;"
  ))
  expect_match(app$get_js("window.__cvCapabilityError"), "projection")
  expect_identical(
    app$get_js("JSON.stringify(window.__cvCapabilityAfter)"),
    app$get_js("JSON.stringify(window.__cvCapabilityBefore)")
  )

  app$run_js(paste0(
    "window.__cvCloneBefore=window.cerebroLinkedViewsState.capture();",
    "window.__cvCloneBad=JSON.parse(JSON.stringify(window.__cvCloneBefore));",
    "window.__cvCloneBad.view.display.clone_layout='bands';",
    "try{window.cerebroLinkedViewsState.apply(window.__cvCloneBad);}",
    "catch(error){window.__cvCloneError=error.message;}",
    "window.__cvCloneAfter=window.cerebroLinkedViewsState.capture();",
    "window.__cvCloneAfter.created_at=window.__cvCloneBefore.created_at;"
  ))
  expect_match(app$get_js("window.__cvCloneError"), "clone")
  expect_identical(
    app$get_js("JSON.stringify(window.__cvCloneAfter)"),
    app$get_js("JSON.stringify(window.__cvCloneBefore)")
  )

  app$run_js(paste0(
    "window.__cvMissingLens=JSON.parse(JSON.stringify(window.__cvCloneBefore));",
    "window.__cvMissingLens.view.lenses.pop();",
    "try{window.cerebroLinkedViewsState.apply(window.__cvMissingLens);}",
    "catch(error){window.__cvMissingLensError=error.message;}"
  ))
  expect_match(app$get_js("window.__cvMissingLensError"), "lens")

  app$run_js(paste0(
    "window.__cvMissingBackground=JSON.parse(JSON.stringify(",
    "window.__cvCloneBefore));",
    "window.__cvMissingBackground.view.spatial_backgrounds=[];",
    "try{window.cerebroLinkedViewsState.apply(window.__cvMissingBackground);}",
    "catch(error){window.__cvMissingBackgroundError=error.message;}"
  ))
  expect_match(app$get_js("window.__cvMissingBackgroundError"), "background")
})

test_that("copy uses the real server validation boundary", {
  local_app_support(config_browser_inst)
  app <- config_browser_app()
  on.exit(app$stop(), add = TRUE)

  app$wait_for_js(
    "window.cerebroLinkedViewsState && window.cerebroLinkedViewsState.ready()",
    timeout = 20000
  )
  app$wait_for_js(
    "document.getElementById('cv-cv-a') !== null",
    timeout = 10000
  )
  app$run_js(paste0(
    "(function(){",
    "document.querySelector('.cv-tbtn[data-act=\"box\"]').click();",
    "var cv=document.getElementById('cv-cv-a'),r=cv.getBoundingClientRect();",
    "cv.dispatchEvent(new MouseEvent('mousedown',{clientX:r.left+8,",
    "clientY:r.top+8,bubbles:true}));",
    "cv.dispatchEvent(new MouseEvent('mousemove',{clientX:r.right-8,",
    "clientY:r.bottom-8,bubbles:true}));",
    "window.dispatchEvent(new MouseEvent('mouseup',{clientX:r.right-8,",
    "clientY:r.bottom-8,bubbles:true}));",
    "})();"
  ))
  app$wait_for_js(
    "window.cerebroLinkedViewsState.summary().selectedCells > 0",
    timeout = 10000
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-config-open') && ",
      "!document.getElementById('cv-config-open').disabled"
    ),
    timeout = 10000
  )
  app$run_js(paste0(
    "document.getElementById('cv-config-open').focus();",
    "document.getElementById('cv-config-open').click();",
    "document.getElementById('cv-config-copy').click();"
  ))
  app$wait_for_js(
    paste0(
      "(function(){var text=document.getElementById('cv-config-status').textContent;",
      "return text.indexOf('Preparing')<0&&",
      "(text.indexOf('Copied')>=0||text.indexOf('Clipboard access was blocked')>=0);",
      "})()"
    ),
    timeout = 20000
  )
  expect_no_match(
    app$get_js("document.getElementById('cv-config-status').textContent"),
    "could not be opened|different cell population"
  )
  app$run_js(paste0(
    "window.__cvWorkingCreateObjectURL=URL.createObjectURL;",
    "URL.createObjectURL=function(){throw new Error('forced object URL failure');};",
    "document.getElementById('cv-config-download').click();"
  ))
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-config-status').textContent",
      ".indexOf('Preparing')<0&&",
      "!document.getElementById('cv-config-download').disabled"
    ),
    timeout = 5000
  )
  expect_match(
    app$get_js("document.getElementById('cv-config-status').textContent"),
    "could not start"
  )
  app$run_js("URL.createObjectURL=window.__cvWorkingCreateObjectURL;")
  app$run_js(paste0(
    "window.__cvDownloadedJson=null;window.__cvDownloadName=null;",
    "window.__cvCreateObjectURL=URL.createObjectURL;",
    "window.__cvAnchorClick=HTMLAnchorElement.prototype.click;",
    "URL.createObjectURL=function(blob){",
    "blob.text().then(function(text){window.__cvDownloadedJson=text;});",
    "return 'blob:linked-views-test';};",
    "HTMLAnchorElement.prototype.click=function(){",
    "if(this.download){window.__cvDownloadName=this.download;return;}",
    "return window.__cvAnchorClick.call(this);};",
    "document.getElementById('cv-config-download').click();"
  ))
  app$wait_for_js(
    paste0(
      "window.__cvDownloadedJson!==null&&window.__cvDownloadName!==null&&",
      "document.getElementById('cv-config-status').textContent",
      ".indexOf('Preparing')<0"
    ),
    timeout = 5000
  )
  downloaded_json_text <- app$get_js("window.__cvDownloadedJson")
  downloaded_from_button <- jsonlite::fromJSON(
    downloaded_json_text,
    simplifyVector = FALSE
  )
  expect_identical(downloaded_from_button$schema, "cerebronexus-linked-view")
  expect_match(
    app$get_js("window.__cvDownloadName"),
    "^linked-views-.*\\.json$"
  )
  app$run_js(paste0(
    "URL.createObjectURL=window.__cvCreateObjectURL;",
    "HTMLAnchorElement.prototype.click=window.__cvAnchorClick;"
  ))
  expect_type(downloaded_from_button$selection$cells, "list")

  original_size <- app$get_js(
    "window.cerebroLinkedViewsState.capture().view.display.point_size"
  )
  config_path <- tempfile("linked-views-", fileext = ".json")
  on.exit(unlink(config_path), add = TRUE)
  writeLines(
    downloaded_json_text,
    config_path,
    useBytes = TRUE
  )
  app$run_js(paste0(
    "(function(){var input=document.getElementById('cv-ps');",
    "input.value='7';input.dispatchEvent(new Event('input',{bubbles:true}));",
    "})()"
  ))
  expect_equal(
    app$get_js(
      "window.cerebroLinkedViewsState.capture().view.display.point_size"
    ),
    7
  )
  invalid_config_path <- tempfile(
    "linked-views-invalid-gene-",
    fileext = ".json"
  )
  on.exit(unlink(invalid_config_path), add = TRUE)
  invalid_config <- jsonlite::fromJSON(
    app$get_js("JSON.stringify(window.cerebroLinkedViewsState.capture())"),
    simplifyVector = FALSE
  )
  invalid_config$view$colour$mode <- "__gene__"
  invalid_config$view$colour$gene <- "__definitely_missing_gene__"
  invalid_config$view$colour$rgb_genes <- list()
  writeLines(
    jsonlite::toJSON(invalid_config, auto_unbox = TRUE, null = "null"),
    invalid_config_path,
    useBytes = TRUE
  )
  app$upload_file(coordviews_config_upload = invalid_config_path)
  app$wait_for_js(
    paste0(
      "(function(){var node=document.getElementById('cv-config-status');",
      "if(!node)return false;",
      "return node.textContent.indexOf('gene that is unavailable')>=0;})()"
    ),
    timeout = 20000
  )
  app$wait_for_js(
    "window.cerebroLinkedViewsState.capture().view.display.point_size===7",
    timeout = 5000
  )
  app$upload_file(coordviews_config_upload = config_path)
  app$wait_for_js(
    paste0(
      "(function(){var node=document.getElementById('cv-config-status');",
      "if(!node||node.textContent.indexOf('Restored ')!==0)return false;",
      "return window.cerebroLinkedViewsState.capture().view.display.point_size===",
      jsonlite::toJSON(original_size, auto_unbox = TRUE),
      ";})()"
    ),
    timeout = 20000
  )
})
