style_contract_path <- function(...) {
  path <- testthat::test_path("..", "..", "inst", ...)
  if (file.exists(path)) {
    return(path)
  }
  path <- system.file(..., package = "CerebroNexus")
  if (!nzchar(path) || !file.exists(path)) {
    stop(
      sprintf("Style contract asset not found: %s", file.path(...)),
      call. = FALSE
    )
  }
  path
}

style_contract_tokens <- function(path) {
  css <- paste(readLines(path, warn = FALSE), collapse = "\n")
  css <- gsub("/\\*[\\s\\S]*?\\*/", "", css, perl = TRUE)
  roots <- regmatches(
    css,
    gregexpr(":root\\s*\\{[^{}]*\\}", css, perl = TRUE)
  )[[1L]]
  if (length(roots) != 1L) {
    stop(
      sprintf(
        "Style contract CSS must contain exactly one :root block; found %d",
        length(roots)
      ),
      call. = FALSE
    )
  }
  root <- roots[[1L]]
  declarations <- regmatches(
    root,
    gregexpr("--[[:alnum:]_-]+\\s*:[^;]+;", root, perl = TRUE)
  )[[1L]]
  if (!length(declarations)) {
    return(stats::setNames(character(), character()))
  }
  names <- trimws(sub(":.*$", "", declarations))
  duplicates <- unique(names[duplicated(names)])
  if (length(duplicates)) {
    stop(
      sprintf(
        "Duplicate token declaration: %s",
        paste(duplicates, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  values <- trimws(sub("^[^:]+:", "", sub(";$", "", declarations)))
  values <- gsub("[[:space:]]+", " ", trimws(values))
  stats::setNames(values, names)
}

style_contract_assert_acyclic_aliases <- function(tokens) {
  if (!length(tokens)) {
    return(invisible(TRUE))
  }

  dependencies <- lapply(unname(tokens), function(value) {
    references <- regmatches(
      value,
      gregexpr("var\\(\\s*--[[:alnum:]_-]+", value, perl = TRUE)
    )[[1L]]
    references <- sub("^var\\(\\s*", "", references, perl = TRUE)
    intersect(unique(references), names(tokens))
  })
  names(dependencies) <- names(tokens)

  state <- stats::setNames(integer(length(tokens)), names(tokens))
  path <- character()
  visit <- function(token) {
    if (state[[token]] == 1L) {
      cycle_start <- match(token, path)
      cycle <- c(path[seq.int(cycle_start, length(path))], token)
      stop(
        sprintf(
          "Custom property alias cycle: %s",
          paste(cycle, collapse = " -> ")
        ),
        call. = FALSE
      )
    }
    if (state[[token]] == 2L) {
      return(invisible(NULL))
    }

    state[[token]] <<- 1L
    path <<- c(path, token)
    for (dependency in dependencies[[token]]) {
      visit(dependency)
    }
    path <<- path[-length(path)]
    state[[token]] <<- 2L
    invisible(NULL)
  }

  for (token in names(tokens)) {
    visit(token)
  }
  invisible(TRUE)
}

style_contract_undefined_custom_properties <- function(css) {
  css <- gsub("/\\*[\\s\\S]*?\\*/", "", css, perl = TRUE)
  declarations <- unique(sub(
    "\\s*:$",
    "",
    trimws(unlist(
      regmatches(
        css,
        gregexpr("--[[:alnum:]_-]+\\s*:", css, perl = TRUE)
      ),
      use.names = FALSE
    ))
  ))
  references <- unique(sub(
    "^var\\(\\s*",
    "",
    unlist(
      regmatches(
        css,
        gregexpr("var\\(\\s*--[[:alnum:]_-]+", css, perl = TRUE)
      ),
      use.names = FALSE
    )
  ))

  setdiff(references, declarations)
}

style_contract_builder_css <- function(
  files = c(
    "builder.base.css",
    "builder.layout.css",
    "builder.components.css",
    "builder.features.css"
  )
) {
  paste(
    vapply(
      files,
      function(file) {
        paste(
          readLines(
            style_contract_path("builder", "www", file),
            warn = FALSE
          ),
          collapse = "\n"
        )
      },
      character(1)
    ),
    collapse = "\n"
  )
}

style_contract_rule_selectors <- function(css) {
  css <- gsub("/\\*[\\s\\S]*?\\*/", "", css, perl = TRUE)
  preludes <- regmatches(
    css,
    gregexpr(
      "(?m)(?:^|(?<=[{}]))\\s*[^{}]+?\\s*(?=\\{)",
      css,
      perl = TRUE
    )
  )[[1L]]
  preludes <- trimws(preludes)
  preludes <- preludes[!startsWith(preludes, "@")]
  selectors <- trimws(unlist(
    strsplit(preludes, ",", fixed = TRUE),
    use.names = FALSE
  ))
  unique(selectors[nzchar(selectors)])
}

style_contract_assert_selector_ownership <- function(stylesheets, owner_map) {
  selectors_by_owner <- lapply(stylesheets, style_contract_rule_selectors)

  for (selector in names(owner_map)) {
    expected_owner <- unname(owner_map[[selector]])
    if (!expected_owner %in% names(stylesheets)) {
      stop(
        sprintf("Unknown stylesheet owner '%s'", expected_owner),
        call. = FALSE
      )
    }
    actual_owners <- names(selectors_by_owner)[vapply(
      selectors_by_owner,
      function(selectors) selector %in% selectors,
      logical(1)
    )]
    if (!expected_owner %in% actual_owners) {
      stop(
        sprintf(
          "Selector '%s' is missing from owner '%s'",
          selector,
          expected_owner
        ),
        call. = FALSE
      )
    }
    unexpected_owners <- setdiff(actual_owners, expected_owner)
    if (length(unexpected_owners)) {
      stop(
        sprintf(
          "Selector '%s' belongs to '%s' but also appears in '%s'",
          selector,
          expected_owner,
          paste(unexpected_owners, collapse = "', '")
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

style_contract_parse_rules <- function(css) {
  css <- gsub("/\\*[\\s\\S]*?\\*/", "", css, perl = TRUE)
  raw_rules <- regmatches(
    css,
    gregexpr("[^{}]+\\{[^{}]*\\}", css, perl = TRUE)
  )[[1L]]
  parsed <- lapply(raw_rules, function(rule) {
    prelude <- trimws(sub("\\{[\\s\\S]*$", "", rule, perl = TRUE))
    if (startsWith(prelude, "@")) {
      return(NULL)
    }
    selectors <- trimws(strsplit(prelude, ",", fixed = TRUE)[[1L]])
    selectors <- selectors[nzchar(selectors)]
    block <- sub("^[^{]*\\{", "", rule, perl = TRUE)
    block <- sub("\\}$", "", block)
    raw_declarations <- trimws(strsplit(block, ";", fixed = TRUE)[[1L]])
    raw_declarations <- raw_declarations[nzchar(raw_declarations)]
    declaration_colons <- regexpr(":", raw_declarations, fixed = TRUE)
    if (any(declaration_colons < 1L)) {
      stop("Style contract found a declaration without a colon", call. = FALSE)
    }
    properties <- tolower(trimws(substr(
      raw_declarations,
      1L,
      declaration_colons - 1L
    )))
    values <- trimws(substr(
      raw_declarations,
      declaration_colons + 1L,
      nchar(raw_declarations)
    ))
    values <- gsub("[[:space:]]+", " ", values)
    values <- gsub("[[:space:]]*!important$", " !important", values)
    list(
      selectors = sort(selectors),
      properties = properties,
      values = values,
      declarations = sort(paste0(properties, ": ", values))
    )
  })
  Filter(Negate(is.null), parsed)
}

style_contract_matching_rule_indexes <- function(rules, selectors) {
  expected <- sort(trimws(selectors))
  which(vapply(
    rules,
    function(rule) identical(rule$selectors, expected),
    logical(1)
  ))
}

style_contract_rule_declarations <- function(css, selectors) {
  rules <- style_contract_parse_rules(css)
  expected <- sort(trimws(selectors))
  matching <- style_contract_matching_rule_indexes(rules, expected)
  if (length(matching) != 1L) {
    stop(
      sprintf(
        "Expected exactly one shared rule for [%s]; found %d",
        paste(expected, collapse = ", "),
        length(matching)
      ),
      call. = FALSE
    )
  }
  rules[[matching[[1L]]]]$declarations
}

style_contract_assert_exclusive_recipe <- function(
  css,
  selectors,
  protected_properties
) {
  rules <- style_contract_parse_rules(css)
  target_selectors <- sort(trimws(selectors))
  protected_properties <- tolower(trimws(protected_properties))
  shared <- style_contract_matching_rule_indexes(rules, target_selectors)
  if (length(shared) != 1L) {
    stop(
      sprintf(
        "Expected exactly one shared rule for [%s]; found %d",
        paste(target_selectors, collapse = ", "),
        length(shared)
      ),
      call. = FALSE
    )
  }

  shared_properties <- rules[[shared[[1L]]]]$properties
  protected_counts <- table(
    shared_properties[shared_properties %in% protected_properties]
  )
  duplicated_protected <- names(protected_counts)[protected_counts > 1L]
  if (length(duplicated_protected)) {
    property <- duplicated_protected[[1L]]
    stop(
      sprintf(
        "Shared recipe declares protected property '%s' %d times",
        property,
        unname(protected_counts[[property]])
      ),
      call. = FALSE
    )
  }

  for (index in setdiff(seq_along(rules), shared)) {
    duplicated_selectors <- intersect(
      rules[[index]]$selectors,
      target_selectors
    )
    duplicated_properties <- intersect(
      rules[[index]]$properties,
      protected_properties
    )
    if (length(duplicated_selectors) && length(duplicated_properties)) {
      stop(
        sprintf(
          paste0(
            "Selector '%s' redeclares protected property '%s' ",
            "outside its shared recipe"
          ),
          duplicated_selectors[[1L]],
          duplicated_properties[[1L]]
        ),
        call. = FALSE
      )
    }
  }
  rules[[shared[[1L]]]]$declarations
}

test_that("exclusive recipes reject split protected-property overrides", {
  button_selectors <- c(
    ".btn-primary",
    ".btn-action",
    ".btn-primary:focus",
    ".btn-action:focus"
  )
  button_css <- paste(
    style_contract_builder_css("builder.components.css"),
    ".btn-primary { background: magenta !important; }",
    sep = "\n"
  )
  expect_length(
    style_contract_rule_declarations(button_css, button_selectors),
    3L
  )
  expect_error(
    style_contract_assert_exclusive_recipe(
      button_css,
      button_selectors,
      c("background", "border-color", "color")
    ),
    "protected property 'background'"
  )

  surface_selectors <- c(
    ".viewer-analysis-result",
    ".viewer-specialized-item"
  )
  surface_css <- paste(
    style_contract_builder_css("builder.features.css"),
    ".viewer-analysis-result { padding: 2rem; }",
    sep = "\n"
  )
  expect_length(
    style_contract_rule_declarations(surface_css, surface_selectors),
    5L
  )
  expect_error(
    style_contract_assert_exclusive_recipe(
      surface_css,
      surface_selectors,
      c("min-width", "padding", "border", "border-radius", "background")
    ),
    "protected property 'padding'"
  )
})

test_that("shared rule lookup reports missing selector sets clearly", {
  split_css <- paste(
    ".btn-primary { background: red; }",
    ".btn-action { background: red; }"
  )

  expect_error(
    style_contract_rule_declarations(
      split_css,
      c(".btn-primary", ".btn-action")
    ),
    paste0(
      "Expected exactly one shared rule for ",
      "[.btn-action, .btn-primary]; found 0"
    ),
    fixed = TRUE
  )
})

test_that("exclusive recipes reject duplicate protected declarations", {
  css <- style_contract_builder_css("builder.components.css")
  needle <- paste(
    ".btn-action:focus {",
    "  border-color: var(--builder-action) !important;",
    "  background: var(--builder-action) !important;",
    sep = "\n"
  )
  replacement <- paste(
    needle,
    "  background: var(--builder-action) !important;",
    sep = "\n"
  )
  mutated_css <- sub(needle, replacement, css, fixed = TRUE)
  expect_false(identical(mutated_css, css))

  expect_error(
    style_contract_assert_exclusive_recipe(
      mutated_css,
      c(
        ".btn-primary",
        ".btn-action",
        ".btn-primary:focus",
        ".btn-action:focus"
      ),
      c("border-color", "background", "color")
    ),
    "protected property 'background' 2 times"
  )
})

test_that("style ownership rejects a full selector duplicated across owners", {
  stylesheets <- list(
    base = ".btn { color: red; }",
    layout = ".actionbar .btn { width: 100%; }",
    components = ".btn { color: blue; }",
    features = ".example-btn { display: block; }",
    legacy = "/* migration shell */"
  )
  owner_map <- c(".btn" = "components")

  expect_match(stylesheets$components, ".btn", fixed = TRUE)
  expect_error(
    style_contract_assert_selector_ownership(stylesheets, owner_map),
    "Selector '.btn' belongs to 'components' but also appears in 'base'",
    fixed = TRUE
  )
  expect_error(
    style_contract_assert_selector_ownership(
      stylesheets,
      c(".missing" = "components")
    ),
    "Selector '.missing' is missing from owner 'components'",
    fixed = TRUE
  )
})

test_that("style contract paths fail clearly for missing assets", {
  expect_error(
    style_contract_path("builder", "www", "missing-style-contract.css"),
    "Style contract asset not found"
  )
})

test_that("style token parsing ignores declarations inside block comments", {
  css_path <- withr::local_tempfile(fileext = ".css")
  writeLines(
    c(
      ":root {",
      "  --visible-token: present;",
      "  /* --hidden-token: absent; */",
      "  --multiline-token: first",
      "    second;",
      "  --base_token: underscored;",
      "  --inline-comment-token: before /* ignored */ after;",
      "}"
    ),
    css_path
  )

  tokens <- style_contract_tokens(css_path)

  expect_true("--visible-token" %in% names(tokens))
  expect_false("--hidden-token" %in% names(tokens))
  expect_identical(unname(tokens["--multiline-token"]), "first second")
  expect_identical(unname(tokens["--base_token"]), "underscored")
  expect_identical(
    unname(tokens["--inline-comment-token"]),
    "before after"
  )
})

test_that("style token parsing rejects a fully commented root", {
  css_path <- withr::local_tempfile(fileext = ".css")
  writeLines(
    c(
      "/*",
      ":root {",
      "  --fake-token: absent;",
      "}",
      "*/"
    ),
    css_path
  )

  expect_error(
    style_contract_tokens(css_path),
    "exactly one :root block; found 0"
  )
})

test_that("style token parsing rejects missing roots clearly", {
  css_path <- withr::local_tempfile(fileext = ".css")
  writeLines(".example { color: red; }", css_path)

  expect_error(
    style_contract_tokens(css_path),
    "exactly one :root block; found 0"
  )
})

test_that("style token parsing accepts a compact empty root", {
  css_path <- withr::local_tempfile(fileext = ".css")
  writeLines(":root {}", css_path)

  expect_identical(
    style_contract_tokens(css_path),
    stats::setNames(character(), character())
  )
})

test_that("style token parsing rejects duplicate declarations", {
  css_path <- withr::local_tempfile(fileext = ".css")
  writeLines(
    c(
      ":root {",
      "  --duplicate-token: first;",
      "  --duplicate-token: second;",
      "}"
    ),
    css_path
  )

  expect_error(
    style_contract_tokens(css_path),
    "Duplicate token declaration: --duplicate-token"
  )
})

test_that("style token parsing rejects multiple canonical roots", {
  css_path <- withr::local_tempfile(fileext = ".css")
  writeLines(
    c(
      ":root { --first-token: first; }",
      ":root { --second-token: second; }"
    ),
    css_path
  )

  expect_error(
    style_contract_tokens(css_path),
    "exactly one :root block; found 2"
  )
})

test_that("style token aliases reject direct and indirect cycles", {
  direct_path <- withr::local_tempfile(fileext = ".css")
  writeLines(
    c(
      ":root {",
      "  --direct: var(--direct);",
      "}"
    ),
    direct_path
  )

  expect_error(
    style_contract_assert_acyclic_aliases(
      style_contract_tokens(direct_path)
    ),
    "Custom property alias cycle: --direct -> --direct"
  )

  indirect_path <- withr::local_tempfile(fileext = ".css")
  writeLines(
    c(
      ":root {",
      "  --first_token: var(--second_token);",
      "  --second_token: var(--third_token);",
      "  --third_token: var(--first_token);",
      "}"
    ),
    indirect_path
  )

  expect_error(
    style_contract_assert_acyclic_aliases(
      style_contract_tokens(indirect_path)
    ),
    paste(
      "Custom property alias cycle:",
      paste(
        "--first_token -> --second_token ->",
        "--third_token -> --first_token"
      )
    )
  )
})

test_that("custom property reference parsing preserves underscores", {
  css <- paste(
    ":root { --base_token: present; }",
    ".example { color: var(--missing_token); }"
  )

  expect_identical(
    style_contract_undefined_custom_properties(css),
    "--missing_token"
  )
})

test_that("Builder token aliases are acyclic", {
  builder <- style_contract_tokens(
    style_contract_path("builder", "www", "builder.tokens.css")
  )

  expect_true(style_contract_assert_acyclic_aliases(builder))
})

test_that("Builder and Viewer share exact design primitives", {
  builder <- style_contract_tokens(
    style_contract_path("builder", "www", "builder.tokens.css")
  )
  viewer <- style_contract_tokens(
    style_contract_path("viewer", "www", "custom.css")
  )
  shared <- c(
    "--c-white",
    "--c-bg",
    "--c-surface",
    "--c-surface-2",
    "--c-surface-3",
    "--c-border",
    "--c-border-2",
    "--c-text",
    "--c-text-2",
    "--c-text-3",
    "--c-blue-50",
    "--c-blue-100",
    "--c-blue-300",
    "--c-blue",
    "--c-blue-600",
    "--c-blue-700",
    "--c-amber-50",
    "--c-amber-100",
    "--c-amber-300",
    "--c-amber",
    "--c-amber-600",
    "--c-amber-700",
    "--c-success",
    "--c-warning",
    "--c-error",
    "--shadow-1",
    "--shadow-2",
    "--shadow-3",
    "--shadow-4",
    "--r-xs",
    "--r-sm",
    "--r-md",
    "--r-lg",
    "--r-pill",
    "--ease",
    "--dur",
    "--font-sans",
    "--font-mono"
  )

  expect_true(all(shared %in% names(builder)))
  expect_identical(unname(builder[shared]), unname(viewer[shared]))
})

test_that("Builder defines semantic action and measure roles", {
  builder <- style_contract_tokens(
    style_contract_path("builder", "www", "builder.tokens.css")
  )

  expect_identical(unname(builder["--builder-action"]), "#c9500b")
  expect_identical(
    unname(builder["--builder-selection-bg"]),
    "var(--builder-action)"
  )
  expect_identical(unname(builder["--builder-measure-copy"]), "48rem")
  expect_identical(unname(builder["--builder-measure-form"]), "56rem")
  builder_font_sans <- paste(
    '-apple-system, BlinkMacSystemFont, "Segoe UI", "Inter",',
    '"PingFang SC", "Hiragino Sans GB", "Microsoft YaHei",',
    '"Helvetica Neue", Arial, sans-serif'
  )
  expect_true("--builder-font-sans" %in% names(builder))
  expect_identical(
    unname(builder["--builder-font-sans"]),
    builder_font_sans
  )
  expect_identical(
    unname(builder["--builder-accent-strong"]),
    "#65230f"
  )
})

test_that("Builder interaction roles use the amber palette", {
  builder <- style_contract_tokens(
    style_contract_path("builder", "www", "builder.tokens.css")
  )
  expected <- c(
    "--builder-action" = "#c9500b",
    "--builder-action-hover" = "#9a3412",
    "--builder-action-active" = "#7c2d12",
    "--builder-accent-strong" = "#65230f",
    "--builder-focus-ring" = "var(--c-amber-600)",
    "--builder-selection-bg" = "var(--builder-action)",
    "--builder-rail-selected-bg" = "var(--c-amber-50)",
    "--builder-rail-selected-marker" = "var(--c-amber)",
    "--builder-rail-hover-bg" = "#fffaf6",
    "--builder-rail-progress-bg" = "var(--c-amber-50)",
    "--builder-rail-progress-fg" = "var(--c-amber)"
  )

  expect_identical(unname(builder[names(expected)]), unname(expected))
})

test_that("Builder strong accent preserves all legacy amber-800 uses", {
  css <- style_contract_builder_css()
  selectors <- c(
    ".viewer-analysis-result-status",
    ".viewer-specialized-badge",
    ".viewer-point-size-value",
    ".viewer-card-method",
    ".review-viewer-content-item h5"
  )
  uses <- regmatches(
    css,
    gregexpr("var\\(--builder-accent-strong\\)", css, perl = TRUE)
  )[[1L]]

  expect_length(uses, 7L)
  for (selector in selectors) {
    start <- regexpr(selector, css, fixed = TRUE)[[1L]]
    expect_true(start > 0L, info = selector)
    selector_css <- substr(css, start, nchar(css))
    block <- regmatches(
      selector_css,
      regexpr("\\{[^}]*\\}", selector_css, perl = TRUE)
    )
    expect_match(
      block,
      "var(--builder-accent-strong)",
      fixed = TRUE,
      info = selector
    )
  }
})

test_that("Builder motion uses its 180ms duration role", {
  css <- style_contract_builder_css()
  shared_duration_uses <- regmatches(
    css,
    gregexpr("var\\(--dur\\)", css, perl = TRUE)
  )[[1L]]
  builder_duration_uses <- regmatches(
    css,
    gregexpr("var\\(--duration-base\\)", css, perl = TRUE)
  )[[1L]]

  expect_length(shared_duration_uses, 0L)
  expect_length(builder_duration_uses, 17L)
})

test_that("Builder layered stylesheets own their declared responsibilities", {
  stylesheets <- list(
    base = style_contract_builder_css("builder.base.css"),
    layout = style_contract_builder_css("builder.layout.css"),
    components = style_contract_builder_css("builder.components.css"),
    features = style_contract_builder_css("builder.features.css")
  )
  owner_map <- c(
    "body" = "base",
    ".visually-hidden" = "base",
    ".form-control" = "base",
    "pre" = "base",
    "code" = "base",
    ".builder-shell" = "layout",
    ".builder-stage" = "layout",
    ".rail-summary" = "layout",
    ".builder-stage-footer" = "components",
    ".btn" = "components",
    ".builder-dialog" = "components",
    ".builder-file-picker" = "components",
    ".enhance-module" = "features",
    ".builder-viewer-card" = "features",
    ".spatial-alignment-layout" = "features",
    ".builder-group-colors" = "features"
  )

  expect_true(
    style_contract_assert_selector_ownership(stylesheets, owner_map)
  )
  legacy_path <- file.path(
    dirname(style_contract_path("builder", "www", "builder.features.css")),
    "builder.css"
  )
  expect_false(file.exists(legacy_path))
})

test_that("Builder primary action classes share one interaction recipe", {
  css <- style_contract_builder_css("builder.components.css")

  base <- style_contract_assert_exclusive_recipe(
    css,
    c(
      ".btn-primary",
      ".btn-action",
      ".btn-primary:focus",
      ".btn-action:focus"
    ),
    c("border-color", "background", "color")
  )
  hover <- style_contract_assert_exclusive_recipe(
    css,
    c(
      ".btn-primary:hover",
      ".btn-action:hover"
    ),
    c("border-color", "background", "box-shadow")
  )
  active <- style_contract_assert_exclusive_recipe(
    css,
    c(
      ".btn-primary:active",
      ".btn-action:active"
    ),
    "background"
  )

  expect_setequal(
    base,
    c(
      "background: var(--builder-action) !important",
      "border-color: var(--builder-action) !important",
      "color: var(--c-white) !important"
    )
  )
  expect_setequal(
    hover,
    c(
      "background: var(--builder-action-hover) !important",
      "border-color: var(--builder-action-hover) !important",
      "box-shadow: var(--shadow-1)"
    )
  )
  expect_setequal(
    active,
    "background: var(--builder-action-active) !important"
  )
})

test_that("Viewer result cards share one surface geometry recipe", {
  css <- style_contract_builder_css("builder.features.css")
  surface <- style_contract_assert_exclusive_recipe(
    css,
    c(
      ".viewer-analysis-result",
      ".viewer-specialized-item"
    ),
    c("min-width", "padding", "border", "border-radius", "background")
  )

  expect_setequal(
    surface,
    c(
      "min-width: 0",
      "padding: .75rem",
      "border: 1px solid var(--c-border)",
      "border-radius: var(--r-sm)",
      "background: var(--c-surface)"
    )
  )
})


test_that("Optional analysis states change emphasis without moving cards", {
  css <- style_contract_builder_css("builder.features.css")
  hover <- style_contract_rule_declarations(css, ".enhance-module:hover")
  selected <- style_contract_rule_declarations(
    css,
    c(
      ".enhance-module:has(input:checked)",
      ".enhance-module:has(input:checked):hover",
      ".enhance-module:has(input:checked):focus-within",
      ".enhance-module.is-selected",
      ".enhance-module.is-selected:hover",
      ".enhance-module.is-selected:focus-within"
    )
  )

  expect_setequal(
    hover,
    c(
      "border-color: var(--builder-hover-border)",
      "background: var(--builder-hover-bg)",
      "box-shadow: var(--shadow-1)"
    )
  )
  expect_setequal(
    selected,
    c(
      "border-color: var(--builder-action-hover)",
      "background: var(--builder-selection-bg)",
      "box-shadow: 0 5px 16px rgba(124, 45, 18, .18)"
    )
  )
})

test_that("Builder stylesheets do not consume undefined custom properties", {
  stylesheet_files <- c(
    "builder.tokens.css",
    "builder.base.css",
    "builder.layout.css",
    "builder.components.css",
    "builder.features.css"
  )
  css <- paste(
    vapply(
      stylesheet_files,
      function(file) {
        paste(
          readLines(
            style_contract_path("builder", "www", file),
            warn = FALSE
          ),
          collapse = "\n"
        )
      },
      character(1)
    ),
    collapse = "\n"
  )

  expect_setequal(
    style_contract_undefined_custom_properties(css),
    character()
  )
})

test_that("Builder enhances every Selectize multi-select consistently", {
  root <- style_contract_path("builder")
  app <- paste(
    readLines(file.path(root, "app.R"), warn = FALSE),
    collapse = "\n"
  )
  js <- paste(
    readLines(file.path(root, "www", "builder.js"), warn = FALSE),
    collapse = "\n"
  )
  css <- style_contract_builder_css("builder.base.css")

  expect_match(app, 'tags$script(src = paste0("builder.js"', fixed = TRUE)
  expect_match(js, 'multiSelectPlaceholder = "Select…"', fixed = TRUE)
  expect_match(js, "select[multiple]", fixed = TRUE)
  expect_match(js, "MutationObserver", fixed = TRUE)
  expect_match(js, "minimumMultiSelectEmptyWidth()", fixed = TRUE)
  expect_match(js, "cerebro-multiselect-empty", fixed = TRUE)
  expect_match(
    css,
    ".selectize-control.multi .selectize-dropdown",
    fixed = TRUE
  )
  expect_match(css, "width: max-content", fixed = TRUE)
})

test_that("Spatial alignment follows its content height", {
  css <- style_contract_builder_css("builder.features.css")

  expect_false(grepl(
    ".spatial-alignment-workbench:has(> .builder-viewer-spatial-alignment[open])",
    css,
    fixed = TRUE
  ))
})
