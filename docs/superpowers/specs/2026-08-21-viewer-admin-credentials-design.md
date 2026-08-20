# Viewer Admin credentials design

## Goal

Make the built-in Viewer administrator credentials configurable while keeping
the existing zero-configuration `admin` / `admin123` behavior.

## Public interfaces

`createShinyApp()` gains two scalar character arguments:

```r
admin_account = "admin"
admin_password = "admin123"
```

The values are validated as non-missing, non-empty scalar strings and stored in
the generated app's private R configuration. They are never rendered into HTML
or JavaScript.

The repository Viewer launched with `shiny::runApp("inst")` reads:

```r
options(
  cerebro.admin.account = "admin",
  cerebro.admin.password = "admin123"
)
```

Unset options fall back to `admin` and `admin123` respectively. Callers set the
options before `runApp()` when they want different credentials.

## Runtime behavior

Both the standalone `/admin` login and the optional shinymanager Viewer login
read one normalized credential pair from `Cerebro.options`. The current
duplicated hard-coded comparisons are removed. Builder-generated apps populate
the same configuration fields.

Configured Builder login users marked Admin continue to work independently of
the built-in account.

## Security boundaries

Credentials stay server-side. Error messages do not reveal which field failed.
The existing per-session failed-login limit remains. This change does not add
password hashing for the built-in account because the generated Viewer must be
able to validate the configured password at runtime; deployments needing an
encrypted credential database can continue using the existing `auth` facility.

## Compatibility

Existing calls and direct Viewer launches retain `admin` / `admin123`. Exported
apps without the new configuration keys use the same fallback, so no migration
is required.

## Verification

Tests cover argument defaults and validation, propagation into generated Viewer
configuration, direct-launch option overrides, fallback behavior, both Admin
login paths, and absence of credentials from browser-facing assets.
