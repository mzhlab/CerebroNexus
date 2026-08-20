## Pure authorization and response helpers for the Viewer Admin surface.

viewer_admin_route <- function(path) {
  is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    grepl("/admin/?$", path) &&
    !grepl("/admin/.+", path)
}

viewer_auth_context <- function(session) {
  closed <- list(authenticated = FALSE, user = NULL, is_admin = FALSE)
  if (is.null(session) || is.null(session$userData)) {
    return(closed)
  }
  context <- tryCatch(
    get("viewer_auth", envir = session$userData, inherits = FALSE),
    error = function(error) NULL
  )
  if (!is.list(context)) {
    return(closed)
  }
  user <- context$user
  authenticated <- isTRUE(context$authenticated) &&
    is.character(user) &&
    length(user) == 1L &&
    !is.na(user) &&
    nzchar(user)
  list(
    authenticated = authenticated,
    user = if (authenticated) user else NULL,
    is_admin = authenticated && isTRUE(context$is_admin)
  )
}

viewer_is_admin <- function(session) {
  isTRUE(viewer_auth_context(session)$is_admin)
}

viewer_admin_records <- function(rows) {
  expected <- c(
    "token",
    "fingerprint",
    "dataset_label",
    "creator",
    "created_at",
    "expires_at",
    "revoked_at"
  )
  if (!is.data.frame(rows) || !all(expected %in% names(rows)) || !nrow(rows)) {
    return(list())
  }
  lapply(seq_len(nrow(rows)), function(index) {
    value <- function(name) {
      item <- rows[[name]][[index]]
      if (length(item) != 1L || is.na(item)) "" else as.character(item)
    }
    list(
      token = value("token"),
      fingerprint = value("fingerprint"),
      dataset_label = value("dataset_label"),
      creator = value("creator"),
      created_at = value("created_at"),
      expires_at = value("expires_at")
    )
  })
}
