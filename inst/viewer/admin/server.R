## Administrator-only navigation and share inventory.

viewer_admin_send <- function(ok, action, nonce = "", ...) {
  session$sendCustomMessage(
    "viewer_admin_result",
    c(
      list(ok = isTRUE(ok), action = action, nonce = nonce),
      list(...)
    )
  )
}

viewer_admin_inventory <- function(nonce = "") {
  if (!viewer_is_admin(session)) {
    viewer_admin_send(FALSE, "list", nonce, code = "forbidden")
    return(invisible(NULL))
  }
  if (is.null(coordviews_share_store)) {
    viewer_admin_send(
      FALSE,
      "list",
      nonce,
      code = "share_unavailable",
      message = "Share links are unavailable on this server."
    )
    return(invisible(NULL))
  }
  rows <- cv_share_store_list(coordviews_share_store)
  viewer_admin_send(
    TRUE,
    "list",
    nonce,
    records = viewer_admin_records(rows)
  )
  invisible(NULL)
}

if (viewer_is_admin(session)) {
  session$onFlushed(
    function() {
      insertUI(
        selector = "#sidebar_item_admin_placeholder",
        where = "afterEnd",
        ui = tags$li(
          id = "sidebar_item_admin",
          menuItem(
            "Admin",
            tabName = "admin",
            icon = icon("shield-halved")
          )$children
        ),
        immediate = TRUE
      )
      session$sendCustomMessage(
        "viewer_admin_capability",
        list(allowed = TRUE, user = viewer_auth_context(session)$user)
      )
      if (viewer_admin_route(isolate(session$clientData$url_pathname))) {
        updateTabItems(session, "sidebar", selected = "admin")
        viewer_admin_inventory()
      }
    },
    once = TRUE
  )
} else {
  session$onFlushed(
    function() {
      session$sendCustomMessage(
        "viewer_admin_capability",
        list(allowed = FALSE)
      )
      if (viewer_admin_route(isolate(session$clientData$url_pathname))) {
        showNotification(
          "Administrator access is required.",
          type = "error",
          duration = 6
        )
        session$sendCustomMessage(
          "viewer_admin_access",
          list(allowed = FALSE)
        )
      }
    },
    once = TRUE
  )
}

observe({
  req(viewer_is_admin(session))
  req(identical(input[["sidebar"]], "admin"))
  invalidateLater(2000, session)
  viewer_admin_inventory()
})

observeEvent(
  input[["viewer_admin_request"]],
  {
    request <- input[["viewer_admin_request"]]
    nonce <- if (
      is.list(request) &&
        is.character(request$nonce) &&
        length(request$nonce) == 1L &&
        !is.na(request$nonce)
    ) {
      substr(request$nonce, 1L, 128L)
    } else {
      ""
    }
    if (!viewer_is_admin(session)) {
      viewer_admin_send(FALSE, "forbidden", nonce, code = "forbidden")
      return(invisible(NULL))
    }
    action <- if (
      is.list(request) &&
        is.character(request$action) &&
        length(request$action) == 1L &&
        !is.na(request$action)
    ) {
      request$action
    } else {
      ""
    }
    tryCatch(
      {
        if (identical(action, "list")) {
          viewer_admin_inventory(nonce)
        } else if (identical(action, "revoke")) {
          if (is.null(coordviews_share_store)) {
            cv_share_abort(
              "share_unavailable",
              "Share links are unavailable on this server."
            )
          }
          token <- cv_share_token_input(request$token, "link")
          cv_share_store_revoke_admin(coordviews_share_store, token)
          viewer_admin_send(TRUE, "revoke", nonce, token = token)
        } else {
          cv_share_abort("invalid_action", "The Admin request is invalid.")
        }
      },
      error = function(error) {
        viewer_admin_send(
          FALSE,
          action,
          nonce,
          code = error$code %||% "internal",
          message = if (inherits(error, "cv_share_error")) {
            conditionMessage(error)
          } else {
            "The Admin request could not be completed."
          }
        )
      }
    )
  },
  ignoreInit = TRUE
)
