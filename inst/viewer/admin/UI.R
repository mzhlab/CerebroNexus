## Tab: Administrator-only share-link management.

tab_admin <- tabItem(
  tabName = "admin",
  div(
    class = "viewer-admin-page",
    tags$section(
      id = "viewer-admin-login",
      class = "viewer-admin-login",
      `aria-labelledby` = "viewer-admin-login-title",
      tags$div(
        class = "viewer-admin-kicker",
        icon("shield-halved"),
        "Administrator"
      ),
      tags$h3(id = "viewer-admin-login-title", "Admin sign in"),
      tags$p("Sign in to review and revoke shared-view links."),
      tags$label(`for` = "viewer-admin-user", "Username"),
      tags$input(
        id = "viewer-admin-user",
        class = "form-control",
        type = "text",
        autocomplete = "username"
      ),
      tags$label(`for` = "viewer-admin-password", "Password"),
      tags$input(
        id = "viewer-admin-password",
        class = "form-control",
        type = "password",
        autocomplete = "current-password"
      ),
      tags$button(
        id = "viewer-admin-sign-in",
        class = "viewer-admin-sign-in",
        type = "button",
        "Sign in"
      ),
      tags$div(
        id = "viewer-admin-login-status",
        class = "viewer-admin-login-status",
        role = "status",
        `aria-live` = "polite"
      )
    ),
    div(
      id = "viewer-admin-content",
      style = "display:none",
      div(
        class = "viewer-admin-heading",
        div(
          class = "viewer-admin-kicker",
          icon("shield-halved"),
          "Administrator"
        ),
        tags$h3("Share management"),
        tags$p(
          "Review every active linked-view snapshot, copy its URL, or revoke it. ",
          "Recipients can restore a snapshot but cannot change the stored link."
        )
      ),
      tags$section(
        class = "viewer-admin-card",
        `aria-labelledby` = "viewer-admin-shares-title",
        div(
          class = "viewer-admin-card-head",
          div(
            tags$h4(id = "viewer-admin-shares-title", "Shared views"),
            tags$p("Links are private bearer URLs and expire after 90 days.")
          ),
          tags$button(
            type = "button",
            id = "viewer-admin-refresh",
            class = "viewer-admin-refresh",
            icon("rotate"),
            "Refresh"
          )
        ),
        div(
          id = "viewer-admin-share-list",
          class = "viewer-admin-share-list",
          `aria-live` = "polite"
        )
      ),
      tags$div(
        id = "viewer-admin-status",
        class = "viewer-admin-status",
        role = "status",
        `aria-live` = "polite"
      )
    )
  )
)
