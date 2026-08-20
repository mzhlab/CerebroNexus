## Tab: Administrator-only share-link management.

tab_admin <- tabItem(
  tabName = "admin",
  div(
    class = "viewer-admin-page",
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
