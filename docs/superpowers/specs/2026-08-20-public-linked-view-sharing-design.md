# Public Linked View Sharing

## Goal

Allow every Viewer visitor—anonymous, authenticated, or administrator—to create a read-only link for the current Linked views cell selection and settings. Authentication remains required only for management under `/admin`.

## User experience

The existing **Share with a link** region remains a separate section in the Linked views management dialog and is visible to every Viewer visitor. It is not grouped with local JSON transfer or browser-private saved views.

The create button is available once the Linked views state contains a valid cell selection. Preparation and creation use distinct status text: `Preparing view…` while producing the normalized configuration and `Creating share link…` while persisting it. A successful request shows the expiring link and copy action. Failures restore the controls and show a safe, actionable error.

## Authorization boundary

Creating and opening a share link does not require authentication or administrator status. The server still validates the request, normalized configuration, dataset fingerprint, token, expiry, and revocation state.

The `/admin` route and all inventory/revocation operations retain their existing administrator check. The built-in administrator and any configured account whose administrator flag is true may manage links. Ordinary authenticated users and anonymous visitors cannot access those operations.

## Persistence and identity

The existing SQLite store remains authoritative. Each row contains the normalized JSON document and its metadata; no separate JSON files are introduced. Random unguessable tokens, multiple active links, and the 90-day expiry remain unchanged.

For audit metadata, authenticated creators retain their username. Anonymous creators are stored without a username. Creator identity never gates creation or opening.

## Implementation boundary

The client must stop hiding or disabling the share region based on the `viewer_admin_capability` message. Share readiness depends only on a valid prepared Linked views state and whether another share operation is active.

The server must remove the administrator guard from `share_create`. Admin list and revoke handlers remain unchanged. The share payload schema and SQLite schema remain compatible.

This change does not alter password policy, default administrator provisioning, Admin UI behavior, token format, expiry duration, or the linked-view configuration schema.

## Error handling

An unavailable or unwritable share store returns the existing safe availability error. Invalid configurations and dataset mismatches remain rejected. Client timeouts and server failures clear the pending state and leave the user able to retry. No creation path should report that administrator access is required.

## Tests

Regression coverage will assert that:

- the share region is visible without an administrator capability;
- anonymous, ordinary authenticated, and administrator sessions may submit `share_create`;
- anonymous creation persists a link with no creator username;
- authenticated creation records the username when available;
- valid links remain openable without authentication;
- Admin inventory and revocation remain restricted to administrators;
- legacy administrator-only client and server guards are absent.

Per the user's instruction for this change, tests will be authored but not executed locally.
