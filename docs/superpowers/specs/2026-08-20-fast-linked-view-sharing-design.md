# Fast Linked View Sharing

## Goal

Make creating and copying a Linked views share link feel immediate after the current view has been prepared, without changing the persisted JSON document, link format, expiry, or Admin behavior.

## Prepared configuration ownership

The server becomes the owner of recently prepared normalized configurations. A successful `prepare` request stores the normalized JSON in a session-local bounded cache and returns an opaque `prepared_id` together with the existing filename and selected-cell count.

Each cached record is bound to the current Shiny session, dataset fingerprint, and prepared revision. Records expire after five minutes and the cache retains at most eight records per session. IDs use cryptographically random, URL-safe tokens. A record may be reused while valid so retrying the same create operation remains safe.

## Create flow

The browser cache stores `prepared_id`, not the normalized JSON body for sharing. `share_create` sends only `nonce`, `action`, `prepared_id`, and the independently generated share token.

The server resolves the prepared record, verifies its dataset fingerprint and expiry, and writes its already-normalized JSON directly to the existing SQLite share store. It does not decode and normalize the JSON a second time. Missing, expired, or mismatched IDs return a safe preparation-expired error and leave the UI retryable.

Import, export, copy-JSON, and local saved views may continue using the prepared JSON result where they need the document body. The prepare response therefore retains `json`; only the share-create transport stops echoing it back to the server.

## UI states

If no current prepared record exists, clicking Create shows `Preparing view…`. Once preparation resolves and the small create request is sent, it shows `Creating share link…`. Success and failure always clear pending state. Existing nonce replay protection remains in place.

## Clipboard behavior

Copy link first performs the synchronous textarea/`execCommand('copy')` fallback inside the original click event. If it succeeds, the button immediately shows `Copied ✓` and resolves successfully. Only if that synchronous path fails does the helper call `navigator.clipboard.writeText`, bounded by the existing 500 ms timeout.

The helper restores focus after removing its temporary textarea so copying does not disrupt keyboard interaction.

## Compatibility and security

SQLite schema, normalized JSON format, random share token format, dataset fingerprint checks, 90-day expiry, revocation, and Admin authorization do not change. Prepared records never cross sessions and are bounded by count and lifetime.

## Tests

Regression code will cover prepared-ID generation, session/dataset/expiry rejection, bounded eviction, create payloads without `config_json`, direct SQLite creation from cached JSON, immediate synchronous clipboard success, async Clipboard API fallback, and visible preparation-versus-creation states.

Per the user's instruction, test code will be authored but no local test or App command will be executed.
