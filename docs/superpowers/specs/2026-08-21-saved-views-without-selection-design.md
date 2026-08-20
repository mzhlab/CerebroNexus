# Saved views without an active selection

## Goal

Keep the Saved views library available whenever the Linked views data set is
ready, regardless of whether cells are currently selected.

## Root cause

The browser currently obtains the active data-set fingerprint by capturing a
complete shareable configuration. That capture requires selection geometry and
throws when no selection exists. The exception is converted to a missing
fingerprint, so every local snapshot is filtered out of the Saved views list.

## Design

Expose the already-loaded data-set fingerprint in the lightweight Linked views
state summary. The configuration dialog reads that summary directly when
filtering local snapshots and no longer captures selection-dependent JSON merely
to identify the data set.

With no active selection:

- Share views remains openable.
- Matching Saved views remain visible and can be opened, downloaded, renamed,
  or deleted.
- Save current view, JSON export/copy, and Create share link remain disabled.
- Import/Open JSON remains available as before.

The JSON schema, localStorage schema, data-set scoping, and share-link behavior
do not change.

## Verification

A browser regression test seeds a snapshot for the loaded data-set fingerprint,
clears the selection, opens Share views, and asserts that the Saved view remains
visible and actionable.
