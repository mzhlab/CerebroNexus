# Shared Linked View Selection Geometry Design

## Goal

A shared Linked views URL opens directly on the Linked views page, restores the
shared visualization without opening the workspace-management dialog, and draws
the exact region the creator selected.

## Configuration contract

The current configuration schema is extended without backward compatibility.
`selection` contains:

- `cells`: the selected cell barcodes;
- `source`: the identity of the panel where the selection was drawn;
- `geometry`: the committed selection geometry, containing the selection mode
  (`lasso` or `box`) and the complete polygon in the source panel's normalized
  data coordinates.

Each polygon point is a finite `[x, y]` pair. The polygon must contain at least
three points and is bounded by the configuration's existing size and complexity
limits. Geometry is required whenever selected cells are present. Saving
normalized data coordinates, rather than canvas pixels, preserves the exact
user-drawn path while allowing it to follow the same viewport projection when
the browser or panel size changes.

## Capture and restoration

When a lasso or box selection is committed, Linked views already retains its
normalized polygon as `lassoData` on the source panel. Configuration capture
serializes that polygon verbatim together with the source panel identity and
mode. It never derives geometry from the selected cells.

Configuration validation verifies the geometry structure and source identity.
Restoration resolves the source panel, restores the selected cell set, assigns
the saved polygon to that panel's committed `lassoData`, clears competing panel
outlines, and redraws. The normal projection path converts the normalized points
to screen coordinates, so the restored dashed boundary follows the creator's
path rather than a convex hull around the selected cells.

## Shared URL entry

When the page contains a valid `linked_view` query parameter and Linked views is
ready, the client activates the `coordinated_views` sidebar tab before applying
the configuration. It sends the existing share-open request in the background
and does not call `openDialog()`. On success, it applies the configuration and
removes the query parameter from browser history. The workspace-management
dialog remains available only when a user explicitly presses **Share views**.

If opening or applying the share fails, an inline visible error is shown on the
Linked views page; the management dialog is not opened merely to carry status.

## Tests

Focused tests cover:

- JSON validation and normalization of exact polygon coordinates;
- capture serializing the committed source panel geometry;
- restoration assigning that geometry directly rather than computing a hull;
- shared URLs selecting the Linked views tab;
- shared URLs never opening the management dialog;
- successful share-open applying the configuration and cleaning the URL;
- invalid geometry and share-open failures producing a visible error.

