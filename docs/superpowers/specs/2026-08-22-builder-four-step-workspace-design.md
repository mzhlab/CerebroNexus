# Builder four-step workspace

## Goal

Present the existing Builder as one calm project workspace with four full-page
steps: Data, Configure, Review, and Build. Preserve all Builder behavior and
data contracts while making project, dataset, progress, and primary actions
immediately legible.

## Shell

- The project header shows project identity and save state, followed by New,
  Open, and Save actions.
- The workflow navigation is always visible below the project header.
- On desktop, the dataset rail stays on the left and the active step occupies
  the remaining width. On narrow screens the rail stacks above the step.
- Every step uses the same header, content, and footer structure. The footer
  carries short state text on the left and the primary action on the right.

## Steps

### Data

Show loaded datasets in the shared rail. The page contains one dominant drop
zone and compact secondary actions for local files, the data browser, and
examples. Empty, loading, and failed imports remain explicit without rendering
empty result cards.

### Configure

Keep the selected dataset visible in the rail. Reuse the existing configuration
surfaces and controls, grouped under the current Basics, Groups, Views, and
Extras concepts. Finishing a dataset check remains the primary footer action.

### Review

Lead with blocking status, then show the frozen dataset and output summary.
Keep technical detail behind the existing disclosures. Continue to Build is
available only after the existing confirmation rules pass.

### Build

Present output type, destination, frozen-plan summary, progress, recovery, and
result in one page. Build remains explicitly triggered and all existing
background worker and publication behavior is retained.

## Implementation boundary

- Reuse current Shiny input/output IDs, observers, workflow state, dataset rail,
  project dialogs, validation, and build status components.
- Change UI composition and shared Builder CSS only where possible; add no new
  framework or parallel state model.
- Preserve workbook, multi-workbook, multi-Sheet, per-Sheet loading, generated
  App, CRB, Viewer, and project restore contracts.
- Use the existing design tokens, controls, focus handling, reduced-motion
  rules, and accessible labels.
- Do not introduce Enhanced Linked views or other branch 4 behavior.

## Acceptance

- Project context, workflow position, selected dataset, and primary action are
  visible without searching.
- All four steps share one stable shell with no duplicate navigation or action
  systems.
- Existing Builder actions continue to use their current server paths and IDs.
- The layout remains usable on desktop and narrow screens without horizontal
  overflow or compressed form controls.
