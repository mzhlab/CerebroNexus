# Builder Amber Theme

## Scope

Replace the redesigned Builder workspace's blue interaction theme with the
existing amber palette. Keep layout, behavior, content-category colors, and
semantic success, warning, and error colors unchanged.

## Color roles

- Primary actions, active workflow steps, progress, and selected markers use a
  medium-to-dark amber with sufficient contrast for white text.
- Selected surfaces, hover surfaces, focus backgrounds, and lightweight
  emphasis use pale amber.
- Focus rings use the same amber family and remain clearly visible.
- Semantic status colors and content-type tags retain their current colors.

## Implementation

Change shared Builder theme tokens first. Replace remaining interaction-only
references to blue tokens with the equivalent amber token. Do not introduce a
second theme layer, page-specific overrides, JavaScript, or new dependencies.

## Checks

Static style-contract checks should confirm that Builder interaction roles no
longer resolve to blue and that status/category colors remain unchanged.
