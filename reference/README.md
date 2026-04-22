# Reference

This directory holds project reference material for the `parsers` repository.

It contains both:

- local copies of important reference documents
- short implementation notes that explain how those documents matter to this
  repo

## Current Baseline

The modern CSS parser in `parsers/css` should track the current CSS standard as
represented by the W3C CSS snapshot.

Local HTML copies:

- `css-2026.html`
- `css-syntax-3.html`
- `selectors-4.html`
- `css-values-4.html`
- `css-nesting-1.html`
- `css-cascade-5.html`

Companion notes:

- `css-snapshot-2026.md`
- `css-syntax.md`
- `css-selectors.md`
- `css-values.md`
- `css-nesting.md`
- `css-cascade.md`

## How To Use This Directory

- Treat the `.html` files as the local source documents.
- Treat the `.md` files as an implementation map, not as a replacement for the
  specifications.
- When the parser grows to cover a new feature area, add or extend a reference
  note here before changing the parser.
- Prefer official W3C documents as primary references.
- Record concrete URLs and publication dates in the note files when a decision
  depends on a specific spec snapshot.

## Refreshing Local Copies

When updating the local HTML copies:

- keep the downloaded filenames stable
- record the intended baseline in the companion `.md` note
- avoid editing the downloaded HTML by hand

## Scope

These notes currently focus on:

- top-level stylesheet structure
- tokenization and parsing algorithms
- selectors
- value grammar and common value types
- modern extensions that affect parsing shape, such as nesting
