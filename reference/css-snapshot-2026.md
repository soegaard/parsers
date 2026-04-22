# CSS Snapshot 2026

Primary URL:

- <https://www.w3.org/TR/css-2026/>

Publication history:

- Group Note published February 26, 2026
- publication history page also lists March 26, 2026

Why this document matters:

- It is the closest thing to the current top-level definition of modern CSS.
- It identifies which modules together define the current state of CSS.
- It is the right baseline for deciding what `parsers/css` should mean.

What to use it for in this repo:

- deciding the intended modern target for `parsers/css`
- identifying which module specifications are in scope
- deciding when a new module should be considered part of the default parser

Important caveat:

- The snapshot is a map of CSS, not a single standalone grammar for the entire
  language.
- Actual parser implementation work must still follow the module specifications
  that define syntax in detail.

Immediate follow-on specs:

- CSS Syntax Module Level 3
- Selectors Level 4
- CSS Values and Units Module Level 4
- CSS Nesting Module Level 1
- CSS Cascading and Inheritance Level 5
