# CSS Nesting

Primary URL:

- <https://www.w3.org/TR/css-nesting-1/>

Why this document matters:

- It changes the shape of modern stylesheet parsing.
- Nested style rules mean that block contents cannot be modeled only as a flat
  list of declarations.
- It is one of the clearest examples of why `parsers/css` should target modern
  CSS rather than historical CSS alone.

What to use it for in this repo:

- deciding the AST shape for style blocks
- planning the distinction between declaration items and nested rule items
- testing that parser recovery remains correct inside nested blocks

Implementation advice:

- keep the first parser architecture flexible enough for a block to contain a
  mixed sequence of declarations and nested rules
- avoid baking in a CSS 2.1-only assumption that a qualified rule body is just
  declarations
