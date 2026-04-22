# Selectors

Primary URL:

- <https://www.w3.org/TR/selectors-4/>

Why this document matters:

- Qualified rules use selectors in their prelude.
- Modern CSS parsing is not complete without a plan for selector syntax.
- New selector forms often affect lookahead and recovery behavior.

What to use it for in this repo:

- defining the structure of selector-related AST nodes
- deciding how much of selector parsing belongs in the core stylesheet parser
- guiding future selector-specific parser helpers

Practical boundary:

- The initial stylesheet parser can treat selector preludes as structured
  component-value sequences.
- A more detailed selector parser can be layered on top or factored into its
  own internal module once the core CSS Syntax parser is stable.

Initial implementation bias:

- first parse the rule boundary correctly
- then refine selector internals

This matches CSS’s architecture better than trying to front-load a complete
Selectors parser before the main stylesheet parser exists.
