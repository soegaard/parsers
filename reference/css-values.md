# CSS Values And Units

Primary URL:

- <https://www.w3.org/TR/css-values-4/>

Current published document noted while setting up this repo:

- CSS Values and Units Module Level 4
- W3C Working Draft, March 12, 2024

Why this document matters:

- It defines the grammar notation used by CSS property values.
- It defines common data types such as strings, URLs, numbers, dimensions, and
  percentages.
- It is central once the parser moves beyond stylesheet and rule boundaries
  into declaration values.

What to use it for in this repo:

- designing value-oriented AST nodes
- deciding how much to preserve as raw component values versus parsed values
- planning a future value parser layer on top of CSS Syntax component values

Implementation advice:

- Do not overfit the initial parser to property-specific value grammars.
- First preserve declaration values faithfully as component values.
- Add typed value parsers later, module by module, where the extra structure is
  actually useful.
