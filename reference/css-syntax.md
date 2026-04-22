# CSS Syntax

Primary URL:

- <https://www.w3.org/TR/css-syntax-3/>

Current published document noted while setting up this repo:

- CSS Syntax Module Level 3
- W3C Candidate Recommendation Draft, December 24, 2021

This is the most important parser reference.

Why it matters:

- It defines how a stream of bytes becomes CSS tokens.
- It defines the core parser entry points such as parsing a stylesheet, a rule,
  a declaration, and lists of component values.
- It defines recovery-oriented parsing algorithms that are central to CSS.

Sections to implement against first:

- section 3, Tokenizing and Parsing CSS
- section 4, Tokenization
- section 5, Parsing
- section 8, Defining Grammars for Rules and Other Values
- section 9, CSS stylesheets

Repository implications:

- `parsers/private/css-tokens.rkt` should stay close to the spec token model
- `parsers/private/css-parser.rkt` should mirror the parser entry points and
  recursive consumption algorithms from this spec
- AST node boundaries should reflect the syntax model here before adding
  higher-level semantic normalization

Important design note:

- CSS Syntax Level 3 explicitly supersedes the lexical scanner and grammar from
  CSS 2.1, so this should be the starting point rather than historical CSS
  grammar texts
