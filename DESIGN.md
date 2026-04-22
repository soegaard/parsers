# Design

This document describes the shared design goals for the `parsers` repository.

The first target is a reusable CSS parser library that parses CSS source into a
structured AST while preserving enough source information for downstream
tooling.

## Goal

The parser should support tooling use cases such as:

- selector inspection
- declaration lookup
- grouped selector handling
- cascade and order analysis
- component-style extraction
- source-preserving rewrites later

This is a real library project, not a one-off script.

The design priorities are:

- correctness first
- explicit structure over clever shortcuts
- extensibility without premature abstraction
- clear separation between parsing and later semantic interpretation

The parser should not try to understand framework-specific conventions such as
Bootstrap. It should parse CSS faithfully.

## Non-Goals

The initial project is not:

- a CSS engine
- a browser-style cascade engine
- a framework-aware CSS analyzer

The initial scope is:

- parser
- AST
- serializer
- basic query helpers

## Core Capabilities

The CSS parser should:

- parse stylesheet input into an AST
- support style rules
- support grouped selectors
- support declarations
- support comments
- support whitespace-insensitive parsing
- support `@media`
- support `@supports`
- support `@import`
- support `@font-face`
- support `@keyframes`
- preserve source order
- preserve raw selector text
- preserve raw declaration values
- represent nested at-rules structurally
- handle malformed input gracefully with useful parse errors
- include source locations for rules and declarations when practical

## Architecture

The implementation should remain split into clear layers:

1. tokenizer or lexer
2. parser
3. AST definitions
4. pretty-printer or serializer
5. query helpers

Parsing and semantic interpretation should remain separate. The parser’s job is
to produce a faithful structured representation of the source, not to decide
whether the stylesheet is valid for a browser, a framework, or a design system.

The AST should stay simple and explicit.

The implementation should avoid fragile regex parsing.

## Public API Direction

The library should grow toward a public API shaped roughly like:

- `parse-stylesheet`
- `serialize-stylesheet`
- helpers for iterating rules
- helpers for finding declarations by property
- helpers for flattening nested at-rules when desired
- helpers for querying selectors

The API should be suitable for later tooling such as:

- theme extraction
- CSS scoping
- structural stylesheet inspection

## Implementation Plan

Implementation should proceed incrementally:

1. design the AST
2. design and implement the tokenizer
3. implement the parser in small grammar areas
4. add serialization
5. add query helpers

Tests should land as each grammar area lands.

## Testing Expectations

The project should include thorough tests, including cases for:

- grouped selectors
- duplicate declarations
- later-rule overrides
- nested at-rules
- comments
- strings, URLs, and escaped characters
- keyframes
- invalid CSS

## CSS-Specific Direction

The modern parser in `parsers/css` should target current CSS as described by
the CSS snapshot and supporting module specifications in `reference/`.

In particular:

- CSS Syntax is the primary parsing reference
- selectors should initially be preserved faithfully, even before a deeper
  selector-specific AST exists
- declaration values should initially preserve raw structure before deeper
  property-specific parsing is added
- nested rules and nested at-rules should be modeled structurally from the
  beginning

## Documentation Expectations

The repository should include:

- a README that explains project purpose
- documentation that explains the AST shape
- documentation that explains API usage
- explicit limitations
- notes on likely future extension areas
