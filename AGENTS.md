# AGENTS.md

The repo contains a collection of parsers written in Racket.

The packages are intended to provide reusable parsers.
The parsers are intended to support multiple applications.
Tooling support is the first planned application, but other uses are expected
as well.

- When given rules, always ask whether they should be added to `AGENTS.md`.

Coding guidelines for Racket code in `parsers`:

1. Prefer `cond` over nested `if` or `let` patterns when branching logic is non-trivial.
2. Prefer internal `define` forms inside `cond` branches instead of wrapping branch bodies in `let` or `let*`.
3. Align right-hand-side expressions when it improves readability.
4. Avoid hanging parentheses; keep closing `))` on the same line as the final expression when practical.
5. Prefer named struct fields over numeric indices.
6. Add function comments for helpers and exported functions:
   - `;; name : contract -> result`
   - `;;   Brief purpose sentence.`
7. Add inline comments for parameter types and optional/default parameters when relevant.
8. Add a comment header before blocks of constant definitions.
9. If a constant or definition name is not self-explanatory, add an end-of-line comment explaining its meaning or purpose.
10. When symbols are used for enumeration, use `case` instead of `cond` for branching.
11. At the top of each file, add a header comment in this form:
    - `;;;`
    - `;;; Title`
    - `;;;`
    - ``
    - `;; An explanation of the contents of the source file.`
12. Use `for` constructs instead of explicit loops.
13. Use `match` for destructuring.
14. For each file export, add a comment near the top of the file, after the header and file explanation, with the export identifier and a one-line explanation. Align the start of the explanations when practical.
15. If you find an error in libraries or folders outside this repo, first make a minimal reproduction of the error, then ask how to proceed.
16. Do not add workarounds. Investigate and fix the root cause instead. Ask for help if needed.
17. Use `rackunit` for tests.
18. For consecutive calls with the same callee and simple arguments, align argument columns to improve scanability.
19. Keep `README.md` lightweight. Long-term public-facing documentation belongs in `parsers-doc`.
20. When generating local Scribble HTML output, place it outside the packages, and use `+m`; for example:
    `raco scribble +m --htmls --dest html/ parsers-doc/parsers.scrbl`.
21. In documentation, prefer explicit input and output types over a plain `procedure?`.
22. In documentation, prefer precise types over broad placeholders such as `any/c`.
23. When starting a parser for a new language, begin with a deliberately small real-language subset in order to validate the shared architecture before expanding toward broader specification coverage.
24. For languages with an official syntax or parsing reference, treat that specification as the primary parser source. Use implementation files and secondary references as supporting context.
25. When adding a new public parser module, update `parsers-doc/parsers.scrbl` in the same turn so the manual matches the shipped API.
26. Keep parsing and semantic interpretation separate.
27. Preserve raw source structure when it materially helps downstream tooling.
28. Prefer simple and explicit AST nodes over clever or overly compact representations.
29. When a local corpus is available, add a corpus checker script in `tools/` instead of relying only on unit tests.
30. Corpus checker scripts in `tools/` must skip cleanly when their local `/tmp` corpus directory is unavailable, so package-server tests do not fail when the corpus is absent.
31. The CSS corpus currently lives at `/private/tmp/lexers-css-corpus`; tooling should treat that as optional local input, not as a package dependency.
32. If files from a local `/tmp` corpus are promoted into stable regression tests, copy them into a repo-local `testdata/` directory first so the tests survive reboot cleanup.

## Packages

There are 3 packages:

- `parsers-lib` - The implementation of the parsers
- `parsers-doc` - The documentation of the parsers, written in Scribble
- `parsers` - Installs both `parsers-lib` and `parsers-doc`

## Design Notes

1. Shared notes are in `DESIGN.md`.
2. Reference documents are in `reference/`.
3. The first parser target is CSS.
