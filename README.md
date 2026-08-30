# parsers

Reusable parsers for multiple consumers.

The repository is split into three Racket packages:

- `parsers`
  Meta-package that installs the library and documentation
- `parsers-lib`
  Parser implementations
- `parsers-doc`
  Scribble manual

The current parser targets are CSS, TOML, Lua, and Scheme.

The public CSS entry point is `parsers/css`, which is intended to track the
modern CSS standard over time.

The public TOML entry point is `parsers/toml`. It parses TOML 1.0 structure
into a source-preserving AST for configuration tooling and inspection.

The public Lua entry point is `parsers/lua`. It parses Lua 5.4 lexical and
statement-like structure into a source-preserving AST for source tooling.

The public Scheme entry point is `parsers/scheme`. It parses reader structure
for R5RS, R6RS, R7RS, Chez, Guile, CHICKEN, and Gambit.

The current CSS library provides:

- parsing from strings or ports
- a structured AST for stylesheets, rules, at-rules, declarations, comments,
  recovery nodes, and source spans
- normalized and source-preserving serialization paths
- selector, declaration, custom-property, media, supports, and recovery query
  helpers
- a reduced exact-target computed-style layer for tooling
- rewrite helpers for common CSS transformation workflows

The TOML library provides:

- parsing from strings or ports
- an explicit AST for documents, tables, keys, values, comments, and recovery
  nodes
- exact source-preserving serialization
- exact table and key lookup helpers

The Lua library provides:

- parsing from strings or ports
- source-preserving chunks, statement-like forms, comments, lexical tokens,
  and recovery nodes
- exact source-preserving serialization
- statement inspection by leading Lua keyword or form kind

The Scheme library provides:

- parsing from strings or ports with an explicit dialect choice
- source-preserving documents, lists, vectors, bytevectors, reader
  abbreviations, atoms, comments, and recovery nodes
- exact source-preserving serialization
- reader-form and atom inspection helpers

If we later need fixed compatibility targets, we can add additional module
paths such as `parsers/css-snapshot-2026`.

## Local Notes

`DESIGN.md` and `reference/` are intentionally local-only working notes.
They are ignored by Git and are not part of the package release.

## Documentation

The long-form public documentation lives in `parsers-doc`.

To build the local manual:

```sh
raco scribble +m --htmls --dest html/ parsers-doc/parsers.scrbl
```

## Corpus

If the local CSS corpus is available at `/private/tmp/lexers-css-corpus`, run:

```sh
racket tools/check-css-corpus.rkt
```

The checker skips cleanly when the corpus directory is absent.

If the local Scheme corpus is available at `/private/tmp/lexers-scheme-corpus`,
run one bounded process:

```sh
racket -c tools/check-scheme-corpus.rkt --memory-limit-mb 256
```

The checker skips cleanly when the corpus directory is absent.

If the local Lua corpus is available at `/private/tmp/lexers-lua-corpus`, run:

```sh
racket -c tools/check-lua-corpus.rkt
```

The checker skips cleanly when the corpus directory is absent.

To build a larger local corpus of distinct CSS files from nearby workspaces,
run:

```sh
racket tools/extend-css-corpus.rkt 1000 /private/tmp/lexers-css-corpus /private/tmp/lexers-css-corpus-1000 ..
```

Then check the expanded corpus with:

```sh
racket tools/check-css-corpus.rkt /private/tmp/lexers-css-corpus-1000
```

For computed-style checking on a local corpus, use bounded runs with an
explicit memory cap. For one chunk:

```sh
racket tools/check-css-corpus-compute.rkt \
  --memory-limit-mb 256 \
  --start-index 0 \
  --max-files 100 \
  --progress-every 25 \
  --max-selector-groups-per-file 2 \
  /tmp/lexers-css-corpus
```

To sweep the whole corpus in safe chunks and print an aggregate summary, run:

```sh
sh tools/check-css-corpus-compute-all.sh /tmp/lexers-css-corpus
```

The wrapper defaults to:

- `CHUNK_SIZE=100`
- `SELECTOR_GROUPS_PER_FILE=2`
- `MEMORY_LIMIT_MB=256`
- `PROGRESS_EVERY=25`

Override those with environment variables when needed, for example:

```sh
CHUNK_SIZE=50 MEMORY_LIMIT_MB=384 sh tools/check-css-corpus-compute-all.sh /tmp/lexers-css-corpus
```

For stable real-CSS computed-style regression checks that survive `/tmp`
cleanup, use the copied fixture files in `testdata/css-compute-fixtures/`:

```sh
racket tools/check-css-compute-fixtures.rkt
```

## License

MIT. See [LICENSE](LICENSE).
