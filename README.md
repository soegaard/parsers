# parsers

Reusable parsers for multiple consumers.

The repository is split into three Racket packages:

- `parsers`
  Meta-package that installs the library and documentation
- `parsers-lib`
  Parser implementations
- `parsers-doc`
  Scribble manual

The first parser target is CSS.

The public CSS entry point is `parsers/css`, which is intended to track the
modern CSS standard over time.

The current CSS library provides:

- parsing from strings or ports
- a structured AST for stylesheets, rules, at-rules, declarations, comments,
  recovery nodes, and source spans
- normalized and source-preserving serialization paths
- selector, declaration, custom-property, media, supports, and recovery query
  helpers
- a reduced exact-target computed-style layer for tooling
- rewrite helpers for common CSS transformation workflows

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
