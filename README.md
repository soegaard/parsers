# parsers

Reusable parsers for multiple consumers.

The repository is arranged like `../lexers` and split into three packages:

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
- a structured AST for stylesheets, rules, declarations, and comments
- a normalized serializer
- basic query helpers for selector and declaration lookup

If we later need fixed compatibility targets, we can add additional module
paths such as `parsers/css-snapshot-2026`.

Shared design notes live in `DESIGN.md`.

Reference notes for the parser live in `reference/`.

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

## License

MIT. See [LICENSE](LICENSE).
