#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     parsers/css))

@(define css-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parsers/css))
     the-eval))

@title{Parsers}
@author+email["Jens Axel Søgaard" "jensaxel@soegaard.net"]

This manual documents the public APIs in the @tt{parsers} packages.

@italic{Note:} The @tt{parsers} package and this documentation were written
with Codex.

@defmodule[parsers/css]

The first target is a reusable CSS parser and rewrite library intended for
tooling-oriented use cases such as selector inspection, declaration lookup,
cascade-oriented analysis, source-preserving edits, and later higher-level
transforms.

@section{Overview}

The @racketmodname[parsers] collection is arranged like @filepath{../lexers}:
the public library lives in @tt{parsers-lib}, the manual lives in
@tt{parsers-doc}, and the top-level @tt{parsers} package installs both.

The public CSS entry point is @racketmodname[parsers/css]. It is intended to
track the modern CSS standard over time. If fixed compatibility targets become
useful later, they can be added as separate module paths such as
@racketmodname[parsers/css-snapshot-2026].

This library is built around four layers:

@itemlist[
 @item{@bold{Parsing}: raw CSS source becomes a stylesheet AST.}
 @item{@bold{Derived views}: selectors, values, and some at-rule preludes can
       be inspected through richer helper APIs without changing the underlying
       AST.}
 @item{@bold{Queries}: common lookups such as “find declarations”, “find rules
       by pseudo”, or “find supports features”.}
 @item{@bold{Rewrites}: normalized AST rewrites plus a smaller
       source-preserving rewrite layer for targeted declaration/block edits.}]

The parser is intentionally @italic{not} a browser engine, layout engine, or
full CSS semantic validator. It parses structure faithfully enough for tooling
and rewriting, and keeps semantic interpretation layered on top.

@section{Quick Start}

For most consumers, the workflow is:

@itemlist[
 @item{parse a stylesheet with @racket[parse-css]}
 @item{inspect rules, declarations, and derived selector/value structure}
 @item{optionally apply rewrites}
 @item{serialize back to CSS with @racket[serialize-stylesheet]}]

@examples[#:eval css-eval
(define stylesheet
  (parse-css ".card, .panel { color: red; }"))
(css-stylesheet? stylesheet)
(map css-style-rule-selector-groups (css-stylesheet-rules stylesheet))
(serialize-stylesheet
 (css-rename-class stylesheet "card" "tile"))
]

@section{Core Model}

The parser returns a small explicit AST:

@itemlist[
 @item{@racket[css-stylesheet?] for a full stylesheet}
 @item{@racket[css-style-rule?] for ordinary style rules}
 @item{@racket[css-at-rule?] for at-rules such as @litchar|{@media}| and
       @litchar|{@supports}|}
 @item{@racket[css-declaration?] for declarations}
 @item{@racket[css-comment?] for preserved comments}
 @item{@racket[css-recovery?] for malformed fragments the parser skipped but
       recorded}
 @item{@racket[css-source-span?] for source locations when available}]

The AST is intentionally simpler than a browser’s internal model. Raw selector
text, declaration values, source order, comments, and recovery information are
preserved first; richer interpretation is exposed through helper APIs rather
than forced into the base tree.

@section{Parsing}

The parser currently handles a substantial structural subset of modern CSS,
including style rules, grouped selectors, declarations, comments, and common
at-rules such as @litchar|{@media}|, @litchar|{@supports}|,
@litchar|{@import}|, @litchar|{@font-face}|, and @litchar|{@keyframes}|.

Internally it uses:

@itemlist[
 @item{@racketmodname[lexers/css] as the tokenizer source}
 @item{a handwritten structural reader for rules, at-rules, blocks, and
       declarations}
 @item{derived selector/value/media/supports helpers layered on top of the raw
       AST}]

Malformed input is handled with recovery nodes where possible, so tooling can
keep working on imperfect stylesheets instead of failing hard on the first
error.

@section{Serialization}

There are two main serialization modes:

@itemlist[
 @item{@bold{normalized}: serialize the AST with consistent spacing}
 @item{@bold{source-preserving}: when the stylesheet still carries original
       source text and the operation did not invalidate it, return that
       original text instead}]

Most normalized AST rewrites clear the preserved source string intentionally.
The smaller source-preserving rewrite family edits source slices directly and
then reparses the result.

@section{Query Helpers}

Query helpers sit above the raw AST and derived structures. They are intended
for common tooling tasks such as:

@itemlist[
 @item{iterating rules in source order}
 @item{finding declarations by property}
 @item{matching exact selector groups or exact raw selector text}
 @item{querying selectors or pseudos}
 @item{collecting derived @litchar|{@media}| and @litchar|{@supports}| information}
 @item{inspecting parser recovery output}]

@section{Rewrite Helpers}

The rewrite layer is broad enough now to support many PostCSS-style workflows.
@margin-note{PostCSS is a JavaScript-based CSS transformation ecosystem built
around plugins that parse CSS, transform an AST, and serialize the result
again. The rewrite helpers here aim to support many of the same kinds of
transformations, but in Racket and with this library’s AST model.}
It currently includes:

@itemlist[
 @item{declaration rewrites and removals}
 @item{selector rewrites, including class renaming and selector prefixing}
 @item{at-rule prelude rewrites for @litchar|{@media}|, @litchar|{@supports}|, and
       @litchar|{@import}|}
 @item{rule insertion, removal, cloning, wrapping, splitting, and merging}
 @item{custom-property, URL, keyframes, comment, and nesting-oriented helpers}
 @item{source-preserving declaration/block edits for targeted cases}]

The important design distinction is that some helpers are fully AST-based,
while others still operate on preserved raw selector or prelude text. The
reference section calls that out where it matters.

@section{Derived Structures}

The raw AST keeps selectors and many values as preserved text. Richer structure
is available through helper APIs:

@itemlist[
 @item{selector parts, compounds, pseudos, attributes, and namespace-aware
       forms}
 @item{component values such as numbers, percentages, dimensions, strings,
       hashes, URLs, functions, and blocks}
 @item{derived @litchar|{@media}| query structures}
 @item{derived @litchar|{@supports}| condition structures}]

This layered approach keeps the parser reusable: consumers can stay close to
the raw source when they need fidelity, or opt into richer interpretation when
they need convenience.

@section{Limitations}

Current limitations worth knowing up front:

@itemlist[
 @item{This is not a CSS engine or full semantic validator.}
 @item{Some rewrite helpers still work at the raw-text level for selectors or
       preludes, because there is not yet a full selector serializer.}
 @item{Source-preserving rewrites exist for targeted declaration/block edits,
       not for every normalized transform.}
 @item{Nesting helpers operate on nested AST structure; they do not magically
       infer arbitrary future syntax beyond what the parser has represented.}
 @item{The manual’s cross-reference polish is still improving, even though the
       API coverage is broad.}]

@section{Cookbook}

Some practical starting points:

@itemlist[
 @item{@bold{Rename a class}: use @racket[css-rename-class].}
 @item{@bold{Scope a stylesheet}: use @racket[css-prefix-selectors].}
 @item{@bold{Rename a custom property}: use
       @racket[css-rename-custom-property].}
 @item{@bold{Rewrite URLs}: use @racket[css-rewrite-url-values].}
 @item{@bold{Wrap matching rules in @litchar|{@media}|}: use
       @racket[css-wrap-rules-in-media].}
 @item{@bold{Split grouped selectors}: use
       @racket[css-split-grouped-selectors].}
 @item{@bold{Remove duplicate declarations}: use
       @racket[css-dedupe-declarations].}
 @item{@bold{Inspect selector pseudos}: use @racket[css-style-rule-selectors]
       together with @racket[css-find-rules-by-pseudo].}]

@subsection{Rename A Class}

This example renames @tt{.card} to @tt{.tile}.

@examples[#:eval css-eval
(define rename-class-input
  ".card:hover, .card .title { color: red; }")
(define rename-class-output
  (serialize-stylesheet
   (css-rename-class
    (parse-css rename-class-input)
    "card"
    "tile")))
rename-class-input
rename-class-output
]

@subsection{Scope A Stylesheet}

This example prefixes every selector with @tt{.scope}.

@examples[#:eval css-eval
(define scope-input
  "body { color: red; }")
(define scope-output
  (serialize-stylesheet
   (css-prefix-selectors
    (parse-css scope-input)
    ".scope")))
scope-input
scope-output
]

@subsection{Rename A Custom Property}

This example renames @tt{--brand} to @tt{--accent} in both declaration names
and @tt{var(...)} references.

@examples[#:eval css-eval
(define custom-property-input
  ":root { --brand: red; color: var(--brand); }")
(define custom-property-output
  (serialize-stylesheet
   (css-rename-custom-property
    (parse-css custom-property-input)
    "--brand"
    "--accent")))
custom-property-input
custom-property-output
]

@subsection{Rewrite URLs}

This example rewrites both declaration and @litchar|{@import}| URLs.

@examples[#:eval css-eval
(define rewrite-url-input
  "body { background: url(\"a.png\"); }\n@import url(\"b.css\") screen;")
(define rewrite-url-output
  (serialize-stylesheet
   (css-rewrite-url-values
    (parse-css rewrite-url-input)
    (lambda (inner)
      (cond
        [(equal? inner "\"a.png\"") "\"c.png\""]
        [(equal? inner "\"b.css\"") "\"d.css\""]
        [else inner])))))
rewrite-url-input
rewrite-url-output
]

@subsection{Wrap Matching Rules In @litchar|{@media}|}

This example wraps the @tt{body} rule in a new @litchar|{@media}| block.

@examples[#:eval css-eval
(define wrap-media-input
  "body { color: red; }")
(define wrap-media-output
  (serialize-stylesheet
   (css-wrap-rules-in-media
    (parse-css wrap-media-input)
    "body"
    "screen")))
wrap-media-input
wrap-media-output
]

@subsection{Split Grouped Selectors}

This example splits one grouped rule into two separate rules.

@examples[#:eval css-eval
(define split-selectors-input
  ".a, .b { background: rgb(1 2 3); }")
(define split-selectors-output
  (serialize-stylesheet
   (css-split-grouped-selectors
    (parse-css split-selectors-input))))
split-selectors-input
split-selectors-output
]

@subsection{Remove Duplicate Declarations}

This example keeps the last duplicate declaration in the rule.

@examples[#:eval css-eval
(define dedupe-input
  "body { color: red; color: blue; margin: 0; }")
(define dedupe-output
  (serialize-stylesheet
   (css-dedupe-declarations
    (parse-css dedupe-input))))
dedupe-input
dedupe-output
]

@subsection{Inspect Selector Pseudos}

This example finds rules that use the pseudo selector @tt{:not}.

@examples[#:eval css-eval
(define pseudo-input
  "a:not(.x, #y) > span:nth-child(2n+1) { color: red; }")
(define pseudo-stylesheet
  (parse-css pseudo-input))
(define pseudo-rules
  (css-find-rules-by-pseudo pseudo-stylesheet "not"))
pseudo-input
(length pseudo-rules)
(map css-style-rule-selector-groups pseudo-rules)
]

@section{Reference}

The remainder of this manual is the API reference, grouped by task rather than
by source file.

@subsection{Parsing and Serialization}

@defthing[current-css-standard symbol?]{
The standard tag used by @racketmodname[parsers/css]. The scaffold currently
uses @racket['current].}

@defproc[(make-css-parser [#:standard standard symbol? current-css-standard])
         (input-port? . -> . any/c)]{
Constructs a port-based CSS parser.

The result is a procedure of one argument, an input port. The intended use is
to create the parser and apply it to a port containing a complete stylesheet.

The current scaffold has a real parser driver and structural reader. Empty
input, simple style rules, grouped selectors, and basic declaration values
currently parse successfully, while broader modern CSS coverage is still under
construction.

@examples[#:eval css-eval
(css-parser? (make-css-parser))
]}

@defproc[(parse-css [source (or/c string? input-port?)])
         any/c]{
Parses CSS from a string or input port.

This is the convenience entry point for most consumers. It accepts either a
complete stylesheet string or an input port and uses the current CSS standard
target.

The current scaffold already parses a useful first slice of CSS, including
simple style rules, grouped selectors, declarations, and the outer structure
of common at-rules such as @litchar|{@media}|, @litchar|{@supports}|,
@litchar|{@import}|, @litchar|{@font-face}|, and @litchar|{@keyframes}|.

@examples[#:eval css-eval
(define stylesheet
  (parse-css "body { color: red; }"))
(css-stylesheet? stylesheet)
]}

@defproc[(parse-stylesheet [source (or/c string? input-port?)])
         any/c]{
Alias for @racket[parse-css].}

@defproc[(serialize-stylesheet [stylesheet css-stylesheet?]
                               [#:preserve-source? preserve-source? boolean? #f])
         string?]{
Serializes a stylesheet AST back to CSS text.

When @racket[preserve-source?] is true and the stylesheet still carries its
original source string, the serializer returns that original source. Otherwise
it produces normalized output from the AST.

@examples[#:eval css-eval
(serialize-stylesheet (parse-css "body { color: red; }") #:preserve-source? #t)
]}

@defproc[(serialize-stylesheet/normalized [stylesheet css-stylesheet?])
         string?]{
Serializes a stylesheet AST using normalized spacing regardless of any
preserved source text.

Comments, declarations, style rules, and the currently supported at-rules are
preserved structurally.

@examples[#:eval css-eval
(serialize-stylesheet/normalized (parse-css "body { color: red; }"))
]}

@defproc[(serialize-css [stylesheet css-stylesheet?])
         string?]{
Alias for @racket[serialize-stylesheet].}

@subsection{Rewrite Reference}

@defproc[(css-map-declarations [stylesheet css-stylesheet?]
                               [proc procedure?])
         css-stylesheet?]{
Rewrites declarations throughout a stylesheet.

The procedure receives each @racket[css-declaration?] node and should return
either a replacement declaration or @racket[#f] to remove it. The returned
stylesheet clears its preserved source string, since the original source is no
longer an exact representation of the modified AST.}

@defproc[(css-map-rules [stylesheet css-stylesheet?]
                        [proc procedure?])
         css-stylesheet?]{
Rewrites each @racket[css-style-rule?] in the stylesheet. The procedure may
return one replacement rule, a list of replacement rules, or @racket[#f] to
remove the rule.}

@defproc[(css-map-at-rules [stylesheet css-stylesheet?]
                           [proc procedure?])
         css-stylesheet?]{
Rewrites each @racket[css-at-rule?] in the stylesheet. The procedure may
return one replacement at-rule, a list of replacement at-rules, or @racket[#f]
to remove the rule.}

@defproc[(css-map-selectors [stylesheet css-stylesheet?]
                            [proc procedure?])
         css-stylesheet?]{
Rewrites each selector-group string in every style rule. The procedure receives
one selector-group string and must return a replacement string.}

@defproc[(css-map-declarations-in-selectors [stylesheet css-stylesheet?]
                                            [selector-group string?]
                                            [proc procedure?])
         css-stylesheet?]{
Rewrites declarations only inside style rules whose selector groups include
@racket[selector-group] exactly.}

@defproc[(css-update-declaration-values [stylesheet css-stylesheet?]
                                        [name string?]
                                        [updater procedure?])
         css-stylesheet?]{
Updates declaration values for property names that match
@racket[name] case-insensitively. The updater is called with the raw
declaration value string and should return a replacement string.}

@defproc[(css-update-declaration-values/preserve-source [stylesheet css-stylesheet?]
                                                        [name string?]
                                                        [updater procedure?])
         css-stylesheet?]{
Updates declaration values like @racket[css-update-declaration-values], but
preserves untouched source text when the stylesheet still has original source
and the targeted declarations have source spans.

The touched declaration text is rewritten in place and the result is reparsed,
so unchanged formatting and comments elsewhere remain intact.}

@defproc[(css-remove-declarations [stylesheet css-stylesheet?]
                                  [name string?])
         css-stylesheet?]{
Removes declarations whose property name matches @racket[name]
case-insensitively.}

@defproc[(css-remove-declarations/preserve-source [stylesheet css-stylesheet?]
                                                  [name string?])
         css-stylesheet?]{
Removes declarations like @racket[css-remove-declarations], but preserves
untouched source text when possible by editing the original source string in
place and reparsing it.}

@defproc[(css-append-declaration [stylesheet css-stylesheet?]
                                 [selector-group string?]
                                 [name string?]
                                 [value string?]
                                 [#:important? important? boolean? #f])
         css-stylesheet?]{
Appends a declaration to each style rule whose selector groups include
@racket[selector-group] exactly.}

@defproc[(css-append-declaration/preserve-source [stylesheet css-stylesheet?]
                                                 [selector-group string?]
                                                 [name string?]
                                                 [value string?]
                                                 [#:important? important? boolean? #f])
         css-stylesheet?]{
Appends a declaration like @racket[css-append-declaration], but preserves
untouched source text when the stylesheet still has original source and the
matched style rules have source spans.

The new declaration text is inserted directly into each matched rule block and
the result is reparsed, so unchanged formatting and comments elsewhere remain
intact.}

@defproc[(css-append-declaration-by-pseudo [stylesheet css-stylesheet?]
                                           [pseudo-name string?]
                                           [name string?]
                                           [value string?]
                                           [#:important? important? boolean? #f])
         css-stylesheet?]{
Appends a declaration to each style rule whose derived selector structure uses
the pseudo selector named by @racket[pseudo-name].}

@defproc[(css-append-declaration-by-pseudo/preserve-source [stylesheet css-stylesheet?]
                                                           [pseudo-name string?]
                                                           [name string?]
                                                           [value string?]
                                                           [#:important? important? boolean? #f])
         css-stylesheet?]{
Appends a declaration like @racket[css-append-declaration-by-pseudo], but
preserves untouched source text when possible by inserting the new declaration
directly into each matched rule block and reparsing the edited source.}

@defproc[(css-append-declaration-by-class [stylesheet css-stylesheet?]
                                          [class-name string?]
                                          [name string?]
                                          [value string?]
                                          [#:important? important? boolean? #f])
         css-stylesheet?]{
Appends a declaration to each style rule whose derived selector structure uses
the class selector named by @racket[class-name].}

@defproc[(css-append-declaration-by-class/preserve-source [stylesheet css-stylesheet?]
                                                          [class-name string?]
                                                          [name string?]
                                                          [value string?]
                                                          [#:important? important? boolean? #f])
         css-stylesheet?]{
Appends a declaration like @racket[css-append-declaration-by-class], but
preserves untouched source text when possible by inserting the new declaration
directly into each matched rule block and reparsing the edited source.}

@defproc[(css-append-declaration-by-attribute [stylesheet css-stylesheet?]
                                              [attribute-name string?]
                                              [name string?]
                                              [value string?]
                                              [#:important? important? boolean? #f])
         css-stylesheet?]{
Appends a declaration to each style rule whose derived selector structure uses
an attribute selector named by @racket[attribute-name].}

@defproc[(css-append-declaration-by-attribute/preserve-source [stylesheet css-stylesheet?]
                                                              [attribute-name string?]
                                                              [name string?]
                                                              [value string?]
                                                              [#:important? important? boolean? #f])
         css-stylesheet?]{
Appends a declaration like @racket[css-append-declaration-by-attribute], but
preserves untouched source text when possible by inserting the new declaration
directly into each matched rule block and reparsing the edited source.}

@defproc[(css-rename-class [stylesheet css-stylesheet?]
                           [old-name string?]
                           [new-name string?])
         css-stylesheet?]{
Renames one class selector throughout the stylesheet.}

@defproc[(css-prefix-selectors [stylesheet css-stylesheet?]
                               [prefix string?])
         css-stylesheet?]{
Prefixes each selector group with @racket[prefix].}

@defproc[(css-rewrite-media-queries [stylesheet css-stylesheet?]
                                    [proc procedure?])
         css-stylesheet?]{
Rewrites @litchar|{@media}| preludes. The procedure receives the raw prelude
string and its derived details, and must return a replacement prelude string.}

@defproc[(css-rewrite-supports-conditions [stylesheet css-stylesheet?]
                                          [proc procedure?])
         css-stylesheet?]{
Rewrites @litchar|{@supports}| preludes. The procedure receives the raw
prelude string and its derived details, and must return a replacement prelude
string.}

@defproc[(css-rewrite-custom-properties [stylesheet css-stylesheet?]
                                        [proc procedure?])
         css-stylesheet?]{
Rewrites custom-property names in both declaration names and @tt{var(...)}
references. The procedure receives one custom-property name such as
@racket["--brand"] and must return a replacement name.}

@defproc[(css-split-grouped-selectors [stylesheet css-stylesheet?])
         css-stylesheet?]{
Splits grouped selector rules into separate style rules, one per selector
group.}

@defproc[(css-clone-rule [stylesheet css-stylesheet?]
                         [selector-group string?]
                         [#:transform proc procedure? values])
         css-stylesheet?]{
Clones each style rule whose selector groups include @racket[selector-group].
The optional transform procedure receives the matched rule and must return the
clone to insert immediately after it.}

@defproc[(css-insert-rule-before [stylesheet css-stylesheet?]
                                 [selector-group string?]
                                 [new-rule css-style-rule?])
         css-stylesheet?]{
Inserts @racket[new-rule] before each style rule whose selector groups include
@racket[selector-group].}

@defproc[(css-insert-rule-after [stylesheet css-stylesheet?]
                                [selector-group string?]
                                [new-rule css-style-rule?])
         css-stylesheet?]{
Inserts @racket[new-rule] after each style rule whose selector groups include
@racket[selector-group].}

@defproc[(css-remove-rules [stylesheet css-stylesheet?]
                           [pred? procedure?])
         css-stylesheet?]{
Removes style rules for which @racket[pred?] returns true.}

@defproc[(css-remove-at-rules [stylesheet css-stylesheet?]
                              [pred? procedure?])
         css-stylesheet?]{
Removes at-rules for which @racket[pred?] returns true.}

@defproc[(css-wrap-rules-in-media [stylesheet css-stylesheet?]
                                  [selector-group string?]
                                  [prelude string?])
         css-stylesheet?]{
Wraps matching style rules in a new @litchar|{@media}| rule.}

@defproc[(css-wrap-rules-in-supports [stylesheet css-stylesheet?]
                                     [selector-group string?]
                                     [prelude string?])
         css-stylesheet?]{
Wraps matching style rules in a new @litchar|{@supports}| rule.}

@defproc[(css-merge-adjacent-rules [stylesheet css-stylesheet?])
         css-stylesheet?]{
Merges adjacent compatible style rules and adjacent compatible at-rules.}

@defproc[(css-dedupe-declarations [stylesheet css-stylesheet?]
                                  [#:keep keep (or/c 'first 'last) 'last])
         css-stylesheet?]{
Removes duplicate declarations within each style rule, keeping either the
first or last occurrence.}

@defproc[(css-sort-declarations [stylesheet css-stylesheet?]
                                [#:less-than less-than procedure? string<?])
         css-stylesheet?]{
Sorts declarations within each style rule using the given comparator on
declaration names.}

@defproc[(css-rename-custom-property [stylesheet css-stylesheet?]
                                     [old-name string?]
                                     [new-name string?])
         css-stylesheet?]{
Renames one custom property in both declaration names and @tt{var(...)}
references.}

@defproc[(css-rewrite-var-functions [stylesheet css-stylesheet?]
                                    [proc procedure?])
         css-stylesheet?]{
Rewrites custom-property names only in @tt{var(...)} references.}

@defproc[(css-rename-keyframes [stylesheet css-stylesheet?]
                               [old-name string?]
                               [new-name string?])
         css-stylesheet?]{
Renames one @litchar|{@keyframes}| identifier and matching animation
references.}

@defproc[(css-rewrite-imports [stylesheet css-stylesheet?]
                              [proc procedure?])
         css-stylesheet?]{
Rewrites @litchar|{@import}| preludes using a callback over the raw prelude
string.}

@defproc[(css-rewrite-font-face [stylesheet css-stylesheet?]
                                [proc procedure?])
         css-stylesheet?]{
Rewrites declarations inside @litchar|{@font-face}| blocks.}

@defproc[(css-rewrite-url-values [stylesheet css-stylesheet?]
                                 [proc procedure?])
         css-stylesheet?]{
Rewrites @tt{url(...)} inner text in declarations and at-rule preludes.}

@defproc[(css-filter-comments [stylesheet css-stylesheet?]
                              [pred? procedure?])
         css-stylesheet?]{
Keeps only comments for which @racket[pred?] returns true.}

@defproc[(css-hoist-nested-rules [stylesheet css-stylesheet?])
         css-stylesheet?]{
Hoists nested style rules into flat rule lists by combining selectors.}

@defproc[(css-lower-nesting [stylesheet css-stylesheet?])
         css-stylesheet?]{
Lowers nesting by hoisting nested rules into flat rule lists.}

@defproc[(css-rewrite-attribute-selectors [stylesheet css-stylesheet?]
                                          [proc procedure?])
         css-stylesheet?]{
Rewrites raw attribute-selector text inside selector groups.}

@defproc[(css-rewrite-pseudos [stylesheet css-stylesheet?]
                              [proc procedure?])
         css-stylesheet?]{
Rewrites raw pseudo-selector text inside selector groups.}

@defproc[(css-rewrite-selector-structure [stylesheet css-stylesheet?]
                                         [proc procedure?])
         css-stylesheet?]{
Rewrites selector groups using both the raw selector-group string and the
derived selector structure for that group.}

@defproc[(css-update-declaration-values-in-media-feature [stylesheet css-stylesheet?]
                                                         [feature-name string?]
                                                         [property-name string?]
                                                         [updater procedure?])
         css-stylesheet?]{
Updates declaration values only inside @litchar|{@media}| rules whose derived
feature set includes @racket[feature-name].}

@defproc[(css-update-declaration-values-in-media-feature/preserve-source [stylesheet css-stylesheet?]
                                                                         [feature-name string?]
                                                                         [property-name string?]
                                                                         [updater procedure?])
         css-stylesheet?]{
Updates declaration values like @racket[css-update-declaration-values-in-media-feature],
but preserves untouched source text when possible by editing the original source
string in place and reparsing it.}

@defproc[(css-remove-declarations-in-media-feature [stylesheet css-stylesheet?]
                                                   [feature-name string?]
                                                   [property-name string?])
         css-stylesheet?]{
Removes declarations only inside @litchar|{@media}| rules whose derived
feature set includes @racket[feature-name].}

@defproc[(css-remove-declarations-in-media-feature/preserve-source [stylesheet css-stylesheet?]
                                                                   [feature-name string?]
                                                                   [property-name string?])
         css-stylesheet?]{
Removes declarations like @racket[css-remove-declarations-in-media-feature],
but preserves untouched source text when possible by editing the original
source string in place and reparsing it.}

@defproc[(css-update-declaration-values-in-supports-feature [stylesheet css-stylesheet?]
                                                            [feature-name string?]
                                                            [property-name string?]
                                                            [updater procedure?])
         css-stylesheet?]{
Updates declaration values only inside @litchar|{@supports}| rules whose
derived feature tests include @racket[feature-name].}

@defproc[(css-update-declaration-values-in-supports-feature/preserve-source [stylesheet css-stylesheet?]
                                                                            [feature-name string?]
                                                                            [property-name string?]
                                                                            [updater procedure?])
         css-stylesheet?]{
Updates declaration values like @racket[css-update-declaration-values-in-supports-feature],
but preserves untouched source text when possible by editing the original
source string in place and reparsing it.}

@defproc[(css-remove-declarations-in-supports-feature [stylesheet css-stylesheet?]
                                                      [feature-name string?]
                                                      [property-name string?])
         css-stylesheet?]{
Removes declarations only inside @litchar|{@supports}| rules whose derived
feature tests include @racket[feature-name].}

@defproc[(css-remove-declarations-in-supports-feature/preserve-source [stylesheet css-stylesheet?]
                                                                      [feature-name string?]
                                                                      [property-name string?])
         css-stylesheet?]{
Removes declarations like @racket[css-remove-declarations-in-supports-feature],
but preserves untouched source text when possible by editing the original
source string in place and reparsing it.}

@subsection{Core AST And Derived Reference}

The parser is intended to return explicit AST nodes instead of ad hoc maps or
lists.

The first scaffolded AST forms are:

@itemlist[
 @item{@racket[css-stylesheet?] for a complete stylesheet node.}
 @item{@racket[css-comment?] for preserved comments.}
 @item{@racket[css-recovery?] for recovered malformed fragments.}
 @item{@racket[css-style-rule?] for a style rule node.}
 @item{@racket[css-at-rule?] for an at-rule node.}
 @item{@racket[css-declaration?] for a declaration node.}
 @item{@racket[css-source-span?] for preserved source span data.}
 @item{@racket[css-qualified-rule?] for a qualified rule node.}]

As the parser grows, additional node types will cover declarations, at-rules,
comments, nested rule contents, and source locations.

@defproc[(css-stylesheet? [v any/c]) boolean?]{
Recognizes stylesheet AST nodes.}

@defproc[(css-stylesheet-rules [stylesheet css-stylesheet?])
         list?]{
Returns the stylesheet’s rule list.}

@defproc[(css-stylesheet-source [stylesheet css-stylesheet?])
         (or/c string? #f)]{
Returns the original source associated with the stylesheet when available.}

@defproc[(css-stylesheet-span [stylesheet css-stylesheet?])
         (or/c css-source-span? #f)]{
Returns the stylesheet source span when available.}

@defproc[(css-source-span? [v any/c]) boolean?]{
Recognizes source span values.}

@defproc[(css-source-span-start [span css-source-span?])
         any/c]{
Returns the start component of a source span.}

@defproc[(css-source-span-end [span css-source-span?])
         any/c]{
Returns the end component of a source span.}

@defproc[(css-comment? [v any/c]) boolean?]{
Recognizes comment AST nodes.}

@defproc[(css-comment-text [comment css-comment?])
         string?]{
Returns the raw comment text.}

@defproc[(css-comment-span [comment css-comment?])
         (or/c css-source-span? #f)]{
Returns the source span for a comment when available.}

@defproc[(css-recovery? [v any/c]) boolean?]{
Recognizes recovery AST nodes emitted when the parser skips a malformed
fragment but continues parsing the surrounding stylesheet.}

@defproc[(css-recovery-kind [recovery css-recovery?])
         symbol?]{
Returns the recovery kind, such as @racket['statement] or
@racket['declaration].}

@defproc[(css-recovery-reason [recovery css-recovery?])
         string?]{
Returns the parse error message that triggered recovery.}

@defproc[(css-recovery-text [recovery css-recovery?])
         string?]{
Returns the raw source text that was skipped during recovery.}

@defproc[(css-recovery-span [recovery css-recovery?])
         (or/c css-source-span? #f)]{
Returns the source span for the skipped fragment when available.}

@defproc[(css-recovery-detail [recovery css-recovery?])
         any/c]{
Returns parser-specific recovery detail, typically derived from the underlying
parse error.}

@defproc[(css-style-rule? [v any/c]) boolean?]{
Recognizes style-rule AST nodes.}

@defproc[(css-style-rule-selector-groups [rule css-style-rule?])
         list?]{
Returns the selector-group representation for a style rule.}

@defproc[(css-style-rule-selectors [rule css-style-rule?])
         list?]{
Returns selector nodes derived from the rule’s selector groups.}

@defproc[(css-selector? [v any/c]) boolean?]{
Recognizes selector nodes.}

@defproc[(css-selector-text [selector css-selector?])
         string?]{
Returns the selector text.}

@defproc[(css-selector-span [selector css-selector?])
         (or/c css-source-span? #f)]{
Returns the selector span when available.}

@defproc[(css-selector-parts [selector css-selector?])
         list?]{
Returns the derived selector parts for one selector group.}

@defproc[(css-selector-compounds [selector css-selector?])
         list?]{
Returns compound-selector groupings derived from one selector group.}

@defproc[(css-selector-compound? [v any/c]) boolean?]{
Recognizes compound selector nodes.}

@defproc[(css-selector-combinator? [v any/c]) boolean?]{
Recognizes selector combinator nodes.}

@defproc[(css-selector-type? [v any/c]) boolean?]{
Recognizes type selector nodes.}

@defproc[(css-selector-namespaced-type? [v any/c]) boolean?]{
Recognizes namespace-qualified type selector nodes such as @tt{svg|rect}.}

@defproc[(css-selector-class? [v any/c]) boolean?]{
Recognizes class selector nodes.}

@defproc[(css-selector-id? [v any/c]) boolean?]{
Recognizes id selector nodes.}

@defproc[(css-selector-attribute? [v any/c]) boolean?]{
Recognizes attribute selector nodes.}

@defproc[(css-selector-attribute-derived-details [attribute css-selector-attribute?])
         css-selector-attribute-details?]{
Returns a derived structured view of an attribute selector.

This is the preferred accessor when you want namespace-aware attribute names or
typed attribute values instead of manually interpreting the raw string fields.}

@defproc[(css-selector-attribute-details? [v any/c]) boolean?]{
Recognizes derived attribute-detail nodes.}

@defproc[(css-selector-attribute-details-namespace [details css-selector-attribute-details?])
         (or/c string? #f)]{
Returns the optional namespace prefix for an attribute selector, such as
@tt{foo} in @tt{[foo|href=button]}.}

@defproc[(css-selector-attribute-details-name [details css-selector-attribute-details?])
         string?]{
Returns the local attribute name.}

@defproc[(css-selector-attribute-details-value [details css-selector-attribute-details?])
         (or/c css-selector-attribute-identifier-value?
               css-selector-attribute-string-value?
               #f)]{
Returns the typed attribute value when one is present.}

@defproc[(css-selector-attribute-identifier-value? [v any/c]) boolean?]{
Recognizes identifier-valued attribute selectors such as @tt{[href=button]}.}

@defproc[(css-selector-attribute-string-value? [v any/c]) boolean?]{
Recognizes string-valued attribute selectors such as @tt{[href=\"button\"]}.}

@defproc[(css-selector-pseudo? [v any/c]) boolean?]{
Recognizes pseudo-class and pseudo-element selector nodes.}

For selector-like functional pseudos such as @tt{:not(...)}, @tt{:is(...)},
@tt{:where(...)}, and @tt{:has(...)}, the pseudo arguments are exposed as
derived selector nodes. For other functional pseudos, the arguments remain
component-value nodes.

@defproc[(css-selector-pseudo-arguments [pseudo css-selector-pseudo?])
         list?]{
Returns the backward-compatible flat argument list for a pseudo selector.

For selector-list pseudos this is a list of @racket[css-selector?] values. For
value-oriented pseudos this is a list of component-value nodes.}

@defproc[(css-selector-pseudo-argument-structure [pseudo css-selector-pseudo?])
         (or/c css-selector-pseudo-selector-list?
               css-selector-pseudo-value-list?
               css-selector-pseudo-nth-arguments?
               #f)]{
Returns the explicit pseudo-argument wrapper when one is available.

This is the preferred way to distinguish selector-list pseudos from
value-oriented pseudos without inspecting the raw argument list by hand.}

@defproc[(css-selector-pseudo-selector-list? [v any/c]) boolean?]{
Recognizes explicit selector-list pseudo arguments.}

@defproc[(css-selector-pseudo-value-list? [v any/c]) boolean?]{
Recognizes explicit component-value pseudo arguments.}

@defproc[(css-selector-pseudo-nth-arguments? [v any/c]) boolean?]{
Recognizes explicit @tt{nth-*} pseudo arguments, including an optional
@tt{of} selector clause.}

@defproc[(css-selector-pseudo-selector-list-selectors [args css-selector-pseudo-selector-list?])
         list?]{
Returns the parsed selector arguments for a selector-list pseudo.}

@defproc[(css-selector-pseudo-value-list-values [args css-selector-pseudo-value-list?])
         list?]{
Returns the parsed component-value arguments for a value-oriented pseudo.}

For the @tt{nth-*} family, @tt{an+b} arguments such as @tt{2n+1},
@tt{odd}, and @tt{-n+6} are exposed as typed
@racket[css-component-an-plus-b?] nodes through
@racket[css-selector-pseudo-nth-arguments?].

@defproc[(css-selector-pseudo-nth-arguments-formula [args css-selector-pseudo-nth-arguments?])
         list?]{
Returns the parsed @tt{an+b} formula portion for an @tt{nth-*} pseudo.}

@defproc[(css-selector-pseudo-nth-arguments-selectors [args css-selector-pseudo-nth-arguments?])
         list?]{
Returns the parsed selector list from an optional @tt{of} clause.

For example, @tt{:nth-child(2n+1 of .item, #main)} exposes @tt{.item} and
@tt{#main} here.}

@defproc[(css-selector-pseudo-identifier-list? [v any/c]) boolean?]{
Recognizes identifier-like pseudo arguments such as those used by
@tt{:lang(...)} and @tt{:dir(...)}.}

@defproc[(css-selector-pseudo-identifier? [v any/c]) boolean?]{
Recognizes one identifier-like pseudo argument.}

@defproc[(css-selector-pseudo-identifier-value [v css-selector-pseudo-identifier?])
         string?]{
Returns the identifier-like pseudo argument text, such as @tt{en-US} or
@tt{rtl}.}

@defproc[(css-selector-universal? [v any/c]) boolean?]{
Recognizes universal selector nodes.}

@defproc[(css-selector-namespaced-universal? [v any/c]) boolean?]{
Recognizes namespace-qualified universal selector nodes such as @tt{*|*} or
@tt{foo|*}.}

@defproc[(css-style-rule-block [rule css-style-rule?])
         any/c]{
Returns the rule block.}

@defproc[(css-style-rule-raw-selector [rule css-style-rule?])
         any/c]{
Returns the raw selector text preserved by the style rule scaffold.}

@defproc[(css-style-rule-span [rule css-style-rule?])
         (or/c css-source-span? #f)]{
Returns the source span for a style rule when available.}

@defproc[(css-at-rule? [v any/c]) boolean?]{
Recognizes at-rule AST nodes.}

@defproc[(css-at-rule-name [rule css-at-rule?])
         any/c]{
Returns the at-rule name, such as @racket["@media"].}

@defproc[(css-at-rule-prelude [rule css-at-rule?])
         any/c]{
Returns the at-rule prelude representation.}

@defproc[(css-at-rule-prelude-values [rule css-at-rule?])
         list?]{
Returns a lightweight component-value view of the at-rule prelude.}

@defproc[(css-at-rule-prelude-derived-details [rule css-at-rule?])
         any/c]{
Returns a richer derived prelude view for recognized at-rules.

Currently this provides structured results for @litchar|{@media}| and
@litchar|{@supports}|; other at-rules fall back to the lightweight component-value
list.}

@defproc[(css-media-prelude-details? [v any/c]) boolean?]{
Recognizes derived @litchar|{@media}| prelude nodes.}

@defproc[(css-media-query? [v any/c]) boolean?]{
Recognizes one derived media query entry.}

@defproc[(css-media-feature? [v any/c]) boolean?]{
Recognizes one derived media feature fragment such as
@tt{(width >= 40rem)}.}

@defproc[(css-media-feature-expression? [v any/c]) boolean?]{
Recognizes a typed media feature expression such as
@tt{(width >= 40rem)} or @tt{(prefers-color-scheme: dark)}.}

@defproc[(css-media-feature-expression-name [feature css-media-feature-expression?])
         string?]{
Returns the media feature name.}

@defproc[(css-media-feature-expression-operator [feature css-media-feature-expression?])
         string?]{
Returns the comparison operator, such as @tt{:} or @tt{>=}.}

@defproc[(css-media-feature-expression-value [feature css-media-feature-expression?])
         string?]{
Returns the raw media feature value text.}

@defproc[(css-media-feature-range? [v any/c]) boolean?]{
Recognizes a typed chained media range such as
@tt{(20rem <= width <= 60rem)}.}

@defproc[(css-media-feature-range-name [feature css-media-feature-range?])
         string?]{
Returns the media feature name for a chained range.}

@defproc[(css-media-feature-range-lower [feature css-media-feature-range?])
         string?]{
Returns the lower bound text for a chained range.}

@defproc[(css-media-feature-range-upper [feature css-media-feature-range?])
         string?]{
Returns the upper bound text for a chained range.}

@defproc[(css-supports-prelude-details? [v any/c]) boolean?]{
Recognizes derived @litchar|{@supports}| prelude nodes.}

@defproc[(css-supports-condition? [v any/c]) boolean?]{
Recognizes one derived supports-condition node.

The current condition kinds include @racket['feature], @racket['not],
@racket['and], @racket['or], and @racket['unknown].}

@defproc[(css-supports-feature? [v any/c]) boolean?]{
Recognizes one typed supports feature test such as
@tt{(display: grid)}.}

@defproc[(css-supports-feature-name [feature css-supports-feature?])
         string?]{
Returns the feature-test name, such as @tt{display}.}

@defproc[(css-supports-feature-value [feature css-supports-feature?])
         string?]{
Returns the raw feature-test value text, such as @tt{grid}.}

@defproc[(css-at-rule-block [rule css-at-rule?])
         any/c]{
Returns the at-rule body or block representation.}

@defproc[(css-at-rule-span [rule css-at-rule?])
         (or/c css-source-span? #f)]{
Returns the source span for an at-rule when available.}

@defproc[(css-declaration? [v any/c]) boolean?]{
Recognizes declaration AST nodes.}

@defproc[(css-declaration-name [declaration css-declaration?])
         any/c]{
Returns the declaration property name.}

@defproc[(css-declaration-value [declaration css-declaration?])
         any/c]{
Returns the declaration value representation.}

@defproc[(css-declaration-component-values [declaration css-declaration?])
         list?]{
Returns a lightweight component-value view of the declaration value.}

@defproc[(css-declaration-important? [declaration css-declaration?])
         boolean?]{
Reports whether the declaration is marked important.}

@defproc[(css-declaration-span [declaration css-declaration?])
         (or/c css-source-span? #f)]{
Returns the source span for a declaration when available.}

@defproc[(css-qualified-rule? [v any/c]) boolean?]{
Recognizes qualified-rule AST nodes.}

@defproc[(css-qualified-rule-prelude [rule css-qualified-rule?])
         any/c]{
Returns the rule prelude. For style rules, this will eventually correspond to
the selector portion before the block.}

@defproc[(css-qualified-rule-block [rule css-qualified-rule?])
         any/c]{
Returns the block part of a qualified rule.}

@defproc[(css-component-token? [v any/c]) boolean?]{
Recognizes simple component token nodes.}

@defproc[(css-component-an-plus-b? [v any/c]) boolean?]{
Recognizes parsed @tt{an+b} component nodes used by the @tt{nth-*} pseudos.}

@defproc[(css-component-an-plus-b-a [v css-component-an-plus-b?]) integer?]{
Returns the @tt{a} coefficient from an @tt{an+b} node.}

@defproc[(css-component-an-plus-b-b [v css-component-an-plus-b?]) integer?]{
Returns the @tt{b} offset from an @tt{an+b} node.}

@defproc[(css-component-number? [v any/c]) boolean?]{
Recognizes numeric component nodes.}

@defproc[(css-component-percentage? [v any/c]) boolean?]{
Recognizes percentage component nodes.}

@defproc[(css-component-dimension? [v any/c]) boolean?]{
Recognizes dimension component nodes such as @tt{10px}.}

@defproc[(css-component-string? [v any/c]) boolean?]{
Recognizes string component nodes.}

@defproc[(css-component-hash? [v any/c]) boolean?]{
Recognizes hash component nodes such as @tt{#fff}.}

@defproc[(css-component-url? [v any/c]) boolean?]{
Recognizes @tt{url(...)} component nodes.}

@defproc[(css-component-function? [v any/c]) boolean?]{
Recognizes function component nodes.}

@defproc[(css-component-block? [v any/c]) boolean?]{
Recognizes simple block component nodes.}

@subsection{Query And Recovery Reference}

@defproc[(css-flatten-rules [stylesheet css-stylesheet?])
         list?]{
Returns a pre-order list of rules and at-rules, recursively flattening nested
rule-bearing at-rules while skipping comments.}

@defproc[(css-find-rules-by-selector-group [stylesheet css-stylesheet?]
                                           [selector-group string?])
         list?]{
Finds style rules whose selector groups include @racket[selector-group]
exactly.

The search preserves source order and flattens nested rule-bearing at-rules the
same way as @racket[css-flatten-rules].}

@defproc[(css-find-rules-by-raw-selector [stylesheet css-stylesheet?]
                                         [raw-selector string?])
         list?]{
Finds style rules whose raw selector text exactly matches
@racket[raw-selector].

The search preserves source order and uses the same nested-rule flattening as
@racket[css-flatten-rules].}

@defproc[(css-find-declarations-in-selector-group
          [stylesheet css-stylesheet?]
          [selector-group string?]
          [property-name (or/c string? #f) #f])
         list?]{
Finds declarations in rules whose selector groups include
@racket[selector-group] exactly.

The result preserves source order. When @racket[property-name] is provided, the
result is filtered case-insensitively by property name.}

@defproc[(css-find-declarations-in-selector-groups
          [stylesheet css-stylesheet?]
          [selector-groups (listof string?)]
          [property-name (or/c string? #f) #f])
         list?]{
Finds declarations in rules whose selector groups include any string from
@racket[selector-groups] exactly.

The result preserves source order and flattens nested rule-bearing at-rules the
same way as @racket[css-flatten-rules]. Each matching rule contributes its
declarations at most once, even if it matches more than one requested selector
group. When @racket[property-name] is provided, the result is filtered
case-insensitively by property name.}

@defproc[(css-collect-custom-properties-in-selector-group
          [stylesheet css-stylesheet?]
          [selector-group string?])
         hash?]{
Collects custom-property declarations from rules whose selector groups include
@racket[selector-group] exactly.

Declarations are processed in source order, and later declarations override
earlier ones in the returned hash.}

@defproc[(css-collect-custom-properties-in-selector-groups
          [stylesheet css-stylesheet?]
          [selector-groups (listof string?)])
         hash?]{
Collects custom-property declarations from rules whose selector groups include
any string from @racket[selector-groups] exactly.

Declarations are processed in source order, and later declarations override
earlier ones in the returned hash. Nested rule-bearing at-rules are flattened
the same way as @racket[css-flatten-rules], and each matching rule is processed
at most once even if it matches more than one requested selector group.}

@defproc[(css-find-declarations [stylesheet css-stylesheet?]
                                [name string?])
         list?]{
Finds declarations whose property name matches @racket[name]
case-insensitively.}

@defproc[(css-query-selector [stylesheet css-stylesheet?]
                             [selector string?])
         list?]{
Finds style rules whose selector groups include @racket[selector].}

@defproc[(css-find-rules-by-pseudo [stylesheet css-stylesheet?]
                                   [pseudo-name string?])
         list?]{
Finds style rules whose derived selector structure contains a pseudo selector
with the given name.}

@defproc[(css-find-media-queries [stylesheet css-stylesheet?])
         list?]{
Returns the derived @racket[css-media-query?] nodes collected from
@litchar|{@media}| rules in the stylesheet.}

@defproc[(css-find-supports-features [stylesheet css-stylesheet?]
                                     [name (or/c string? #f) #f])
         list?]{
Returns the typed @racket[css-supports-feature?] leaves collected from
@litchar|{@supports}| rules.

When @racket[name] is provided, the result is filtered case-insensitively by
feature name.}

@defproc[(css-recovery-nodes [stylesheet css-stylesheet?])
         list?]{
Returns all recovery nodes in the stylesheet.}

@defproc[(css-has-recovery? [stylesheet css-stylesheet?])
         boolean?]{
Reports whether the stylesheet contains any recovery nodes.}

@defproc[(css-recovery-summary [stylesheet css-stylesheet?])
         list?]{
Summarizes recovery nodes by kind.}

@subsection{Error Reference}

The parser scaffold uses a CSS-specific exception type rather than generic
contract or read errors for parser failures.

@defproc[(exn:fail:css? [v any/c]) boolean?]{
Recognizes CSS parser exceptions raised by the scaffold.}

@defproc[(exn:fail:css-source [e exn:fail:css?])
         any/c]{
Returns the source value attached to a CSS parser exception.}

@defproc[(exn:fail:css-detail [e exn:fail:css?])
         any/c]{
Returns parser-specific detail attached to a CSS parser exception.}

@subsection{Parser Procedure Reference}

@defproc[(css-parser? [v any/c]) boolean?]{
Recognizes parser procedures created by @racket[make-css-parser].
}
