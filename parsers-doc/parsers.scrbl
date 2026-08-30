#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     (only-in parsers/css
                              current-css-standard
                              parse-css
                              parse-stylesheet)
                     parsers/lua
                     parsers/toml
                     (except-in parsers/private/css-ast
                                css-at-rule
                                css-comment
                                css-declaration
                                css-qualified-rule
                                css-recovery
                                css-source-span
                                css-style-rule
                                css-stylesheet)
                     parsers/private/css-compute
                     parsers/private/css-errors
                     parsers/private/css-parser
                     parsers/private/css-query
                     parsers/private/css-recovery
                     parsers/private/css-rewrite
                     parsers/private/css-serialize
                     parsers/private/css-standard
                     parsers/private/css-structure))

@(define css-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parsers/css))
     the-eval))

@(define toml-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parsers/toml))
     the-eval))

@(define lua-eval
   (let ([the-eval (make-base-eval)])
     (the-eval '(require racket/base
                         parsers/lua))
     the-eval))

@title{Parsers}
@author+email["Jens Axel Søgaard" "jensaxel@soegaard.net"]

@italic{Note:}
The @tt{parsers} library and documentation were written with the help of Codex.

The @tt{parsers} package is a collection of reusable parsers.
This release provides CSS, TOML, and Lua parsers. The CSS library also
includes tools for inspection and rewriting.

The manual is organized by language. Future parsers will get their own
chapters.

@local-table-of-contents[]

@section{CSS}

@defmodule[parsers/css
           #:use-sources (parsers/private/css-ast
                          parsers/private/css-compute
                          parsers/private/css-errors
                          parsers/private/css-parser
                          parsers/private/css-query
                          parsers/private/css-recovery
                          parsers/private/css-rewrite
                          parsers/private/css-serialize
                          parsers/private/css-standard
                          parsers/private/css-structure)]

CSS is the stylesheet language used to describe the presentation of
HTML and other structured documents, including layout, colors,
typography, and responsive styling.

This CSS parser and rewrite library is intended for tooling-oriented use cases
such as selector inspection, declaration lookup, cascade-oriented analysis,
source-preserving edits, and later higher-level transforms.

@subsection{Overview}

The public CSS entry point is @racketmodname[parsers/css]. It is intended to
track the modern CSS standard over time. If fixed compatibility targets become
useful later, they can be added as separate module paths.

The CSS library is built around five layers:

@itemlist[
 @item{@bold{Parsing}: raw CSS source becomes a stylesheet AST.}
 @item{@bold{Derived views}: selectors, values, and some at-rule preludes can
       be inspected through richer helper APIs without changing the underlying
       AST.}
 @item{@bold{Queries}: common lookups such as “find declarations”, “find rules
       by pseudo”, or “find supports features”.}
 @item{@bold{Reduced computed style}: exact-target winner selection and limited
       shorthand expansion for tooling that needs final values without a
       browser engine.}
 @item{@bold{Rewrites}: normalized AST rewrites plus a smaller
       source-preserving rewrite layer for targeted declaration/block edits.}]

The parser is intentionally @italic{not} a browser engine, layout engine, or
full CSS semantic validator. It parses structure faithfully enough for tooling
and rewriting, and keeps semantic interpretation layered on top.


@subsection{Quick Start}

Install the package with @tt{raco pkg install parsers}, then require the CSS
module:

@racketblock[
(require parsers/css)]

For most users, the workflow is:

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
(map css-declaration-value
     (css-find-declarations-in-selector-group stylesheet ".card" "color"))
(serialize-stylesheet
 (css-rename-class stylesheet "card" "tile"))
]

To parse a file, pass an input port:

@racketblock[
(define stylesheet
  (call-with-input-file "site.css" parse-css))]

Malformed input is recorded with recovery nodes when the parser can continue:

@examples[#:eval css-eval
(define recovered
  (parse-css ".ok { color: red; }\n.bad { color }\n.next { color: blue; }"))
(css-has-recovery? recovered)
(length (css-recovery-nodes recovered))
]

@subsection{Cookbook}

This section is intended to give a taste of what can be done
with the parsed stylesheet. See the full reference later for
details.

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

@subsubsection{Rename A Class}

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

@subsubsection{Scope A Stylesheet}

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

@subsubsection{Rename A Custom Property}

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

@subsubsection{Rewrite URLs}

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

@subsubsection{Wrap Matching Rules In @litchar|{@media}|}

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

@subsubsection{Split Grouped Selectors}

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

@subsubsection{Remove Duplicate Declarations}

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

@subsubsection{Inspect Selector Pseudos}

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



@subsection{Core Model}

The parser returns a small explicit AST:

@itemlist[
 @item{@racket[css-stylesheet?]  for a full stylesheet}
 @item{@racket[css-style-rule?]  for ordinary style rules}
 @item{@racket[css-at-rule?]     for at-rules such as @litchar|{@media}| and @litchar|{@supports}|}
 @item{@racket[css-declaration?] for declarations}
 @item{@racket[css-comment?]     for preserved comments}
 @item{@racket[css-recovery?]    for malformed fragments the parser skipped but recorded}
 @item{@racket[css-source-span?] for source locations when available}]

The AST is intentionally simpler than a browser’s internal model. Raw selector
text, declaration values, source order, comments, and recovery information are
preserved first; richer interpretation is exposed through helper APIs rather
than forced into the base tree.

The public module exports predicates and accessors for AST values returned by
the parser. It does not export the core AST constructors as the primary editing
interface. For edits, prefer the rewrite helpers; when a helper asks for a
replacement rule node, a common pattern is to parse a small CSS snippet and
extract the rule from the resulting stylesheet.

@examples[#:eval css-eval
(define replacement-rule
  (car (css-stylesheet-rules
        (parse-css ".notice { color: blue; }"))))
(css-style-rule? replacement-rule)
]

@subsection{Parsing}

The parser currently handles a substantial structural subset of modern CSS,
including style rules, grouped selectors, declarations, comments, and common
at-rules such as @litchar|{@media}|, @litchar|{@supports}|,
@litchar|{@import}|, @litchar|{@font-face}|, and @litchar|{@keyframes}|.

Internally it uses:

@itemlist[
 @item{@tt{lexers/css} as the tokenizer source}
 @item{a handwritten structural reader for rules, at-rules, blocks, and
       declarations}
 @item{derived selector/value/media/supports helpers layered on top of the raw
       AST}]

Malformed input is handled with recovery nodes where possible, so tooling can
keep working on imperfect stylesheets instead of failing hard on the first
error.

As a release target, the parser aims to preserve useful structure rather than
prove that every value is semantically valid CSS:

@itemlist[
 @item{@bold{Generally supported}: stylesheets, style rules, grouped
       selectors, declarations, comments, common rule-bearing at-rules,
       @litchar|{@import}|, @litchar|{@font-face}|, and @litchar|{@keyframes}|.}
 @item{@bold{Derived support}: selector parts, component values, selected
       @litchar|{@media}| preludes, and selected @litchar|{@supports}|
       conditions.}
 @item{@bold{Recovered}: malformed statements and declarations when the parser
       can skip the fragment and continue.}
 @item{@bold{Out of scope}: browser validation, DOM matching, layout,
       inheritance, media-environment simulation, and framework-specific
       behavior.}]

@subsection{Serialization}

There are two main serialization modes:

@itemlist[
 @item{@bold{normalized}: serialize the AST with consistent spacing}
 @item{@bold{source-preserving}: when the stylesheet still carries original
       source text and the operation did not invalidate it, return that
       original text instead}]

Most normalized AST rewrites clear the preserved source string intentionally.
The smaller source-preserving rewrite family edits source slices directly and
then reparses the result.

@subsection{Query Helpers}

Query helpers sit above the raw AST and derived structures. They are intended
for common tooling tasks such as:

@itemlist[
 @item{iterating rules in source order}
 @item{finding declarations by property}
 @item{matching exact selector groups or exact raw selector text}
 @item{querying selectors or pseudos}
 @item{computing reduced exact-target style and custom-property environments}
 @item{collecting derived @litchar|{@media}| and @litchar|{@supports}| information}
 @item{inspecting parser recovery output}]

@subsubsection{Choosing Helpers}

The helper families intentionally sit at different levels:

@itemlist[
 @item{Use @racket[css-find-rules-by-selector-group] when you know the exact
       selector group text you want, such as @racket[".btn"] or
       @racket[".dropdown-menu .dropdown-item"].}
 @item{Use @racket[css-find-rules-by-raw-selector] when the whole selector
       prelude must match exactly, including grouped selector text.}
 @item{Use @racket[css-query-selector] or @racket[css-find-rules-by-pseudo]
       when derived selector structure is more useful than raw text.}
 @item{Use @racket[css-collect-custom-properties-in-selector-group] for a
       source-order custom-property collector with later declarations
       overriding earlier ones.}
 @item{Use @racket[css-compute-custom-properties-for-selector-group] when you
       need exact-target winner selection with importance, specificity, source
       order, and optional @tt{var(...)} resolution.}
 @item{Use @racket[css-compute-style-for-selector-group] when you need final
       standard-property values for one exact selector-group target, including
       the limited shorthand expansion documented below.}
 @item{Use rewrite helpers when you want a new stylesheet AST or a targeted
       source-preserving edit rather than just inspection results.}]

@subsubsection{Reduced Computed Style}

The library also includes a small computed-style layer for tooling use cases.
It is deliberately narrow:

@itemlist[
 @item{exact selector-group matching only}
 @item{cascade winner selection by @tt{!important}, specificity, and source order}
 @item{limited shorthand expansion for @tt{border}, @tt{border-top},
       @tt{border-right}, @tt{border-bottom}, @tt{border-left},
       @tt{padding}, and @tt{margin}}
 @item{optional @tt{var(...)} resolution against computed custom properties and
       caller-supplied defaults}
 @item{optional trace output so downstream tools can inspect why a value won}]

This is useful for stylesheet inspection tools, but it is @bold{not} a browser
engine. In particular, it does not do general selector matching, inheritance,
DOM simulation, media-environment evaluation, or layout.

Exact matching means that @tt{.btn} and @tt{.btn:hover} are different
selector-group targets. Nested rule-bearing at-rules such as
@litchar|{@media}| are included by structural flattening, but their conditions
are not evaluated.

@examples[#:eval css-eval
(define exact-style
  (parse-css
   (string-append
    ".btn:hover { color: red; }\n"
    ".btn { color: blue; }\n"
    "@media screen { .btn { color: green; } }")))
(length (css-find-rules-by-selector-group exact-style ".btn"))
(length (css-find-rules-by-selector-group exact-style ".btn:hover"))
(hash-ref (css-compute-style-for-selector-group exact-style ".btn")
          "color"
          #f)
]

Custom properties can be returned as their own exact-target environment, and
standard properties can optionally resolve @tt{var(...)} references through
that environment and caller-supplied defaults.

@examples[#:eval css-eval
(define computed-style
  (parse-css
   (string-append
    ".btn { --accent: steelblue; color: var(--accent); padding: 1px 2px; }\n"
    ".fallback { color: var(--missing); }")))
(hash-ref (css-compute-custom-properties-for-selector-group
           computed-style
           ".btn"
           #:resolve-vars? #t)
          "--accent"
          #f)
(hash-ref (css-compute-style-for-selector-group
           computed-style
           ".btn"
           #:resolve-vars? #t)
          "color"
          #f)
(hash-ref (css-compute-style-for-selector-group
           computed-style
           ".btn")
          "padding-left"
          #f)
(hash-ref (css-compute-style-for-selector-group
           computed-style
           ".fallback"
           #:resolve-vars? #t
           #:defaults (hash "--missing" "royalblue"))
          "color"
          #f)
]

When @racket[#:trace?] is true, the computed-style helpers return two values:
the computed hash and an inspectable @racket[css-compute-style-trace?]
payload.

@examples[#:eval css-eval
(define-values (style trace)
  (css-compute-style-for-selector-group
   computed-style
   ".btn"
   #:resolve-vars? #t
   #:trace? #t))
(css-compute-style-trace? trace)
(length (css-compute-style-trace-matched-rules trace))
(hash-ref style "color" #f)
]

@subsection{Rewrite Helpers}

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

@subsection{Derived Structures}

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

@subsection{Limitations}

Current limitations worth knowing up front:

@itemlist[
 @item{This is not a CSS engine or full semantic validator.}
 @item{Some rewrite helpers still work at the raw-text level for selectors or
       preludes, because there is not yet a full selector serializer.}
 @item{Source-preserving rewrites exist for targeted declaration/block edits,
       not for every normalized transform.}
 @item{Nesting helpers operate on nested AST structure; they do not magically
       infer arbitrary future syntax beyond what the parser has represented.}
 @item{The computed-style helpers use an exact selector-group model; they do
       not match selectors against a DOM tree.}]


@subsection{Reference}

The remainder of this chapter is the API reference.

@subsubsection{Parsing and Serialization}

@defthing[current-css-standard symbol?]{
The standard tag used by @racketmodname[parsers/css]. The current public
parser target is @racket['current].}

@defproc[(make-css-parser [#:standard standard symbol? current-css-standard])
         (input-port? . -> . css-stylesheet?)]{
Constructs a port-based CSS parser.

The result is a procedure of one argument, an input port. The intended use is
to create the parser and apply it to a port containing a complete stylesheet.

The parser handles stylesheets with style rules, grouped selectors,
declarations, comments, recovery nodes, and common at-rules.

@examples[#:eval css-eval
(css-parser? (make-css-parser))
]}

@defproc[(parse-css [source (or/c string? input-port?)])
         css-stylesheet?]{
Parses CSS from a string or input port.

This is the convenience entry point for most consumers. It accepts either a
complete stylesheet string or an input port and uses the current CSS standard
target.

The parser supports style rules, grouped selectors, declarations, comments,
recovery nodes for malformed fragments, and the outer structure of common
at-rules such as @litchar|{@media}|, @litchar|{@supports}|,
@litchar|{@import}|, @litchar|{@font-face}|, and @litchar|{@keyframes}|.

@examples[#:eval css-eval
(define stylesheet
  (parse-css "body { color: red; }"))
(css-stylesheet? stylesheet)
]}

@defproc[(parse-stylesheet [source (or/c string? input-port?)])
         css-stylesheet?]{
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

@subsubsection{Rewrite Reference}

@defproc[(css-map-declarations [stylesheet css-stylesheet?]
                               [proc (-> css-declaration?
                                         (or/c css-declaration? #f))])
         css-stylesheet?]{
Rewrites declarations throughout a stylesheet.

The procedure receives each @racket[css-declaration?] node and should return
either a replacement declaration or @racket[#f] to remove it. The returned
stylesheet clears its preserved source string, since the original source is no
longer an exact representation of the modified AST.}

@defproc[(css-map-rules [stylesheet css-stylesheet?]
                        [proc (-> css-style-rule?
                                  (or/c css-style-rule?
                                        (listof css-style-rule?)
                                        #f))])
         css-stylesheet?]{
Rewrites each @racket[css-style-rule?] in the stylesheet. The procedure may
return one replacement rule, a list of replacement rules, or @racket[#f] to
remove the rule.}

@defproc[(css-map-at-rules [stylesheet css-stylesheet?]
                           [proc (-> css-at-rule?
                                     (or/c css-at-rule?
                                           (listof css-at-rule?)
                                           #f))])
         css-stylesheet?]{
Rewrites each @racket[css-at-rule?] in the stylesheet. The procedure may
return one replacement at-rule, a list of replacement at-rules, or @racket[#f]
to remove the rule.}

@defproc[(css-map-selectors [stylesheet css-stylesheet?]
                            [proc (-> string? string?)])
         css-stylesheet?]{
Rewrites each selector-group string in every style rule. The procedure receives
one selector-group string and must return a replacement string.}

@defproc[(css-map-declarations-in-selectors [stylesheet css-stylesheet?]
                                            [selector-group string?]
                                            [proc (-> css-declaration?
                                                      (or/c css-declaration? #f))])
         css-stylesheet?]{
Rewrites declarations only inside style rules whose selector groups include
@racket[selector-group] exactly.}

@defproc[(css-update-declaration-values [stylesheet css-stylesheet?]
                                        [name string?]
                                        [updater (-> string? string?)])
         css-stylesheet?]{
Updates declaration values for property names that match
@racket[name] case-insensitively. The updater is called with the raw
declaration value string and should return a replacement string.}

@defproc[(css-update-declaration-values/preserve-source [stylesheet css-stylesheet?]
                                                        [name string?]
                                                        [updater (-> string? string?)])
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
                                    [proc (-> string?
                                              css-media-prelude-details?
                                              string?)])
         css-stylesheet?]{
Rewrites @litchar|{@media}| preludes. The procedure receives the raw prelude
string and its derived details, and must return a replacement prelude string.}

@defproc[(css-rewrite-supports-conditions [stylesheet css-stylesheet?]
                                          [proc (-> string?
                                                    css-supports-prelude-details?
                                                    string?)])
         css-stylesheet?]{
Rewrites @litchar|{@supports}| preludes. The procedure receives the raw
prelude string and its derived details, and must return a replacement prelude
string.}

@defproc[(css-rewrite-custom-properties [stylesheet css-stylesheet?]
                                        [proc (-> string? string?)])
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
                         [#:transform proc (-> css-style-rule? css-style-rule?) values])
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
                           [pred? (-> css-style-rule? boolean?)])
         css-stylesheet?]{
Removes style rules for which @racket[pred?] returns true.}

@defproc[(css-remove-at-rules [stylesheet css-stylesheet?]
                              [pred? (-> css-at-rule? boolean?)])
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
                                [#:less-than less-than (-> string? string? boolean?) string<?])
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
                                    [proc (-> string? string?)])
         css-stylesheet?]{
Rewrites custom-property names only in @tt{var(...)} references.}

@defproc[(css-rename-keyframes [stylesheet css-stylesheet?]
                               [old-name string?]
                               [new-name string?])
         css-stylesheet?]{
Renames one @litchar|{@keyframes}| identifier and matching animation
references.}

@defproc[(css-rewrite-imports [stylesheet css-stylesheet?]
                              [proc (-> string? string?)])
         css-stylesheet?]{
Rewrites @litchar|{@import}| preludes using a callback over the raw prelude
string.}

@defproc[(css-rewrite-font-face [stylesheet css-stylesheet?]
                                [proc (-> css-declaration?
                                          (or/c css-declaration? #f))])
         css-stylesheet?]{
Rewrites declarations inside @litchar|{@font-face}| blocks.}

@defproc[(css-rewrite-url-values [stylesheet css-stylesheet?]
                                 [proc (-> string? string?)])
         css-stylesheet?]{
Rewrites @tt{url(...)} inner text in declarations and at-rule preludes.}

@defproc[(css-filter-comments [stylesheet css-stylesheet?]
                              [pred? (-> css-comment? boolean?)])
         css-stylesheet?]{
Keeps only comments for which @racket[pred?] returns true.}

@defproc[(css-hoist-nested-rules [stylesheet css-stylesheet?])
         css-stylesheet?]{
Hoists nested style rules into flat rule lists by combining selectors.}

@defproc[(css-lower-nesting [stylesheet css-stylesheet?])
         css-stylesheet?]{
Lowers nesting by hoisting nested rules into flat rule lists.}

@defproc[(css-rewrite-attribute-selectors [stylesheet css-stylesheet?]
                                          [proc (-> string? string?)])
         css-stylesheet?]{
Rewrites raw attribute-selector text inside selector groups.}

@defproc[(css-rewrite-pseudos [stylesheet css-stylesheet?]
                              [proc (-> string? string?)])
         css-stylesheet?]{
Rewrites raw pseudo-selector text inside selector groups.}

@defproc[(css-rewrite-selector-structure [stylesheet css-stylesheet?]
                                         [proc (-> string? css-selector? string?)])
         css-stylesheet?]{
Rewrites selector groups using both the raw selector-group string and the
derived selector structure for that group.}

@defproc[(css-update-declaration-values-in-media-feature [stylesheet css-stylesheet?]
                                                         [feature-name string?]
                                                         [property-name string?]
                                                         [updater (-> string? string?)])
         css-stylesheet?]{
Updates declaration values only inside @litchar|{@media}| rules whose derived
feature set includes @racket[feature-name].}

@defproc[(css-update-declaration-values-in-media-feature/preserve-source [stylesheet css-stylesheet?]
                                                                         [feature-name string?]
                                                                         [property-name string?]
                                                                         [updater (-> string? string?)])
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
                                                            [updater (-> string? string?)])
         css-stylesheet?]{
Updates declaration values only inside @litchar|{@supports}| rules whose
derived feature tests include @racket[feature-name].}

@defproc[(css-update-declaration-values-in-supports-feature/preserve-source [stylesheet css-stylesheet?]
                                                                            [feature-name string?]
                                                                            [property-name string?]
                                                                            [updater (-> string? string?)])
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

@subsubsection{Core AST And Derived Reference}

The parser is intended to return explicit AST nodes instead of ad hoc maps or
lists.

The core AST forms are:

@itemlist[
 @item{@racket[css-stylesheet?] for a complete stylesheet node.}
 @item{@racket[css-comment?] for preserved comments.}
 @item{@racket[css-recovery?] for recovered malformed fragments.}
 @item{@racket[css-style-rule?] for a style rule node.}
 @item{@racket[css-at-rule?] for an at-rule node.}
 @item{@racket[css-declaration?] for a declaration node.}
 @item{@racket[css-source-span?] for preserved source span data.}
 @item{@racket[css-qualified-rule?] for a qualified rule node.}]

Derived helper APIs provide richer selector, component-value, media-query, and
supports-condition structures when consumers need more detail.

@defstruct*[#:link-target? #f
            css-stylesheet
            ([rules (listof (or/c css-style-rule?
                                   css-at-rule?
                                   css-comment?
                                   css-recovery?))]
             [source (or/c string? #f)]
             [span (or/c css-source-span? #f)])
            #:transparent
            #:omit-constructor]{
Represents a complete stylesheet.

The @racket[rules] field contains the top-level stylesheet nodes in source
order. The @racket[source] field contains the original stylesheet text when it
is available; source-preserving serializers and rewrites use this value. The
@racket[span] field records the source extent of the stylesheet when available.}

@defstruct*[#:link-target? #f
            css-style-rule
            ([selector-groups (listof string?)]
             [block (listof (or/c css-declaration?
                                   css-comment?
                                   css-recovery?
                                   css-style-rule?
                                   css-at-rule?))]
             [raw-selector string?]
             [span (or/c css-source-span? #f)])
            #:transparent
            #:omit-constructor]{
Represents an ordinary CSS style rule.

The @racket[selector-groups] field contains the comma-separated selector groups
as exact source-text strings, with surrounding selector whitespace trimmed. The
@racket[block] field contains the rule body in source order: declarations,
comments, recovery nodes, and any nested rule-bearing nodes the parser
represented structurally. The @racket[raw-selector] field preserves the full
selector prelude text before the block. The @racket[span] field records the
rule source extent when available.}

@defstruct*[#:link-target? #f
            css-at-rule
            ([name string?]
             [prelude string?]
             [block (or/c (listof (or/c css-style-rule?
                                         css-at-rule?
                                         css-declaration?
                                         css-comment?
                                         css-recovery?))
                          #f)]
             [span (or/c css-source-span? #f)])
            #:transparent
            #:omit-constructor]{
Represents an at-rule such as @litchar|{@media}|, @litchar|{@supports}|,
@litchar|{@import}|, @litchar|{@font-face}|, or @litchar|{@keyframes}|.

The @racket[name] field contains the at-keyword, including the leading
@litchar|{@}|. The @racket[prelude] field contains the raw prelude text between
the at-rule name and the terminating semicolon or block. The @racket[block]
field contains the at-rule body in source order, or @racket[#f] for at-rules
without a block. The @racket[span] field records the at-rule source extent when
available.}

@defstruct*[#:link-target? #f
            css-declaration
            ([name string?]
             [value string?]
             [important? boolean?]
             [span (or/c css-source-span? #f)])
            #:transparent
            #:omit-constructor]{
Represents a CSS declaration.

The @racket[name] field contains the property name exactly as parsed. The
@racket[value] field contains the raw declaration value text, excluding the
property name, colon, semicolon, and trailing @tt{!important} marker. The
@racket[important?] field records whether the declaration was marked
@tt{!important}. The @racket[span] field records the declaration source extent
when available.}

@defstruct*[#:link-target? #f
            css-comment
            ([text string?]
             [span (or/c css-source-span? #f)])
            #:transparent
            #:omit-constructor]{
Represents a preserved CSS comment.

The @racket[text] field contains the raw comment text, including the
@tt{/* ... */} delimiters. The @racket[span] field records the comment source
extent when available.}

@defstruct*[#:link-target? #f
            css-recovery
            ([kind symbol?]
             [reason string?]
             [text string?]
             [span (or/c css-source-span? #f)]
             [detail any/c])
            #:transparent
            #:omit-constructor]{
Represents a malformed source fragment that the parser skipped while recovering
and continuing with the surrounding stylesheet.

The @racket[kind] field classifies the skipped fragment, for example
@racket['statement] or @racket['declaration]. The @racket[reason] field
contains a human-readable parse error message. The @racket[text] field
contains the raw skipped source text. The @racket[span] field records the
skipped source extent when available. The @racket[detail] field contains
parser-specific diagnostic data for tools that want more context.}

@defstruct*[#:link-target? #f
            css-source-span
            ([start any/c]
             [end any/c])
            #:transparent
            #:omit-constructor]{
Represents a source extent.

The @racket[start] and @racket[end] fields mark the beginning and end of a
source range. In parsed stylesheets these are parser-tools position values;
some tests and manually constructed ASTs use exact nonnegative offsets. Treat
the values as source-location data rather than CSS syntax.}

@defstruct*[#:link-target? #f
            css-qualified-rule
            ([prelude list?]
             [block list?])
            #:transparent
            #:omit-constructor]{
Represents a generic qualified rule.

The @racket[prelude] field contains the component-value prelude before the
block. The @racket[block] field contains the rule body representation. Most
tooling should prefer the more specific @tt{css-style-rule} struct when
working with ordinary style rules.}

@defproc[(css-stylesheet? [v any/c]) boolean?]{
Recognizes stylesheet AST nodes.}

@defproc[(css-stylesheet-rules [stylesheet css-stylesheet?])
         (listof (or/c css-style-rule?
                       css-at-rule?
                       css-comment?
                       css-recovery?))]{
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
Returns the start source-location value of a source span. Parsed stylesheets
use parser-tools position values; manually constructed spans may use exact
nonnegative offsets.}

@defproc[(css-source-span-end [span css-source-span?])
         any/c]{
Returns the end source-location value of a source span. Parsed stylesheets use
parser-tools position values; manually constructed spans may use exact
nonnegative offsets.}

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
         (listof string?)]{
Returns the selector-group representation for a style rule.}

@defproc[(css-style-rule-selectors [rule css-style-rule?])
         (listof css-selector?)]{
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
         (listof (or/c css-selector-type?
                       css-selector-namespaced-type?
                       css-selector-class?
                       css-selector-id?
                       css-selector-attribute?
                       css-selector-pseudo?
                       css-selector-universal?
                       css-selector-namespaced-universal?
                       css-selector-combinator?))]{
Returns the derived selector parts for one selector group.}

@defproc[(css-selector-compounds [selector css-selector?])
         (listof (or/c css-selector-compound?
                       css-selector-combinator?))]{
Returns compound-selector groupings derived from one selector group.}

@defproc[(css-selector-compound? [v any/c]) boolean?]{
Recognizes compound selector nodes.}

@defproc[(css-selector-compound-items [compound css-selector-compound?])
         list?]{
Returns the selector parts in a compound selector.}

@defproc[(css-selector-compound-span [compound css-selector-compound?])
         (or/c css-source-span? #f)]{
Returns the source span for a compound selector when available.}

@defproc[(css-selector-combinator? [v any/c]) boolean?]{
Recognizes selector combinator nodes.}

@defproc[(css-selector-combinator-text [combinator css-selector-combinator?])
         string?]{
Returns the combinator text, such as @tt{>}, @tt{+}, @tt{~}, or a space.}

@defproc[(css-selector-combinator-span [combinator css-selector-combinator?])
         (or/c css-source-span? #f)]{
Returns the source span for a combinator when available.}

@defproc[(css-selector-type? [v any/c]) boolean?]{
Recognizes type selector nodes.}

@defproc[(css-selector-type-name [selector css-selector-type?])
         string?]{
Returns the type selector name.}

@defproc[(css-selector-type-span [selector css-selector-type?])
         (or/c css-source-span? #f)]{
Returns the source span for a type selector when available.}

@defproc[(css-selector-namespaced-type? [v any/c]) boolean?]{
Recognizes namespace-qualified type selector nodes such as @tt{svg|rect}.}

@defproc[(css-selector-namespaced-type-namespace
          [selector css-selector-namespaced-type?])
         string?]{
Returns the namespace prefix for a namespace-qualified type selector.}

@defproc[(css-selector-namespaced-type-name
          [selector css-selector-namespaced-type?])
         string?]{
Returns the local type name for a namespace-qualified type selector.}

@defproc[(css-selector-namespaced-type-span
          [selector css-selector-namespaced-type?])
         (or/c css-source-span? #f)]{
Returns the source span for a namespace-qualified type selector when
available.}

@defproc[(css-selector-class? [v any/c]) boolean?]{
Recognizes class selector nodes.}

@defproc[(css-selector-class-name [selector css-selector-class?])
         string?]{
Returns the class name without the leading dot.}

@defproc[(css-selector-class-span [selector css-selector-class?])
         (or/c css-source-span? #f)]{
Returns the source span for a class selector when available.}

@defproc[(css-selector-id? [v any/c]) boolean?]{
Recognizes id selector nodes.}

@defproc[(css-selector-id-name [selector css-selector-id?])
         string?]{
Returns the ID name without the leading hash.}

@defproc[(css-selector-id-span [selector css-selector-id?])
         (or/c css-source-span? #f)]{
Returns the source span for an ID selector when available.}

@defproc[(css-selector-attribute? [v any/c]) boolean?]{
Recognizes attribute selector nodes.}

@defproc[(css-selector-attribute-name [attribute css-selector-attribute?])
         string?]{
Returns the raw attribute name text.}

@defproc[(css-selector-attribute-matcher [attribute css-selector-attribute?])
         (or/c string? #f)]{
Returns the attribute matcher, such as @tt{=}, @tt{~=}, or @tt{^=}, when one
is present.}

@defproc[(css-selector-attribute-value [attribute css-selector-attribute?])
         (or/c string? #f)]{
Returns the raw attribute value text when one is present.}

@defproc[(css-selector-attribute-modifier [attribute css-selector-attribute?])
         (or/c string? #f)]{
Returns the attribute selector modifier, such as @tt{i} or @tt{s}, when one is
present.}

@defproc[(css-selector-attribute-text [attribute css-selector-attribute?])
         string?]{
Returns the full raw attribute selector text.}

@defproc[(css-selector-attribute-span [attribute css-selector-attribute?])
         (or/c css-source-span? #f)]{
Returns the source span for an attribute selector when available.}

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

@defproc[(css-selector-attribute-details-matcher [details css-selector-attribute-details?])
         (or/c string? #f)]{
Returns the attribute matcher when one is present.}

@defproc[(css-selector-attribute-details-value [details css-selector-attribute-details?])
         (or/c css-selector-attribute-identifier-value?
               css-selector-attribute-string-value?
               #f)]{
Returns the typed attribute value when one is present.}

@defproc[(css-selector-attribute-details-modifier [details css-selector-attribute-details?])
         (or/c string? #f)]{
Returns the attribute selector modifier when one is present.}

@defproc[(css-selector-attribute-details-text [details css-selector-attribute-details?])
         string?]{
Returns the full raw attribute selector text.}

@defproc[(css-selector-attribute-details-span [details css-selector-attribute-details?])
         (or/c css-source-span? #f)]{
Returns the source span for derived attribute details when available.}

@defproc[(css-selector-attribute-identifier-value? [v any/c]) boolean?]{
Recognizes identifier-valued attribute selectors such as @tt{[href=button]}.}

@defproc[(css-selector-attribute-identifier-value-text
          [value css-selector-attribute-identifier-value?])
         string?]{
Returns the raw identifier value text.}

@defproc[(css-selector-attribute-identifier-value-value
          [value css-selector-attribute-identifier-value?])
         string?]{
Returns the decoded identifier value.}

@defproc[(css-selector-attribute-identifier-value-span
          [value css-selector-attribute-identifier-value?])
         (or/c css-source-span? #f)]{
Returns the source span for an identifier attribute value when available.}

@defproc[(css-selector-attribute-string-value? [v any/c]) boolean?]{
Recognizes string-valued attribute selectors such as @tt{[href=\"button\"]}.}

@defproc[(css-selector-attribute-string-value-text
          [value css-selector-attribute-string-value?])
         string?]{
Returns the raw string value text, including quotes.}

@defproc[(css-selector-attribute-string-value-value
          [value css-selector-attribute-string-value?])
         string?]{
Returns the decoded string value.}

@defproc[(css-selector-attribute-string-value-span
          [value css-selector-attribute-string-value?])
         (or/c css-source-span? #f)]{
Returns the source span for a string attribute value when available.}

@defproc[(css-selector-pseudo? [v any/c]) boolean?]{
Recognizes pseudo-class and pseudo-element selector nodes.}

@defproc[(css-selector-pseudo-name [pseudo css-selector-pseudo?])
         string?]{
Returns the pseudo selector name without leading colon characters.}

@defproc[(css-selector-pseudo-element? [pseudo css-selector-pseudo?])
         boolean?]{
Reports whether the pseudo selector was written as a pseudo-element.}

@defproc[(css-selector-pseudo-text [pseudo css-selector-pseudo?])
         string?]{
Returns the full raw pseudo selector text.}

@defproc[(css-selector-pseudo-span [pseudo css-selector-pseudo?])
         (or/c css-source-span? #f)]{
Returns the source span for a pseudo selector when available.}

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

@defproc[(css-selector-pseudo-selector-list-text [args css-selector-pseudo-selector-list?])
         string?]{
Returns the raw selector-list argument text.}

@defproc[(css-selector-pseudo-selector-list-span [args css-selector-pseudo-selector-list?])
         (or/c css-source-span? #f)]{
Returns the source span for selector-list pseudo arguments when available.}

@defproc[(css-selector-pseudo-value-list-values [args css-selector-pseudo-value-list?])
         list?]{
Returns the parsed component-value arguments for a value-oriented pseudo.}

@defproc[(css-selector-pseudo-value-list-text [args css-selector-pseudo-value-list?])
         string?]{
Returns the raw component-value argument text.}

@defproc[(css-selector-pseudo-value-list-span [args css-selector-pseudo-value-list?])
         (or/c css-source-span? #f)]{
Returns the source span for value-list pseudo arguments when available.}

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

@defproc[(css-selector-pseudo-nth-arguments-text [args css-selector-pseudo-nth-arguments?])
         string?]{
Returns the raw @tt{nth-*} argument text.}

@defproc[(css-selector-pseudo-nth-arguments-span [args css-selector-pseudo-nth-arguments?])
         (or/c css-source-span? #f)]{
Returns the source span for @tt{nth-*} arguments when available.}

@defproc[(css-selector-pseudo-identifier-list? [v any/c]) boolean?]{
Recognizes identifier-like pseudo arguments such as those used by
@tt{:lang(...)} and @tt{:dir(...)}.}

@defproc[(css-selector-pseudo-identifier-list-values
          [args css-selector-pseudo-identifier-list?])
         (listof css-selector-pseudo-identifier?)]{
Returns the identifier argument nodes.}

@defproc[(css-selector-pseudo-identifier-list-text
          [args css-selector-pseudo-identifier-list?])
         string?]{
Returns the raw identifier-list argument text.}

@defproc[(css-selector-pseudo-identifier-list-span
          [args css-selector-pseudo-identifier-list?])
         (or/c css-source-span? #f)]{
Returns the source span for identifier-list pseudo arguments when available.}

@defproc[(css-selector-pseudo-identifier? [v any/c]) boolean?]{
Recognizes one identifier-like pseudo argument.}

@defproc[(css-selector-pseudo-identifier-text [v css-selector-pseudo-identifier?])
         string?]{
Returns the raw identifier argument text.}

@defproc[(css-selector-pseudo-identifier-value [v css-selector-pseudo-identifier?])
         string?]{
Returns the identifier-like pseudo argument text, such as @tt{en-US} or
@tt{rtl}.}

@defproc[(css-selector-pseudo-identifier-span [v css-selector-pseudo-identifier?])
         (or/c css-source-span? #f)]{
Returns the source span for an identifier pseudo argument when available.}

@defproc[(css-selector-universal? [v any/c]) boolean?]{
Recognizes universal selector nodes.}

@defproc[(css-selector-universal-text [selector css-selector-universal?])
         string?]{
Returns the raw universal selector text.}

@defproc[(css-selector-universal-span [selector css-selector-universal?])
         (or/c css-source-span? #f)]{
Returns the source span for a universal selector when available.}

@defproc[(css-selector-namespaced-universal? [v any/c]) boolean?]{
Recognizes namespace-qualified universal selector nodes such as @tt{*|*} or
@tt{foo|*}.}

@defproc[(css-selector-namespaced-universal-namespace
          [selector css-selector-namespaced-universal?])
         string?]{
Returns the namespace prefix for a namespace-qualified universal selector.}

@defproc[(css-selector-namespaced-universal-text
          [selector css-selector-namespaced-universal?])
         string?]{
Returns the raw namespace-qualified universal selector text.}

@defproc[(css-selector-namespaced-universal-span
          [selector css-selector-namespaced-universal?])
         (or/c css-source-span? #f)]{
Returns the source span for a namespace-qualified universal selector when
available.}

@defproc[(css-style-rule-block [rule css-style-rule?])
         (listof (or/c css-declaration?
                       css-comment?
                       css-recovery?
                       css-style-rule?
                       css-at-rule?))]{
Returns the rule block.}

@defproc[(css-style-rule-raw-selector [rule css-style-rule?])
         string?]{
Returns the raw selector text preserved by the style rule.}

@defproc[(css-style-rule-span [rule css-style-rule?])
         (or/c css-source-span? #f)]{
Returns the source span for a style rule when available.}

@defproc[(css-at-rule? [v any/c]) boolean?]{
Recognizes at-rule AST nodes.}

@defproc[(css-at-rule-name [rule css-at-rule?])
         string?]{
Returns the at-rule name, such as @racket["@media"].}

@defproc[(css-at-rule-prelude [rule css-at-rule?])
         string?]{
Returns the at-rule prelude representation.}

@defproc[(css-at-rule-prelude-values [rule css-at-rule?])
         (listof (or/c css-component-token?
                       css-component-an-plus-b?
                       css-component-number?
                       css-component-percentage?
                       css-component-dimension?
                       css-component-string?
                       css-component-hash?
                       css-component-url?
                       css-component-function?
                       css-component-block?))]{
Returns a lightweight component-value view of the at-rule prelude.}

@defproc[(css-at-rule-prelude-derived-details [rule css-at-rule?])
         (or/c css-media-prelude-details?
               css-supports-prelude-details?
               list?)]{
Returns a richer derived prelude view for recognized at-rules.

Currently this provides structured results for @litchar|{@media}| and
@litchar|{@supports}|; other at-rules fall back to the lightweight component-value
list.}

@defproc[(css-media-prelude-details? [v any/c]) boolean?]{
Recognizes derived @litchar|{@media}| prelude nodes.}

@defproc[(css-media-prelude-details-queries [details css-media-prelude-details?])
         (listof css-media-query?)]{
Returns the derived media queries in a @litchar|{@media}| prelude.}

@defproc[(css-media-prelude-details-text [details css-media-prelude-details?])
         string?]{
Returns the raw @litchar|{@media}| prelude text.}

@defproc[(css-media-prelude-details-span [details css-media-prelude-details?])
         (or/c css-source-span? #f)]{
Returns the source span for derived media prelude details when available.}

@defproc[(css-media-query? [v any/c]) boolean?]{
Recognizes one derived media query entry.}

@defproc[(css-media-query-modifier [query css-media-query?])
         (or/c string? #f)]{
Returns the media query modifier, such as @tt{not} or @tt{only}, when one is
present.}

@defproc[(css-media-query-media-type [query css-media-query?])
         (or/c string? #f)]{
Returns the media type, such as @tt{screen}, when one is present.}

@defproc[(css-media-query-features [query css-media-query?])
         (listof (or/c css-media-feature?
                       css-media-feature-expression?
                       css-media-feature-range?))]{
Returns the media feature fragments in the query.}

@defproc[(css-media-query-text [query css-media-query?])
         string?]{
Returns the raw media query text.}

@defproc[(css-media-query-span [query css-media-query?])
         (or/c css-source-span? #f)]{
Returns the source span for a media query when available.}

@defproc[(css-media-feature? [v any/c]) boolean?]{
Recognizes one derived media feature fragment such as
@tt{(width >= 40rem)}.}

@defproc[(css-media-feature-text [feature css-media-feature?])
         string?]{
Returns the raw media feature text.}

@defproc[(css-media-feature-span [feature css-media-feature?])
         (or/c css-source-span? #f)]{
Returns the source span for a media feature when available.}

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

@defproc[(css-media-feature-expression-text [feature css-media-feature-expression?])
         string?]{
Returns the raw media feature expression text.}

@defproc[(css-media-feature-expression-span [feature css-media-feature-expression?])
         (or/c css-source-span? #f)]{
Returns the source span for a media feature expression when available.}

@defproc[(css-media-feature-range? [v any/c]) boolean?]{
Recognizes a typed chained media range such as
@tt{(20rem <= width <= 60rem)}.}

@defproc[(css-media-feature-range-name [feature css-media-feature-range?])
         string?]{
Returns the media feature name for a chained range.}

@defproc[(css-media-feature-range-lower [feature css-media-feature-range?])
         string?]{
Returns the lower bound text for a chained range.}

@defproc[(css-media-feature-range-lower-operator [feature css-media-feature-range?])
         string?]{
Returns the lower comparison operator for a chained range.}

@defproc[(css-media-feature-range-upper-operator [feature css-media-feature-range?])
         string?]{
Returns the upper comparison operator for a chained range.}

@defproc[(css-media-feature-range-upper [feature css-media-feature-range?])
         string?]{
Returns the upper bound text for a chained range.}

@defproc[(css-media-feature-range-text [feature css-media-feature-range?])
         string?]{
Returns the raw chained range text.}

@defproc[(css-media-feature-range-span [feature css-media-feature-range?])
         (or/c css-source-span? #f)]{
Returns the source span for a chained media range when available.}

@defproc[(css-supports-prelude-details? [v any/c]) boolean?]{
Recognizes derived @litchar|{@supports}| prelude nodes.}

@defproc[(css-supports-prelude-details-conditions
          [details css-supports-prelude-details?])
         (listof css-supports-condition?)]{
Returns the top-level derived supports conditions.}

@defproc[(css-supports-prelude-details-text [details css-supports-prelude-details?])
         string?]{
Returns the raw @litchar|{@supports}| prelude text.}

@defproc[(css-supports-prelude-details-span [details css-supports-prelude-details?])
         (or/c css-source-span? #f)]{
Returns the source span for derived supports prelude details when available.}

@defproc[(css-supports-condition? [v any/c]) boolean?]{
Recognizes one derived supports-condition node.

The current condition kinds include @racket['feature], @racket['not],
@racket['and], @racket['or], and @racket['unknown].}

@defproc[(css-supports-condition-kind [condition css-supports-condition?])
         symbol?]{
Returns the condition kind, such as @racket['feature], @racket['not],
@racket['and], @racket['or], or @racket['unknown].}

@defproc[(css-supports-condition-text [condition css-supports-condition?])
         string?]{
Returns the raw supports condition text.}

@defproc[(css-supports-condition-arguments [condition css-supports-condition?])
         list?]{
Returns child conditions or feature nodes for this supports condition.}

@defproc[(css-supports-condition-span [condition css-supports-condition?])
         (or/c css-source-span? #f)]{
Returns the source span for a supports condition when available.}

@defproc[(css-supports-feature? [v any/c]) boolean?]{
Recognizes one typed supports feature test such as
@tt{(display: grid)}.}

@defproc[(css-supports-feature-name [feature css-supports-feature?])
         string?]{
Returns the feature-test name, such as @tt{display}.}

@defproc[(css-supports-feature-value [feature css-supports-feature?])
         string?]{
Returns the raw feature-test value text, such as @tt{grid}.}

@defproc[(css-supports-feature-text [feature css-supports-feature?])
         string?]{
Returns the raw supports feature-test text.}

@defproc[(css-supports-feature-span [feature css-supports-feature?])
         (or/c css-source-span? #f)]{
Returns the source span for a supports feature when available.}

@defproc[(css-at-rule-block [rule css-at-rule?])
         (or/c (listof (or/c css-style-rule?
                              css-at-rule?
                              css-declaration?
                              css-comment?
                              css-recovery?))
               #f)]{
Returns the at-rule body or block representation.}

@defproc[(css-at-rule-span [rule css-at-rule?])
         (or/c css-source-span? #f)]{
Returns the source span for an at-rule when available.}

@defproc[(css-declaration? [v any/c]) boolean?]{
Recognizes declaration AST nodes.}

@defproc[(css-declaration-name [declaration css-declaration?])
         string?]{
Returns the declaration property name.}

@defproc[(css-declaration-value [declaration css-declaration?])
         string?]{
Returns the declaration value representation.}

@defproc[(css-declaration-component-values [declaration css-declaration?])
         (listof (or/c css-component-token?
                       css-component-an-plus-b?
                       css-component-number?
                       css-component-percentage?
                       css-component-dimension?
                       css-component-string?
                       css-component-hash?
                       css-component-url?
                       css-component-function?
                       css-component-block?))]{
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
         list?]{
Returns the rule prelude.}

@defproc[(css-qualified-rule-block [rule css-qualified-rule?])
         list?]{
Returns the block part of a qualified rule.}

@defproc[(css-component-token? [v any/c]) boolean?]{
Recognizes simple component token nodes.}

@defproc[(css-component-token-text [token css-component-token?])
         string?]{
Returns the raw component token text.}

@defproc[(css-component-token-span [token css-component-token?])
         (or/c css-source-span? #f)]{
Returns the source span for a component token when available.}

@defproc[(css-component-an-plus-b? [v any/c]) boolean?]{
Recognizes parsed @tt{an+b} component nodes used by the @tt{nth-*} pseudos.}

@defproc[(css-component-an-plus-b-text [v css-component-an-plus-b?])
         string?]{
Returns the raw @tt{an+b} text.}

@defproc[(css-component-an-plus-b-a [v css-component-an-plus-b?]) integer?]{
Returns the @tt{a} coefficient from an @tt{an+b} node.}

@defproc[(css-component-an-plus-b-b [v css-component-an-plus-b?]) integer?]{
Returns the @tt{b} offset from an @tt{an+b} node.}

@defproc[(css-component-an-plus-b-span [v css-component-an-plus-b?])
         (or/c css-source-span? #f)]{
Returns the source span for an @tt{an+b} component when available.}

@defproc[(css-component-number? [v any/c]) boolean?]{
Recognizes numeric component nodes.}

@defproc[(css-component-number-text [v css-component-number?])
         string?]{
Returns the raw number text.}

@defproc[(css-component-number-value [v css-component-number?])
         number?]{
Returns the parsed numeric value.}

@defproc[(css-component-number-span [v css-component-number?])
         (or/c css-source-span? #f)]{
Returns the source span for a number component when available.}

@defproc[(css-component-percentage? [v any/c]) boolean?]{
Recognizes percentage component nodes.}

@defproc[(css-component-percentage-text [v css-component-percentage?])
         string?]{
Returns the raw percentage text.}

@defproc[(css-component-percentage-value [v css-component-percentage?])
         number?]{
Returns the parsed percentage number without the trailing percent sign.}

@defproc[(css-component-percentage-span [v css-component-percentage?])
         (or/c css-source-span? #f)]{
Returns the source span for a percentage component when available.}

@defproc[(css-component-dimension? [v any/c]) boolean?]{
Recognizes dimension component nodes such as @tt{10px}.}

@defproc[(css-component-dimension-text [v css-component-dimension?])
         string?]{
Returns the raw dimension text.}

@defproc[(css-component-dimension-value [v css-component-dimension?])
         number?]{
Returns the parsed numeric value of a dimension.}

@defproc[(css-component-dimension-unit [v css-component-dimension?])
         string?]{
Returns the dimension unit text, such as @tt{px} or @tt{rem}.}

@defproc[(css-component-dimension-span [v css-component-dimension?])
         (or/c css-source-span? #f)]{
Returns the source span for a dimension component when available.}

@defproc[(css-component-string? [v any/c]) boolean?]{
Recognizes string component nodes.}

@defproc[(css-component-string-text [v css-component-string?])
         string?]{
Returns the raw string token text, including quotes.}

@defproc[(css-component-string-value [v css-component-string?])
         string?]{
Returns the decoded string value.}

@defproc[(css-component-string-span [v css-component-string?])
         (or/c css-source-span? #f)]{
Returns the source span for a string component when available.}

@defproc[(css-component-hash? [v any/c]) boolean?]{
Recognizes hash component nodes such as @tt{#fff}.}

@defproc[(css-component-hash-text [v css-component-hash?])
         string?]{
Returns the raw hash token text, including the leading hash.}

@defproc[(css-component-hash-value [v css-component-hash?])
         string?]{
Returns the hash value without the leading hash.}

@defproc[(css-component-hash-span [v css-component-hash?])
         (or/c css-source-span? #f)]{
Returns the source span for a hash component when available.}

@defproc[(css-component-url? [v any/c]) boolean?]{
Recognizes @tt{url(...)} component nodes.}

@defproc[(css-component-url-text [v css-component-url?])
         string?]{
Returns the raw @tt{url(...)} text.}

@defproc[(css-component-url-value [v css-component-url?])
         string?]{
Returns the URL argument text.}

@defproc[(css-component-url-span [v css-component-url?])
         (or/c css-source-span? #f)]{
Returns the source span for a URL component when available.}

@defproc[(css-component-function? [v any/c]) boolean?]{
Recognizes function component nodes.}

@defproc[(css-component-function-name [v css-component-function?])
         string?]{
Returns the function name, such as @tt{rgb}, @tt{calc}, or @tt{var}.}

@defproc[(css-component-function-arguments [v css-component-function?])
         list?]{
Returns the component values inside the function argument list.}

@defproc[(css-component-function-text [v css-component-function?])
         string?]{
Returns the full raw function text, including the function name and
parentheses.}

@defproc[(css-component-function-span [v css-component-function?])
         (or/c css-source-span? #f)]{
Returns the source span for a function component when available.}

@defproc[(css-component-block? [v any/c]) boolean?]{
Recognizes simple block component nodes.}

@defproc[(css-component-block-delimiter [v css-component-block?])
         char?]{
Returns the opening delimiter character for the block.}

@defproc[(css-component-block-values [v css-component-block?])
         list?]{
Returns the component values inside the block.}

@defproc[(css-component-block-text [v css-component-block?])
         string?]{
Returns the raw block text, including delimiters.}

@defproc[(css-component-block-span [v css-component-block?])
         (or/c css-source-span? #f)]{
Returns the source span for a block component when available.}

@subsubsection{Query And Recovery Reference}

@defproc[(css-flatten-rules [stylesheet css-stylesheet?])
         (listof (or/c css-style-rule? css-at-rule?))]{
Returns a pre-order list of rules and at-rules, recursively flattening nested
rule-bearing at-rules while skipping comments.}

@defproc[(css-find-rules-by-selector-group [stylesheet css-stylesheet?]
                                           [selector-group string?])
         (listof css-style-rule?)]{
Finds style rules whose selector groups include @racket[selector-group]
exactly.

The search preserves source order and flattens nested rule-bearing at-rules the
same way as @racket[css-flatten-rules].}

@defproc[(css-find-rules-by-raw-selector [stylesheet css-stylesheet?]
                                         [raw-selector string?])
         (listof css-style-rule?)]{
Finds style rules whose raw selector text exactly matches
@racket[raw-selector].

The search preserves source order and uses the same nested-rule flattening as
@racket[css-flatten-rules].}

@defproc[(css-find-declarations-in-selector-group
          [stylesheet css-stylesheet?]
          [selector-group string?]
          [property-name (or/c string? #f) #f])
         (listof css-declaration?)]{
Finds declarations in rules whose selector groups include
@racket[selector-group] exactly.

The result preserves source order. When @racket[property-name] is provided, the
result is filtered case-insensitively by property name.}

@defproc[(css-find-declarations-in-selector-groups
          [stylesheet css-stylesheet?]
          [selector-groups (listof string?)]
          [property-name (or/c string? #f) #f])
         (listof css-declaration?)]{
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
         (hash/c string? string?)]{
Collects custom-property declarations from rules whose selector groups include
@racket[selector-group] exactly.

Declarations are processed in source order, and later declarations override
earlier ones in the returned hash.}

@defproc[(css-collect-custom-properties-in-selector-groups
          [stylesheet css-stylesheet?]
          [selector-groups (listof string?)])
         (hash/c string? string?)]{
Collects custom-property declarations from rules whose selector groups include
any string from @racket[selector-groups] exactly.

Declarations are processed in source order, and later declarations override
earlier ones in the returned hash. Nested rule-bearing at-rules are flattened
the same way as @racket[css-flatten-rules], and each matching rule is processed
at most once even if it matches more than one requested selector group.}

@defproc[(css-compute-style-for-selector-group
          [stylesheet css-stylesheet?]
          [selector-group string?]
          [#:resolve-vars? resolve-vars? boolean? #f]
          [#:defaults defaults (or/c (hash/c string? string?) #f) #f]
          [#:trace? trace? boolean? #f])
         (or/c (hash/c string? string?)
               (values/c (hash/c string? string?)
                         css-compute-style-trace?))]{
Computes a reduced tooling-oriented style result for one exact
@racket[selector-group].

Matching uses exact selector-group text only. The helper flattens nested
rule-bearing at-rules the same way as @racket[css-flatten-rules], considers the
declarations from matching rules in source order, and picks winners by
@tt{!important}, then selector specificity, then later source order.

The result is a hash from normalized property name to raw declaration value.
Standard property names are normalized to lowercase. Custom properties are not
included in this result; use
@racket[css-compute-custom-properties-for-selector-group] for the custom
property environment.

This reduced computed-style layer also expands a small explicit shorthand set:
@tt{border}, the four side-specific @tt{border-*} shorthands, @tt{padding}, and
@tt{margin}. Shorthand declarations generate synthetic longhand candidates that
participate in the same winner-selection pipeline as authored longhands, so a
later authored longhand can still override one side from an earlier shorthand.

When all four border side values agree, the returned hash also exposes the
shared aggregate property for @tt{border-width}, @tt{border-style}, or
@tt{border-color}. Otherwise those aggregate keys are omitted instead of being
lossily reconstructed.

When @racket[resolve-vars?] is true, @tt{var(...)} references are resolved
against computed custom properties for the same selector-group target and then
against @racket[defaults]. Unresolved references are left intact.

When @racket[trace?] is true, the function returns two values:
the computed hash and a @racket[css-compute-style-trace?] struct.}

@defproc[(css-compute-custom-properties-for-selector-group
          [stylesheet css-stylesheet?]
          [selector-group string?]
          [#:defaults defaults (or/c (hash/c string? string?) #f) #f]
          [#:resolve-vars? resolve-vars? boolean? #f]
          [#:trace? trace? boolean? #f])
         (or/c (hash/c string? string?)
               (values/c (hash/c string? string?)
                         css-compute-style-trace?))]{
Computes the final custom-property environment for one exact
@racket[selector-group].

Winner selection follows the same rules as
@racket[css-compute-style-for-selector-group]:
@tt{!important}, then selector specificity, then later source order.

When @racket[resolve-vars?] is true, custom-property values are resolved
against other computed custom properties and then @racket[defaults]. Cycles do
not raise errors; cyclical values are left in their raw unresolved form. The
returned hash contains those final resolved values directly, so downstream
tooling does not need to run a second custom-property resolver for ordinary
exact-target use cases.

When @racket[trace?] is true, the function returns two values:
the computed hash and a @racket[css-compute-style-trace?] struct.}

@defproc[(css-compute-style-trace? [v any/c])
         boolean?]{
Recognizes trace payloads returned by the computed-style helpers when
@racket[#:trace? #t] is requested.}

@defproc[(css-compute-style-trace-selector-group
          [trace css-compute-style-trace?])
         string?]{
Returns the exact selector-group target used for the computation.}

@defproc[(css-compute-style-trace-matched-rules
          [trace css-compute-style-trace?])
         (listof css-compute-matched-rule?)]{
Returns the matched-rule records considered for the exact selector-group
target. Each entry records the selector group, specificity, source order, and
style rule.}

@defproc[(css-compute-style-trace-property-results
          [trace css-compute-style-trace?])
         (listof css-compute-property-result?)]{
Returns per-property winner-selection records for standard properties. Each
result includes the considered candidates and the winning candidate.}

@defproc[(css-compute-style-trace-custom-property-results
          [trace css-compute-style-trace?])
         (listof css-compute-property-result?)]{
Returns per-property winner-selection records for custom properties.}

@defproc[(css-compute-style-trace-var-resolutions
          [trace css-compute-style-trace?])
         (listof css-compute-var-resolution?)]{
Returns variable-resolution records describing resolved values, referenced
custom properties, and whether cycle handling was encountered.}

@defproc[(css-compute-matched-rule? [v any/c])
         boolean?]{
Recognizes trace entries for style rules that matched the exact selector-group
target.}

@defproc[(css-compute-matched-rule-selector-group
          [matched-rule css-compute-matched-rule?])
         string?]{
Returns the matched selector-group string.}

@defproc[(css-compute-matched-rule-specificity
          [matched-rule css-compute-matched-rule?])
         (list/c exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 exact-nonnegative-integer?)]{
Returns the selector specificity tuple as @tt{(ids classes types)}.}

@defproc[(css-compute-matched-rule-source-order
          [matched-rule css-compute-matched-rule?])
         exact-nonnegative-integer?]{
Returns the source-order index used as the final winner-selection tie-breaker.}

@defproc[(css-compute-matched-rule-rule
          [matched-rule css-compute-matched-rule?])
         css-style-rule?]{
Returns the underlying matched style rule.}

@defproc[(css-compute-property-result? [v any/c])
         boolean?]{
Recognizes trace entries for one computed property name.}

@defproc[(css-compute-property-result-name
          [result css-compute-property-result?])
         string?]{
Returns the normalized property name for the result.}

@defproc[(css-compute-property-result-candidates
          [result css-compute-property-result?])
         (listof css-compute-candidate?)]{
Returns the authored or shorthand-expanded candidates considered for this
property.}

@defproc[(css-compute-property-result-winner
          [result css-compute-property-result?])
         css-compute-candidate?]{
Returns the candidate that won by importance, specificity, and source order.}

@defproc[(css-compute-candidate? [v any/c])
         boolean?]{
Recognizes one declaration candidate considered by the reduced computed-style
winner selection.}

@defproc[(css-compute-candidate-name
          [candidate css-compute-candidate?])
         string?]{
Returns the property name being assigned by this candidate.}

@defproc[(css-compute-candidate-value
          [candidate css-compute-candidate?])
         string?]{
Returns the raw declaration value for this candidate.}

@defproc[(css-compute-candidate-important?
          [candidate css-compute-candidate?])
         boolean?]{
Reports whether the source declaration was marked @tt{!important}.}

@defproc[(css-compute-candidate-specificity
          [candidate css-compute-candidate?])
         (list/c exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 exact-nonnegative-integer?)]{
Returns the selector specificity tuple associated with this candidate.}

@defproc[(css-compute-candidate-source-order
          [candidate css-compute-candidate?])
         exact-nonnegative-integer?]{
Returns the source-order index associated with this candidate.}

@defproc[(css-compute-candidate-declaration
          [candidate css-compute-candidate?])
         css-declaration?]{
Returns the authored declaration that produced this candidate.}

@defproc[(css-compute-candidate-matched-rule
          [candidate css-compute-candidate?])
         css-compute-matched-rule?]{
Returns the matched-rule trace entry associated with this candidate.}

@defproc[(css-compute-candidate-source-name
          [candidate css-compute-candidate?])
         string?]{
Returns the authored property name that produced a candidate.

If this differs from @racket[(css-compute-candidate-name candidate)], then the
candidate came from shorthand expansion rather than from an authored longhand.}

@defproc[(css-compute-var-resolution? [v any/c])
         boolean?]{
Recognizes trace entries for one custom-property or style-property variable
resolution step.}

@defproc[(css-compute-var-resolution-name
          [resolution css-compute-var-resolution?])
         string?]{
Returns the property name whose value was resolved.}

@defproc[(css-compute-var-resolution-raw-value
          [resolution css-compute-var-resolution?])
         string?]{
Returns the raw value before @tt{var(...)} substitution.}

@defproc[(css-compute-var-resolution-resolved-value
          [resolution css-compute-var-resolution?])
         string?]{
Returns the value after the reduced resolver substituted what it could.}

@defproc[(css-compute-var-resolution-references
          [resolution css-compute-var-resolution?])
         (listof string?)]{
Returns the custom-property names referenced while resolving the value.}

@defproc[(css-compute-var-resolution-cycle?
          [resolution css-compute-var-resolution?])
         boolean?]{
Reports whether cycle handling was encountered while resolving the value.}

@defproc[(css-find-declarations [stylesheet css-stylesheet?]
                                [name string?])
         (listof css-declaration?)]{
Finds declarations whose property name matches @racket[name]
case-insensitively.}

@defproc[(css-query-selector [stylesheet css-stylesheet?]
                             [selector string?])
         (listof css-style-rule?)]{
Finds style rules whose selector groups include @racket[selector].}

@defproc[(css-find-rules-by-pseudo [stylesheet css-stylesheet?]
                                   [pseudo-name string?])
         (listof css-style-rule?)]{
Finds style rules whose derived selector structure contains a pseudo selector
with the given name.}

@defproc[(css-find-media-queries [stylesheet css-stylesheet?])
         (listof css-media-query?)]{
Returns the derived @racket[css-media-query?] nodes collected from
@litchar|{@media}| rules in the stylesheet.}

@defproc[(css-find-supports-features [stylesheet css-stylesheet?]
                                     [name (or/c string? #f) #f])
         (listof css-supports-feature?)]{
Returns the typed @racket[css-supports-feature?] leaves collected from
@litchar|{@supports}| rules.

When @racket[name] is provided, the result is filtered case-insensitively by
feature name.}

@defproc[(css-recovery-nodes [stylesheet css-stylesheet?])
         (listof css-recovery?)]{
Returns all recovery nodes in the stylesheet.}

@defproc[(css-has-recovery? [stylesheet css-stylesheet?])
         boolean?]{
Reports whether the stylesheet contains any recovery nodes.}

@defproc[(css-recovery-summary [stylesheet css-stylesheet?])
         (listof (cons/c symbol? exact-nonnegative-integer?))]{
Summarizes recovery nodes by kind as a sorted association list of
@racket[(kind . count)] pairs.}

@subsubsection{Error Reference}

The parser uses a CSS-specific exception type rather than generic
contract or read errors for parser failures.

@defproc[(exn:fail:css? [v any/c]) boolean?]{
Recognizes CSS parser exceptions.}

@defproc[(exn:fail:css-source [e exn:fail:css?])
         any/c]{
Returns the source value attached to a CSS parser exception.}

@defproc[(exn:fail:css-detail [e exn:fail:css?])
         any/c]{
Returns parser-specific detail attached to a CSS parser exception.}

@subsubsection{Parser Procedure Reference}

@defproc[(css-parser? [v any/c]) boolean?]{
Recognizes parser procedures created by @racket[make-css-parser].
}

@section{TOML}

@defmodule[parsers/toml
           #:use-sources (parsers/private/toml-ast
                          parsers/private/toml-parser)]

TOML is a configuration-file format with explicit tables, key/value entries,
arrays, inline tables, strings, numbers, booleans, and date/time values.
@racketmodname[parsers/toml] parses that structural syntax into a small,
source-preserving AST suitable for configuration tooling and inspection.

@subsection{Quick Start}

@examples[#:eval toml-eval
(define document
  (parse-toml
   "[package]\nname = \"parsers\"\nfeatures = [\"css\", \"toml\"]\n"))
(toml-document? document)
(serialize-toml document)
]

@subsection{Model and Limitations}

The parser preserves the complete original source in every
@racket[toml-document?], so @racket[serialize-toml] reproduces the parsed text
exactly. The AST represents document order, table headers, assignments, nested
arrays and inline tables, comments, and lexical recovery nodes.

This is a structural parser, not a TOML semantic evaluator. In particular, it
does not reject duplicate keys, resolve dotted paths into a nested environment,
enforce table redefinition rules, or decode TOML escapes into Racket values.
Those policies belong in a separate interpretation layer.

@subsection{Reference}

@defproc[(parse-toml [source (or/c string? input-port?)]) toml-document?]{
Parses a complete TOML document supplied as a string or input port.}

@defproc[(parse-toml-port [in input-port?]) toml-document?]{
Parses TOML source from @racket[in].}

@defproc[(serialize-toml [document toml-document?]) string?]{
Returns the exact source retained by @racket[document].}

@defproc[(toml-document-find-tables [document toml-document?]
                                    [path (or/c string? (listof string?))])
         (listof toml-table?)]{
Returns tables whose dotted key path matches @racket[path] exactly, in source
order. A string path is split on dots; use a list for keys that contain dots.}

@defproc[(toml-table-find-values [table toml-table?]
                                 [path (or/c string? (listof string?))])
         list?]{
Returns values assigned by the exact dotted @racket[path] in @racket[table],
in source order.}

@defstruct*[#:link-target? #f toml-source-span
            ([start exact-nonnegative-integer?] [end exact-nonnegative-integer?])
            #:transparent #:omit-constructor]{
Represents a half-open character range. @racket[start] is the zero-based first
offset and @racket[end] is the first offset after the node.}

@defstruct*[#:link-target? #f toml-document
            ([items list?] [source string?] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents a complete TOML file. @racket[items] contains top-level tables,
assignments before the first table, comments, and recovery nodes in source
order. @racket[source] is the original text and @racket[span] covers it.}

@defstruct*[#:link-target? #f toml-table
            ([key toml-key?] [array? boolean?] [entries list?] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents a table header. @racket[key] is its dotted path;
@racket[array?] distinguishes @litchar|{[[table]]}| from @litchar|{[table]}|;
@racket[entries] contains following assignments and comments; and
@racket[span] covers the header.}

@defstruct*[#:link-target? #f toml-key-value
            ([key toml-key?] [value toml-value?] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents one assignment. @racket[key] is the dotted TOML key,
@racket[value] is its parsed structural value, and @racket[span] covers the
whole assignment.}

@defproc[(toml-value? [value any/c]) boolean?]{
Recognizes a parsed TOML scalar, array, or inline-table value.}

@defstruct*[#:link-target? #f toml-key
            ([parts (listof string?)] [text string?] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents a dotted key. @racket[parts] contains each key component,
@racket[text] preserves the authored text, and @racket[span] covers it.}

@defstruct*[#:link-target? #f toml-array
            ([values list?] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents an array. @racket[values] contains parsed values in order and
@racket[span] covers its brackets and contents.}

@defstruct*[#:link-target? #f toml-inline-table
            ([entries (listof toml-key-value?)] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents an inline table. @racket[entries] contains its assignments in order
and @racket[span] covers its braces and contents.}

@defstruct*[#:link-target? #f toml-string
            ([text string?] [value string?] [literal? boolean?]
             [multiline? boolean?] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents a quoted string. @racket[text] preserves delimiters and escapes;
@racket[value] removes delimiters without decoding escapes; @racket[literal?]
identifies single-quoted strings; @racket[multiline?] identifies triple-quoted
strings; and @racket[span] covers the source.}

@defstruct*[#:link-target? #f toml-boolean
            ([text string?] [value boolean?] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents @tt{true} or @tt{false}. @racket[text] is the source spelling,
@racket[value] is the Racket boolean, and @racket[span] covers the source.}

@defstruct*[#:link-target? #f toml-number
            ([text string?] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents a TOML numeric spelling. @racket[text] is retained without numeric
conversion and @racket[span] covers its source.}

@defstruct*[#:link-target? #f toml-date-time
            ([text string?] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents a TOML date, time, or date-time spelling. @racket[text] retains the
authored value and @racket[span] covers the source.}

@defstruct*[#:link-target? #f toml-bare-value
            ([text string?] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents an unclassified bare value. @racket[text] is the source spelling
and @racket[span] covers it.}

@defstruct*[#:link-target? #f toml-comment
            ([text string?] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents a comment. @racket[text] includes its leading @tt{#};
@racket[span] covers the comment excluding its line ending.}

@defstruct*[#:link-target? #f toml-recovery
            ([reason string?] [text string?] [span toml-source-span?])
            #:transparent #:omit-constructor]{
Represents malformed source the parser retained. @racket[reason] explains the
issue, @racket[text] retains the affected source, and @racket[span] identifies
its location.}

@section{Lua}

@defmodule[parsers/lua
           #:use-sources (parsers/private/lua-ast
                          parsers/private/lua-parser)]

Lua is a lightweight, embeddable programming language commonly used for
configuration, scripting, and application extension. @racketmodname[parsers/lua]
uses the Lua 5.4 lexer to provide a small source-preserving structural model
for tools that need to inspect or rewrite source without evaluating it.

@subsection{Quick Start}

@examples[#:eval lua-eval
(define chunk
  (parse-lua
   "-- greeting\nlocal message = \"hello\"\nfunction greet(name)\n  return message .. name\nend\n"))
(map lua-statement-kind (lua-chunk-statements chunk))
(serialize-lua chunk)
]

@subsection{Model and Limitations}

The parser consumes the derived token stream from @racketmodname[lexers/lua].
It groups non-trivia tokens into statement-like source forms, separates
comments, preserves exact token text and positions, and retains the complete
original source. Thus @racket[serialize-lua] exactly reproduces parsed input.

The statement grouping follows physical source lines and is intentionally not
a full Lua grammar. It does not build expression trees, resolve names, execute
code, validate block matching, or model Lua runtime semantics. Those concerns
belong in later syntax and interpretation layers.

@subsection{Reference}

@defproc[(parse-lua [source (or/c string? input-port?)]) lua-chunk?]{
Parses Lua supplied as a string or input port into a source-preserving chunk.}

@defproc[(parse-lua-port [in input-port?]) lua-chunk?]{
Parses Lua source from @racket[in].}

@defproc[(serialize-lua [chunk lua-chunk?]) string?]{
Returns the exact source retained by @racket[chunk].}

@defproc[(lua-chunk-statements [chunk lua-chunk?])
         (listof lua-statement?)]{
Returns all statement-like forms from @racket[chunk] in source order.}

@defproc[(lua-find-statements-by-kind [chunk lua-chunk?] [kind symbol?])
         (listof lua-statement?)]{
Returns statement-like forms whose leading-kind classification equals
@racket[kind]. Typical kinds include @racket['local], @racket['function],
@racket['if], @racket['return], @racket['end], and
@racket['expression-or-assignment].}

@defstruct*[#:link-target? #f lua-source-span
            ([start exact-nonnegative-integer?] [end exact-nonnegative-integer?])
            #:transparent #:omit-constructor]{
Represents a half-open character range. @racket[start] is the zero-based first
offset and @racket[end] is the first offset after the node.}

@defstruct*[#:link-target? #f lua-chunk
            ([forms (listof (or/c lua-statement? lua-comment? lua-recovery?))]
             [source string?] [span lua-source-span?])
            #:transparent #:omit-constructor]{
Represents a complete Lua source file. @racket[forms] contains statement-like
forms, comments, and recoveries in source order. @racket[source] retains the
complete original text and @racket[span] covers it.}

@defstruct*[#:link-target? #f lua-statement
            ([kind symbol?] [text string?] [tokens (listof lua-token?)]
             [span lua-source-span?])
            #:transparent #:omit-constructor]{
Represents a statement-like source form. @racket[kind] classifies its leading
keyword or token, @racket[text] retains the exact source from its first through
last non-trivia token, @racket[tokens] contains those tokens in order, and
@racket[span] covers that text.}

@defstruct*[#:link-target? #f lua-comment
            ([text string?] [span lua-source-span?])
            #:transparent #:omit-constructor]{
Represents a line or long comment. @racket[text] includes its Lua comment
delimiter and @racket[span] covers its source.}

@defstruct*[#:link-target? #f lua-token
            ([kind symbol?] [tags (listof symbol?)] [text string?]
             [span lua-source-span?])
            #:transparent #:omit-constructor]{
Represents one non-trivia lexer-derived token. @racket[kind] is one of
@racket['keyword], @racket['constant], @racket['identifier], @racket['string],
@racket['number], @racket['operator], @racket['delimiter], or
@racket['unknown]. @racket[tags] retains the lexer classifications,
@racket[text] retains exact source text, and @racket[span] covers it.}

@defstruct*[#:link-target? #f lua-recovery
            ([reason string?] [text string?] [span lua-source-span?])
            #:transparent #:omit-constructor]{
Represents malformed input reported by the lexer. @racket[reason] describes
the issue, @racket[text] retains the malformed source, and @racket[span]
identifies its location.}
