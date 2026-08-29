#lang racket/base

;;;
;;; TOML Parser
;;;

;; Parse TOML's structural syntax while preserving exact source in the AST.

;; parse-toml-source : string? -> toml-document?
;;   Parse a TOML source string into a source-preserving document AST.

(provide parse-toml-source)

(require parser-tools/lex
         racket/list
         racket/string
         lexers/toml
         "toml-ast.rkt")

;; parser : string? exact-nonnegative-integer? -> parser?
;;   Keep the source and the next source offset together.
(struct parser (source index) #:mutable)

;; parser-end? : parser? -> boolean?
;;   Determine whether the parser has consumed its source.
(define (parser-end? state)
  (>= (parser-index state) (string-length (parser-source state))))

;; parser-peek : parser? -> (or/c char? eof-object?)
;;   Read the next character without consuming it.
(define (parser-peek state)
  (if (parser-end? state) eof (string-ref (parser-source state) (parser-index state))))

;; parser-peek-at : parser? exact-nonnegative-integer? -> (or/c char? eof-object?)
;;   Read a character at an offset from the current parser position.
(define (parser-peek-at state offset)
  (define index (+ (parser-index state) offset))
  (if (>= index (string-length (parser-source state))) eof
      (string-ref (parser-source state) index)))

;; parser-char=? : (or/c char? eof-object?) char? -> boolean?
;;   Safely compare an optional source character.
(define (parser-char=? value expected)
  (and (char? value) (char=? value expected)))

;; parser-advance! : parser? [exact-nonnegative-integer?] -> void?
;;   Consume one or more source characters.
(define (parser-advance! state [count 1])
  (set-parser-index! state (+ (parser-index state) count)))

;; span : exact-nonnegative-integer? exact-nonnegative-integer? -> toml-source-span?
;;   Construct a source range for AST nodes.
(define (span start end) (toml-source-span start end))

;; horizontal-space? : char? -> boolean?
;;   Recognize TOML whitespace that does not end a line.
(define (horizontal-space? ch) (member ch '(#\space #\tab)))

;; skip-horizontal-space! : parser? -> void?
;;   Consume spaces and tabs but leave line endings for the document parser.
(define (skip-horizontal-space! state)
  (let loop ()
    (define next (parser-peek state))
    (when (and (char? next) (horizontal-space? next))
      (parser-advance! state)
      (loop))))

;; consume-line-ending! : parser? -> void?
;;   Consume one TOML line ending, including CRLF.
(define (consume-line-ending! state)
  (define next (parser-peek state))
  (cond
    [(parser-char=? next #\return)
     (parser-advance! state)
     (when (parser-char=? (parser-peek state) #\newline)
       (parser-advance! state))]
    [(parser-char=? next #\newline) (parser-advance! state)]))

;; read-comment : parser? -> toml-comment?
;;   Read a comment through, but not including, its line ending.
(define (read-comment state)
  (define start (parser-index state))
  (let loop ()
    (define next (parser-peek state))
    (unless (or (eof-object? next) (parser-char=? next #\newline) (parser-char=? next #\return))
      (parser-advance! state)
      (loop)))
  (define end (parser-index state))
  (toml-comment (substring (parser-source state) start end) (span start end)))

;; quoted-end : parser? char? boolean? -> exact-nonnegative-integer?
;;   Consume one basic or literal TOML string and return its ending offset.
(define (quoted-end state quote multiline?)
  (define delimiter-length (if multiline? 3 1))
  (parser-advance! state delimiter-length)
  (let loop ([escaped? #f])
    (define next (parser-peek state))
    (cond
      [(eof-object? next) (parser-index state)]
      [(and multiline?
            (parser-char=? next quote)
            (parser-char=? (parser-peek-at state 1) quote)
            (parser-char=? (parser-peek-at state 2) quote))
       (parser-advance! state 3)
       (parser-index state)]
      [(and (not multiline?) (parser-char=? next quote) (not escaped?))
       (parser-advance! state)
       (parser-index state)]
      [else
       (parser-advance! state)
       (loop (and (char=? quote #\") (parser-char=? next #\\) (not escaped?)))])))

;; unquote-toml-string : string? boolean? boolean? -> string?
;;   Return the string content without its TOML delimiters; escapes stay raw.
(define (unquote-toml-string text literal? multiline?)
  (define width (if multiline? 3 1))
  (if (>= (string-length text) (* 2 width))
      (substring text width (- (string-length text) width))
      text))

;; read-quoted-string : parser? -> toml-string?
;;   Read a quoted TOML key or value.
(define (read-quoted-string state)
  (define start (parser-index state))
  (define quote (parser-peek state))
  (define multiline?
    (and (parser-char=? (parser-peek-at state 1) quote)
         (parser-char=? (parser-peek-at state 2) quote)))
  (define end (quoted-end state quote multiline?))
  (define text (substring (parser-source state) start end))
  (toml-string text
               (unquote-toml-string text (char=? quote #\') multiline?)
               (char=? quote #\')
               multiline?
               (span start end)))

;; read-bare : parser? (char? -> boolean?) -> string?
;;   Consume a contiguous unquoted source fragment accepted by predicate.
(define (read-bare state continue?)
  (define start (parser-index state))
  (let loop ()
    (define next (parser-peek state))
    (when (and (char? next) (continue? next))
      (parser-advance! state)
      (loop)))
  (substring (parser-source state) start (parser-index state)))

;; parse-key : parser? -> toml-key?
;;   Parse a dotted TOML key, preserving its source spelling.
(define (parse-key state)
  (define start (parser-index state))
  (define parts '())
  (let loop ()
    (skip-horizontal-space! state)
    (define next (parser-peek state))
    (define part
      (cond
    [(or (parser-char=? next #\") (parser-char=? next #\'))
         (toml-string-value (read-quoted-string state))]
        [else (read-bare state (lambda (ch) (or (char-alphabetic? ch)
                                                 (char-numeric? ch)
                                                 (member ch '(#\_ #\-)))))]))
    (set! parts (append parts (list part)))
    (skip-horizontal-space! state)
    (cond
      [(parser-char=? (parser-peek state) #\.)
       (parser-advance! state)
       (loop)]))
  (define end (parser-index state))
  (toml-key parts (substring (parser-source state) start end) (span start end)))

;; bare-value-kind : string? -> symbol?
;;   Classify a complete unquoted TOML scalar conservatively.
(define (bare-value-kind text)
  (cond
    [(member text '("true" "false")) 'boolean]
    [(regexp-match? #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}(?:[Tt ][0-9:.+-]+(?:[Zz]|[+-][0-9:]+)?)?$" text) 'date-time]
    [(regexp-match? #px"^[0-9]{2}:[0-9]{2}:[0-9]{2}(?:\\.[0-9]+)?$" text) 'date-time]
    [(regexp-match? #px"^[+-]?(?:inf|nan|(?:0|[1-9][0-9_]*)(?:\\.[0-9_]+)?(?:[eE][+-]?[0-9_]+)?|0[xX][0-9A-Fa-f_]+|0[oO][0-7_]+|0[bB][01_]+)$" text) 'number]
    [else 'bare]))

;; parse-bare-value : parser? -> toml-value?
;;   Parse an unquoted scalar through the next TOML value boundary.
(define (parse-bare-value state)
  (define start (parser-index state))
  (let loop ()
    (define next (parser-peek state))
    (unless (or (eof-object? next)
                (parser-char=? next #\newline) (parser-char=? next #\return)
                (parser-char=? next #\#) (parser-char=? next #\,) (parser-char=? next #\]) (parser-char=? next #\}))
      (parser-advance! state)
      (loop)))
  (define end (parser-index state))
  (define text (string-trim (substring (parser-source state) start end)))
  (case (bare-value-kind text)
    [(boolean) (toml-boolean text (string=? text "true") (span start end))]
    [(number) (toml-number text (span start end))]
    [(date-time) (toml-date-time text (span start end))]
    [else (toml-bare-value text (span start end))]))

;; parse-array : parser? -> toml-array?
;;   Parse a TOML array and its nested values.
(define (parse-array state)
  (define start (parser-index state))
  (parser-advance! state)
  (define values '())
  (let loop ()
    (skip-value-trivia! state)
    (define next (parser-peek state))
    (cond
      [(or (eof-object? next) (parser-char=? next #\]))
       (when (char? next) (parser-advance! state))]
      [else
       (set! values (append values (list (parse-value state))))
       (skip-value-trivia! state)
       (when (parser-char=? (parser-peek state) #\,)
         (parser-advance! state))
       (loop)]))
  (toml-array values (span start (parser-index state))))

;; parse-inline-table : parser? -> toml-inline-table?
;;   Parse a TOML inline table and its key/value entries.
(define (parse-inline-table state)
  (define start (parser-index state))
  (parser-advance! state)
  (define entries '())
  (let loop ()
    (skip-horizontal-space! state)
    (define next (parser-peek state))
    (cond
      [(or (eof-object? next) (parser-char=? next #\}))
       (when (char? next) (parser-advance! state))]
      [else
       (define entry-start (parser-index state))
       (define key (parse-key state))
       (skip-horizontal-space! state)
       (when (parser-char=? (parser-peek state) #\=)
         (parser-advance! state))
       (skip-horizontal-space! state)
       (define value (parse-value state))
       (set! entries (append entries (list (toml-key-value key value (span entry-start (parser-index state))))))
       (skip-horizontal-space! state)
       (when (parser-char=? (parser-peek state) #\,)
         (parser-advance! state))
       (loop)]))
  (toml-inline-table entries (span start (parser-index state))))

;; skip-value-trivia! : parser? -> void?
;;   Skip value whitespace and comments, including newlines inside arrays.
(define (skip-value-trivia! state)
  (let loop ()
    (skip-horizontal-space! state)
    (define next (parser-peek state))
    (cond
      [(parser-char=? next #\#) (read-comment state) (loop)]
      [(or (parser-char=? next #\newline) (parser-char=? next #\return))
       (consume-line-ending! state)
       (loop)])))

;; parse-value : parser? -> toml-value?
;;   Parse one TOML value without interpreting its semantic meaning.
(define (parse-value state)
  (define next (parser-peek state))
  (cond
    [(or (parser-char=? next #\") (parser-char=? next #\')) (read-quoted-string state)]
    [(parser-char=? next #\[) (parse-array state)]
    [(parser-char=? next #\{) (parse-inline-table state)]
    [else (parse-bare-value state)]))

;; skip-to-line-end! : parser? -> void?
;;   Consume trailing horizontal space and a possible comment.
(define (skip-to-line-end! state)
  (skip-horizontal-space! state)
  (when (parser-char=? (parser-peek state) #\#)
    (read-comment state))
  (when (or (parser-char=? (parser-peek state) #\newline) (parser-char=? (parser-peek state) #\return))
    (consume-line-ending! state)))

;; parse-header : parser? -> toml-table?
;;   Parse a table or array-of-tables header.
(define (parse-header state)
  (define start (parser-index state))
  (parser-advance! state)
  (define array? (parser-char=? (parser-peek state) #\[))
  (when array? (parser-advance! state))
  (skip-horizontal-space! state)
  (define key (parse-key state))
  (skip-horizontal-space! state)
  (when (parser-char=? (parser-peek state) #\])
    (parser-advance! state)
    (when (and array? (parser-char=? (parser-peek state) #\]))
      (parser-advance! state)))
  (skip-to-line-end! state)
  (toml-table key array? '() (span start (parser-index state))))

;; parse-assignment : parser? -> toml-key-value?
;;   Parse a document-level TOML key/value assignment.
(define (parse-assignment-value state)
  (parser-advance! state)
  (skip-horizontal-space! state)
  (parse-value state))

;; parse-assignment : parser? -> toml-key-value?
;;   Parse a document-level TOML key/value assignment.
(define (parse-assignment state)
  (define start (parser-index state))
  (define key (parse-key state))
  (skip-horizontal-space! state)
  (if (parser-char=? (parser-peek state) #\=)
      (let ([value (parse-assignment-value state)])
        (skip-to-line-end! state)
        (toml-key-value key value (span start (parser-index state))))
      (begin
        (skip-to-line-end! state)
        (toml-recovery "expected = after TOML key"
                       (substring (parser-source state) start (parser-index state))
                       (span start (parser-index state))))))

;; lexical-recoveries : string? -> (listof toml-recovery?)
;;   Record lexical problems reported by the TOML lexer alongside structural parsing.
(define (lexical-recoveries source)
  (for/list ([token (in-list (toml-string->derived-tokens source))]
             #:when (toml-derived-token-has-tag? token 'malformed-token))
    (define start (sub1 (position-offset (toml-derived-token-start token))))
    (define end (sub1 (position-offset (toml-derived-token-end token))))
    (toml-recovery "malformed TOML token" (toml-derived-token-text token) (span start end))))

;; parse-toml-source : string? -> toml-document?
;;   Parse TOML source, retaining lexical errors from lexers/toml as recovery nodes.
(define (parse-toml-source source)
  (define state (parser source 0))
  (define items '())
  (define current-table #f)
  (define (append-item! item)
    (cond
      [current-table
       (set-toml-table-entries! current-table (append (toml-table-entries current-table) (list item)))]
      [else (set! items (append items (list item)))]))
  (let loop ()
    (skip-horizontal-space! state)
    (unless (parser-end? state)
      (define next (parser-peek state))
      (cond
        [(or (parser-char=? next #\newline) (parser-char=? next #\return)) (consume-line-ending! state)]
        [(parser-char=? next #\#) (append-item! (read-comment state)) (consume-line-ending! state)]
        [(parser-char=? next #\[)
         (define table (parse-header state))
         (set! items (append items (list table)))
         (set! current-table table)]
        [else (append-item! (parse-assignment state))])
      (loop)))
  (define recoveries (lexical-recoveries source))
  (toml-document (append items recoveries) source (span 0 (string-length source))))
