#lang racket/base

;;;
;;; Scheme Parser
;;;

;; Parse common Scheme reader structure from lexers/scheme derived tokens.

;; parse-scheme-source : string? symbol? -> scheme-document?
;;   Parse Scheme source for one supported report or implementation dialect.

(provide parse-scheme-source)

(require parser-tools/lex
         racket/list
         lexers/scheme
         "scheme-ast.rkt")

;; cursor : vector? exact-nonnegative-integer? -> cursor?
;;   Hold a Scheme derived-token stream and its next position.
(struct cursor (tokens index) #:mutable)

;; cursor-end? : cursor? -> boolean?
;;   Determine whether all derived tokens have been consumed.
(define (cursor-end? state) (>= (cursor-index state) (vector-length (cursor-tokens state))))

;; cursor-peek : cursor? -> (or/c scheme-derived-token? #f)
;;   Inspect the next derived token without consuming it.
(define (cursor-peek state)
  (and (not (cursor-end? state)) (vector-ref (cursor-tokens state) (cursor-index state))))

;; cursor-read! : cursor? -> (or/c scheme-derived-token? #f)
;;   Consume and return the next derived token.
(define (cursor-read! state)
  (define token (cursor-peek state))
  (when token (set-cursor-index! state (add1 (cursor-index state))))
  token)

;; trivia? : scheme-derived-token? -> boolean?
;;   Identify whitespace that the parser does not represent as a datum.
(define (trivia? token) (scheme-derived-token-has-tag? token 'scheme-whitespace))

;; skip-trivia! : cursor? -> void?
;;   Consume non-semantic reader whitespace.
(define (skip-trivia! state)
  (let loop ()
    (define token (cursor-peek state))
    (when (and token (trivia? token))
      (cursor-read! state)
      (loop))))

;; token-span : scheme-derived-token? -> scheme-source-span?
;;   Convert lexer source positions to zero-based half-open offsets.
(define (token-span token)
  (scheme-source-span
   (sub1 (position-offset (scheme-derived-token-start token)))
   (sub1 (position-offset (scheme-derived-token-end token)))))

;; token-kind : scheme-derived-token? -> symbol?
;;   Classify a lexer token into a parser-facing atomic or comment kind.
(define (token-kind token)
  (cond
    [(scheme-derived-token-has-tag? token 'scheme-line-comment) 'line]
    [(scheme-derived-token-has-tag? token 'scheme-block-comment) 'block]
    [(scheme-derived-token-has-tag? token 'scheme-datum-comment) 'datum]
    [(scheme-derived-token-has-tag? token 'scheme-reader-directive) 'reader-directive]
    [(scheme-derived-token-has-tag? token 'scheme-boolean) 'boolean]
    [(scheme-derived-token-has-tag? token 'scheme-number) 'number]
    [(scheme-derived-token-has-tag? token 'scheme-string) 'string]
    [(scheme-derived-token-has-tag? token 'scheme-character) 'character]
    [(scheme-derived-token-has-tag? token 'scheme-keyword) 'keyword]
    [(scheme-derived-token-has-tag? token 'scheme-escaped-identifier) 'escaped-identifier]
    [(scheme-derived-token-has-tag? token 'scheme-identifier) 'identifier]
    [else 'unknown]))

;; close-delimiter? : scheme-derived-token? -> boolean?
;;   Identify a list-closing delimiter.
(define (close-delimiter? token)
  (and (scheme-derived-token-has-tag? token 'scheme-delimiter)
       (member (scheme-derived-token-text token) '(")" "]" "}"))))

;; opening-delimiter? : scheme-derived-token? -> boolean?
;;   Identify an ordinary list-opening delimiter.
(define (opening-delimiter? token)
  (and (scheme-derived-token-has-tag? token 'scheme-delimiter)
       (member (scheme-derived-token-text token) '("(" "[" "{"))))

;; matching-close : string? -> string?
;;   Return the matching reader delimiter for a list opener.
(define (matching-close opener)
  (case (string-ref opener 0)
    [(#\() ")"]
    [(#\[) "]"]
    [(#\{) "}"]))

;; abbreviation? : scheme-derived-token? -> boolean?
;;   Identify reader abbreviation delimiters that prefix one following datum.
(define (abbreviation? token)
  (or (scheme-derived-token-has-tag? token 'scheme-abbreviation)
      (scheme-derived-token-has-tag? token 'scheme-reader-abbreviation)))

;; parse-form : cursor? -> (or/c scheme-datum? #f)
;;   Parse one reader form, preserving malformed fragments as recovery nodes.
(define (parse-form state)
  (skip-trivia! state)
  (define token (cursor-peek state))
  (cond
    [(not token) #f]
    [(scheme-derived-token-has-tag? token 'scheme-comment)
     (cursor-read! state)
     (scheme-comment (token-kind token) (scheme-derived-token-text token) (token-span token))]
    [(scheme-derived-token-has-tag? token 'malformed-token)
     (cursor-read! state)
     (scheme-recovery "malformed Scheme token" (scheme-derived-token-text token) (token-span token))]
    [(opening-delimiter? token) (parse-list state)]
    [(scheme-derived-token-has-tag? token 'scheme-vector-open) (parse-vector state 'vector)]
    [(scheme-derived-token-has-tag? token 'scheme-bytevector-open) (parse-vector state 'bytevector)]
    [(abbreviation? token) (parse-abbreviation state)]
    [(close-delimiter? token)
     (cursor-read! state)
     (scheme-recovery "unexpected closing Scheme delimiter"
                      (scheme-derived-token-text token)
                      (token-span token))]
    [else
     (cursor-read! state)
     (scheme-atom (token-kind token)
                  (scheme-derived-token-text token)
                  (scheme-derived-token-tags token)
                  (token-span token))]))

;; parse-list : cursor? -> scheme-list?
;;   Parse an ordinary delimited list, including a dotted tail when present.
(define (parse-list state)
  (define opening-token (cursor-read! state))
  (define opener (scheme-derived-token-text opening-token))
  (define expected-close (matching-close opener))
  (define items '())
  (define tail #f)
  (let loop ()
    (skip-trivia! state)
    (define token (cursor-peek state))
    (cond
      [(not token)
       (define opening-span (token-span opening-token))
       (scheme-list opener (reverse items) tail opening-span)]
      [(and (close-delimiter? token)
            (string=? (scheme-derived-token-text token) expected-close))
       (define close-token (cursor-read! state))
       (define opening-span (token-span opening-token))
       (define close-span (token-span close-token))
       (scheme-list opener (reverse items) tail
                    (scheme-source-span (scheme-source-span-start opening-span)
                                        (scheme-source-span-end close-span)))]
      [(and (string=? (scheme-derived-token-text token) ".") (not tail))
       (cursor-read! state)
       (set! tail (parse-form state))
       (loop)]
      [else
       (define form (parse-form state))
       (when form (set! items (cons form items)))
       (loop)])))

;; parse-vector : cursor? symbol? -> scheme-vector?
;;   Parse a vector or bytevector delimited by a lexer-recognized opener.
(define (parse-vector state kind)
  (define opening-token (cursor-read! state))
  (define items '())
  (let loop ()
    (skip-trivia! state)
    (define token (cursor-peek state))
    (cond
      [(not token)
       (scheme-vector kind (reverse items) (token-span opening-token))]
      [(and (close-delimiter? token) (string=? (scheme-derived-token-text token) ")"))
       (define close-token (cursor-read! state))
       (define opening-span (token-span opening-token))
       (define close-span (token-span close-token))
       (scheme-vector kind (reverse items)
                      (scheme-source-span (scheme-source-span-start opening-span)
                                          (scheme-source-span-end close-span)))]
      [else
       (define form (parse-form state))
       (when form (set! items (cons form items)))
       (loop)])))

;; parse-abbreviation : cursor? -> scheme-abbreviation?
;;   Parse a reader prefix and the datum it abbreviates.
(define (parse-abbreviation state)
  (define prefix-token (cursor-read! state))
  (define datum (parse-form state))
  (define prefix-span (token-span prefix-token))
  (define end
    (cond
      [(and datum (scheme-datum? datum))
       (cond
         [(scheme-list? datum) (scheme-source-span-end (scheme-list-span datum))]
         [(scheme-vector? datum) (scheme-source-span-end (scheme-vector-span datum))]
         [(scheme-abbreviation? datum) (scheme-source-span-end (scheme-abbreviation-span datum))]
         [(scheme-atom? datum) (scheme-source-span-end (scheme-atom-span datum))]
         [(scheme-comment? datum) (scheme-source-span-end (scheme-comment-span datum))]
         [else (scheme-source-span-end (scheme-recovery-span datum))])]
      [else (scheme-source-span-end prefix-span)]))
  (scheme-abbreviation (scheme-derived-token-text prefix-token)
                       (or datum (scheme-recovery "missing abbreviated datum" "" prefix-span))
                       (scheme-source-span (scheme-source-span-start prefix-span) end)))

;; parse-scheme-source : string? symbol? -> scheme-document?
;;   Parse source with lexer support for the selected Scheme dialect.
(define (parse-scheme-source source dialect)
  (unless (member dialect scheme-dialects)
    (raise-arguments-error 'parse-scheme-source "unknown Scheme dialect" "dialect" dialect))
  (define state
    (cursor (list->vector (scheme-string->derived-tokens source #:dialect dialect)) 0))
  (define forms '())
  (let loop ()
    (define form (parse-form state))
    (when form
      (set! forms (cons form forms))
      (loop)))
  (scheme-document (reverse forms) source dialect (scheme-source-span 0 (string-length source))))
