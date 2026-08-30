#lang racket/base

;;;
;;; Scheme Parser
;;;

;; Public entry points for a source-preserving Scheme reader parser.

;; parse-scheme      : (or/c string? input-port?) keyword-arguments -> scheme-document?
;;   Parse Scheme source for a selected reader dialect.
;; parse-scheme-port : input-port? keyword-arguments -> scheme-document?
;;   Parse Scheme input from a port.
;; serialize-scheme  : scheme-document? -> string?
;;   Reproduce the original Scheme source exactly.

(provide parse-scheme
         parse-scheme-port
         serialize-scheme
         scheme-document-datums
         scheme-find-atoms-by-text
         scheme-parser-dialects
         (all-from-out "private/scheme-ast.rkt"))

(require racket/list
         racket/port
         lexers/scheme
         "private/scheme-ast.rkt"
         "private/scheme-parser.rkt")

;; scheme-parser-dialects : (listof symbol?)
;;   Supported Scheme report and implementation reader dialects.
(define scheme-parser-dialects scheme-dialects)

;; parse-scheme-port : input-port? [#:dialect symbol?] -> scheme-document?
;;   Parse Scheme input from a port using the selected reader dialect.
(define (parse-scheme-port in #:dialect [dialect 'r5rs])
  (unless (input-port? in)
    (raise-argument-error 'parse-scheme-port "input-port?" in))
  (parse-scheme-source (port->string in) dialect))

;; parse-scheme : (or/c string? input-port?) [#:dialect symbol?] -> scheme-document?
;;   Parse Scheme source supplied as a string or input port.
(define (parse-scheme source #:dialect [dialect 'r5rs])
  (cond
    [(string? source) (parse-scheme-source source dialect)]
    [(input-port? source) (parse-scheme-port source #:dialect dialect)]
    [else (raise-argument-error 'parse-scheme "(or/c string? input-port?)" source)]))

;; serialize-scheme : scheme-document? -> string?
;;   Reproduce the original source retained by a parsed Scheme document.
(define (serialize-scheme document)
  (unless (scheme-document? document)
    (raise-argument-error 'serialize-scheme "scheme-document?" document))
  (scheme-document-source document))

;; scheme-document-datums : scheme-document? -> (listof scheme-datum?)
;;   Return all reader forms, comments, and recoveries in source order.
(define (scheme-document-datums document) (scheme-document-forms document))

;; scheme-find-atoms-by-text : scheme-document? string? -> (listof scheme-atom?)
;;   Find atomic reader forms whose exact source text equals the requested text.
(define (scheme-find-atoms-by-text document text)
  (define found '())
  (define (visit datum)
    (cond
      [(scheme-atom? datum)
       (when (string=? (scheme-atom-text datum) text)
         (set! found (cons datum found)))]
      [(scheme-list? datum)
       (for ([item (in-list (scheme-list-items datum))]) (visit item))
       (when (scheme-list-tail datum) (visit (scheme-list-tail datum)))]
      [(scheme-vector? datum)
       (for ([item (in-list (scheme-vector-items datum))]) (visit item))]
      [(scheme-abbreviation? datum) (visit (scheme-abbreviation-datum datum))]))
  (for ([datum (in-list (scheme-document-datums document))]) (visit datum))
  (reverse found))

(module+ test
  (require rackunit)

  (define source
    ";; reader forms\n(define data '(#u8(1 2) #:name \"ok\"))\n")
  (define document (parse-scheme source #:dialect 'guile))
  (define forms (scheme-document-datums document))

  (check-true (scheme-document? document))
  (check-equal? (serialize-scheme document) source)
  (check-equal? (scheme-document-dialect document) 'guile)
  (check-true (scheme-comment? (first forms)))
  (check-true (scheme-list? (second forms)))
  (check-equal? (length (scheme-find-atoms-by-text document "define")) 1)
  (define dialect-examples
    (list (cons 'r5rs "(define pair '(a . b))\n")
          (cons 'r6rs "#!r6rs\n#u8(1 2 3)\n")
          (cons 'r7rs "#!fold-case\n#0=(a . #0#)\n")
          (cons 'chez "#3(1 2 3)\n")
          (cons 'guile "(display #:name #'symbol)\n")
          (cons 'chicken "#$foreign\n")
          (cons 'gambit "(display name:)\n")))
  (for ([example (in-list dialect-examples)])
    (define dialect (car example))
    (define dialect-source (cdr example))
    (define dialect-document (parse-scheme dialect-source #:dialect dialect))
    (check-true (scheme-document? dialect-document))
    (check-equal? (serialize-scheme dialect-document) dialect-source)))
