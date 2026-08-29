#lang racket/base

;;;
;;; TOML Parser
;;;

;; Public entry points for a source-preserving TOML parser.

;; parse-toml      : (or/c string? input-port?) -> toml-document?
;;   Parse TOML source into an explicit document AST.
;; parse-toml-port : input-port? -> toml-document?
;;   Parse TOML input from a port.
;; serialize-toml  : toml-document? -> string?
;;   Reproduce the original TOML source exactly.

(provide parse-toml
         parse-toml-port
         serialize-toml
         toml-document-find-tables
         toml-table-find-values
         (all-from-out "private/toml-ast.rkt"))

(require racket/list
         racket/port
         racket/string
         "private/toml-ast.rkt"
         "private/toml-parser.rkt")

;; parse-toml-port : input-port? -> toml-document?
;;   Parse TOML input from a port.
(define (parse-toml-port in)
  (unless (input-port? in)
    (raise-argument-error 'parse-toml-port "input-port?" in))
  (parse-toml-source (port->string in)))

;; parse-toml : (or/c string? input-port?) -> toml-document?
;;   Parse TOML source supplied as a string or input port.
(define (parse-toml source)
  (cond
    [(string? source) (parse-toml-source source)]
    [(input-port? source) (parse-toml-port source)]
    [else (raise-argument-error 'parse-toml "(or/c string? input-port?)" source)]))

;; serialize-toml : toml-document? -> string?
;;   Reproduce the original source retained by a parsed TOML document.
(define (serialize-toml document)
  (unless (toml-document? document)
    (raise-argument-error 'serialize-toml "toml-document?" document))
  (toml-document-source document))

;; toml-document-find-tables : toml-document? (or/c string? (listof string?)) -> (listof toml-table?)
;;   Find table headers with an exact dotted key path.
(define (toml-document-find-tables document path)
  (define expected-parts
    (if (string? path) (string-split path ".") path))
  (for/list ([item (in-list (toml-document-items document))]
             #:when (and (toml-table? item)
                         (equal? (toml-key-parts (toml-table-key item)) expected-parts)))
    item))

;; toml-table-find-values : toml-table? (or/c string? (listof string?)) -> (listof toml-value?)
;;   Find values assigned by an exact dotted key path in one table.
(define (toml-table-find-values table path)
  (define expected-parts
    (if (string? path) (string-split path ".") path))
  (for/list ([item (in-list (toml-table-entries table))]
             #:when (and (toml-key-value? item)
                         (equal? (toml-key-parts (toml-key-value-key item)) expected-parts)))
    (toml-key-value-value item)))

(module+ test
  (require rackunit)

  (define source
    "# cargo\n[package]\nname = \"parsers\"\nfeatures = [\"toml\", \"css\"]\nmeta = { active = true }\n")
  (define document (parse-toml source))
  (define package (first (toml-document-find-tables document "package")))

  (check-true (toml-document? document))
  (check-equal? (serialize-toml document) source)
  (check-equal? (toml-key-value-value (first (toml-table-entries package)))
                (toml-string "\"parsers\"" "parsers" #f #f (toml-source-span 25 34)))
  (check-equal? (length (toml-table-find-values package "features")) 1)
  (check-true (toml-inline-table? (first (toml-table-find-values package "meta"))))
  (check-true (toml-document? (parse-toml (open-input-string "enabled = true\n")))))
