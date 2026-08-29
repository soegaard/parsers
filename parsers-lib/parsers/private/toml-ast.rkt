#lang racket/base

;;;
;;; TOML AST
;;;

;; Explicit source-preserving data structures for TOML documents.

;; toml-source-span : exact-nonnegative-integer? exact-nonnegative-integer? -> toml-source-span?
;;   Represent a half-open source range.
;; toml-document    : list? string? toml-source-span? -> toml-document?
;;   Represent one complete TOML document.
;; toml-table       : toml-key? boolean? list? toml-source-span? -> toml-table?
;;   Represent a table or array-of-tables header and its entries.
;; toml-key-value   : toml-key? toml-value? toml-source-span? -> toml-key-value?
;;   Represent one key/value assignment.
;; toml-value?      : any/c -> boolean?
;;   Recognize a parsed TOML value node.

(provide (struct-out toml-source-span)
         (struct-out toml-document)
         (struct-out toml-table)
         (struct-out toml-key-value)
         (struct-out toml-key)
         (struct-out toml-array)
         (struct-out toml-inline-table)
         (struct-out toml-string)
         (struct-out toml-boolean)
         (struct-out toml-number)
         (struct-out toml-date-time)
         (struct-out toml-bare-value)
         (struct-out toml-comment)
         (struct-out toml-recovery)
         toml-value?)

(struct toml-source-span (start end) #:transparent)
(struct toml-document (items source span) #:transparent)
(struct toml-table (key array? entries span) #:transparent #:mutable)
(struct toml-key-value (key value span) #:transparent)
(struct toml-key (parts text span) #:transparent)
(struct toml-array (values span) #:transparent)
(struct toml-inline-table (entries span) #:transparent)
(struct toml-string (text value literal? multiline? span) #:transparent)
(struct toml-boolean (text value span) #:transparent)
(struct toml-number (text span) #:transparent)
(struct toml-date-time (text span) #:transparent)
(struct toml-bare-value (text span) #:transparent)
(struct toml-comment (text span) #:transparent)
(struct toml-recovery (reason text span) #:transparent)

;; toml-value? : any/c -> boolean?
;;   Recognize a parsed TOML value node.
(define (toml-value? value)
  (or (toml-string? value)
      (toml-boolean? value)
      (toml-number? value)
      (toml-date-time? value)
      (toml-bare-value? value)
      (toml-array? value)
      (toml-inline-table? value)))
