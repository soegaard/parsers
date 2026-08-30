#lang racket/base

;;;
;;; Scheme AST
;;;

;; Explicit source-preserving data structures for Scheme reader forms.

;; scheme-source-span : exact-nonnegative-integer? exact-nonnegative-integer? -> scheme-source-span?
;;   Represent a half-open Scheme source range.
;; scheme-document    : list? string? symbol? scheme-source-span? -> scheme-document?
;;   Represent a complete Scheme source document.
;; scheme-list        : string? list? (or/c scheme-datum? #f) scheme-source-span? -> scheme-list?
;;   Represent a parenthesized, bracketed, or braced reader form.
;; scheme-vector      : symbol? list? scheme-source-span? -> scheme-vector?
;;   Represent a vector or bytevector reader form.
;; scheme-atom        : symbol? string? (listof symbol?) scheme-source-span? -> scheme-atom?
;;   Represent an atomic Scheme datum.

(provide (struct-out scheme-source-span)
         (struct-out scheme-document)
         (struct-out scheme-list)
         (struct-out scheme-vector)
         (struct-out scheme-abbreviation)
         (struct-out scheme-atom)
         (struct-out scheme-comment)
         (struct-out scheme-recovery)
         scheme-datum?)

(struct scheme-source-span (start end) #:transparent)
(struct scheme-document (forms source dialect span) #:transparent)
(struct scheme-list (opener items tail span) #:transparent)
(struct scheme-vector (kind items span) #:transparent)
(struct scheme-abbreviation (prefix datum span) #:transparent)
(struct scheme-atom (kind text tags span) #:transparent)
(struct scheme-comment (kind text span) #:transparent)
(struct scheme-recovery (reason text span) #:transparent)

;; scheme-datum? : any/c -> boolean?
;;   Recognize a parsed Scheme datum or preserved comment/recovery node.
(define (scheme-datum? value)
  (or (scheme-list? value)
      (scheme-vector? value)
      (scheme-abbreviation? value)
      (scheme-atom? value)
      (scheme-comment? value)
      (scheme-recovery? value)))
