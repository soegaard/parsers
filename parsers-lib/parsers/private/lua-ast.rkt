#lang racket/base

;;;
;;; Lua AST
;;;

;; Explicit source-preserving data structures for Lua chunks.

;; lua-source-span : exact-nonnegative-integer? exact-nonnegative-integer? -> lua-source-span?
;;   Represent a half-open Lua source range.
;; lua-chunk       : list? string? lua-source-span? -> lua-chunk?
;;   Represent a complete Lua source file.
;; lua-statement   : symbol? string? list? lua-source-span? -> lua-statement?
;;   Represent one statement-like source form.
;; lua-token       : symbol? (listof symbol?) string? lua-source-span? -> lua-token?
;;   Represent one lexer-derived token in a parsed form.

(provide (struct-out lua-source-span)
         (struct-out lua-chunk)
         (struct-out lua-statement)
         (struct-out lua-comment)
         (struct-out lua-token)
         (struct-out lua-recovery))

(struct lua-source-span (start end) #:transparent)
(struct lua-chunk (forms source span) #:transparent)
(struct lua-statement (kind text tokens span) #:transparent)
(struct lua-comment (text span) #:transparent)
(struct lua-token (kind tags text span) #:transparent)
(struct lua-recovery (reason text span) #:transparent)
