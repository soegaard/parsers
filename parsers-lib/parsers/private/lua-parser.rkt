#lang racket/base

;;;
;;; Lua Parser
;;;

;; Build source-preserving Lua chunk structure from lexers/lua derived tokens.

;; parse-lua-source : string? -> lua-chunk?
;;   Parse Lua source into a chunk of statement-like forms and comments.

(provide parse-lua-source)

(require parser-tools/lex
         racket/list
         racket/string
         lexers/lua
         "lua-ast.rkt")

;; token-span : lua-derived-token? -> lua-source-span?
;;   Convert lexer source positions to zero-based half-open offsets.
(define (token-span token)
  (lua-source-span
   (sub1 (position-offset (lua-derived-token-start token)))
   (sub1 (position-offset (lua-derived-token-end token)))))

;; derived-token-kind : lua-derived-token? -> symbol?
;;   Classify a lexer-derived token into a stable parser-facing token kind.
(define (derived-token-kind token)
  (cond
    [(lua-derived-token-has-tag? token 'lua-keyword) 'keyword]
    [(lua-derived-token-has-tag? token 'lua-constant) 'constant]
    [(lua-derived-token-has-tag? token 'lua-identifier) 'identifier]
    [(lua-derived-token-has-tag? token 'lua-string-literal) 'string]
    [(lua-derived-token-has-tag? token 'lua-number) 'number]
    [(lua-derived-token-has-tag? token 'lua-operator) 'operator]
    [(lua-derived-token-has-tag? token 'lua-delimiter) 'delimiter]
    [else 'unknown]))

;; derived->token : lua-derived-token? -> lua-token?
;;   Convert one non-trivia lexer token to a parser AST token.
(define (derived->token token)
  (lua-token (derived-token-kind token)
             (lua-derived-token-tags token)
             (lua-derived-token-text token)
             (token-span token)))

;; statement-kind : (listof lua-token?) -> symbol?
;;   Categorize a statement-like form from its leading Lua keyword or token.
(define (statement-kind tokens)
  (cond
    [(null? tokens) 'empty]
    [else
     (define first-token (car tokens))
     (define text (lua-token-text first-token))
     (cond
       [(eq? (lua-token-kind first-token) 'keyword)
        (string->symbol text)]
       [(string=? text "::") 'label]
       [else 'expression-or-assignment])]))

;; tokens->text : string? (listof lua-token?) -> string?
;;   Extract the original source covering a contiguous statement token sequence.
(define (tokens->text source tokens)
  (define start (lua-source-span-start (lua-token-span (car tokens))))
  (define end (lua-source-span-end (lua-token-span (last tokens))))
  (substring source start end))

;; tokens->statement : string? (listof lua-token?) -> lua-statement?
;;   Construct a statement-like AST node from contiguous non-trivia tokens.
(define (tokens->statement source tokens)
  (define first-span (lua-token-span (car tokens)))
  (define last-span (lua-token-span (last tokens)))
  (lua-statement (statement-kind tokens)
                 (tokens->text source tokens)
                 tokens
                 (lua-source-span (lua-source-span-start first-span)
                                  (lua-source-span-end last-span))))

;; whitespace-has-newline? : lua-derived-token? -> boolean?
;;   Determine whether a trivia token ends one or more source lines.
(define (whitespace-has-newline? token)
  (or (string-contains? (lua-derived-token-text token) "\n")
      (string-contains? (lua-derived-token-text token) "\r")))

;; parse-lua-source : string? -> lua-chunk?
;;   Parse lexer-derived Lua tokens into source-order structural forms.
(define (parse-lua-source source)
  (define derived-tokens (lua-string->derived-tokens source))
  (define forms '())
  (define statement-tokens '())
  ;; Flush before comments and physical line endings to preserve readable source order.
  (define (flush-statement!)
    (unless (null? statement-tokens)
      (set! forms (append forms (list (tokens->statement source statement-tokens))))
      (set! statement-tokens '())))
  (for ([token (in-list derived-tokens)])
    (cond
      [(lua-derived-token-has-tag? token 'lua-whitespace)
       (when (whitespace-has-newline? token) (flush-statement!))]
      [(lua-derived-token-has-tag? token 'lua-comment)
       (flush-statement!)
       (set! forms
             (append forms
                     (list (lua-comment (lua-derived-token-text token)
                                        (token-span token)))))]
      [(lua-derived-token-has-tag? token 'malformed-token)
       (flush-statement!)
       (set! forms
             (append forms
                     (list (lua-recovery "malformed Lua token"
                                         (lua-derived-token-text token)
                                         (token-span token)))))]
      [else (set! statement-tokens (append statement-tokens (list (derived->token token))))]))
  (flush-statement!)
  (lua-chunk forms source (lua-source-span 0 (string-length source))))
