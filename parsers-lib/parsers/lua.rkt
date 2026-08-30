#lang racket/base

;;;
;;; Lua Parser
;;;

;; Public entry points for a source-preserving Lua parser.

;; parse-lua      : (or/c string? input-port?) -> lua-chunk?
;;   Parse Lua source into an explicit chunk AST.
;; parse-lua-port : input-port? -> lua-chunk?
;;   Parse Lua input from a port.
;; serialize-lua  : lua-chunk? -> string?
;;   Reproduce the original Lua source exactly.

(provide parse-lua
         parse-lua-port
         serialize-lua
         lua-chunk-statements
         lua-find-statements-by-kind
         (all-from-out "private/lua-ast.rkt"))

(require racket/list
         racket/port
         "private/lua-ast.rkt"
         "private/lua-parser.rkt")

;; parse-lua-port : input-port? -> lua-chunk?
;;   Parse Lua input from a port.
(define (parse-lua-port in)
  (unless (input-port? in)
    (raise-argument-error 'parse-lua-port "input-port?" in))
  (parse-lua-source (port->string in)))

;; parse-lua : (or/c string? input-port?) -> lua-chunk?
;;   Parse Lua source supplied as a string or input port.
(define (parse-lua source)
  (cond
    [(string? source) (parse-lua-source source)]
    [(input-port? source) (parse-lua-port source)]
    [else (raise-argument-error 'parse-lua "(or/c string? input-port?)" source)]))

;; serialize-lua : lua-chunk? -> string?
;;   Reproduce the original source retained by a parsed Lua chunk.
(define (serialize-lua chunk)
  (unless (lua-chunk? chunk)
    (raise-argument-error 'serialize-lua "lua-chunk?" chunk))
  (lua-chunk-source chunk))

;; lua-chunk-statements : lua-chunk? -> (listof lua-statement?)
;;   Extract statement-like forms in source order.
(define (lua-chunk-statements chunk)
  (for/list ([form (in-list (lua-chunk-forms chunk))]
             #:when (lua-statement? form))
    form))

;; lua-find-statements-by-kind : lua-chunk? symbol? -> (listof lua-statement?)
;;   Find statement-like forms by their exact leading-kind classification.
(define (lua-find-statements-by-kind chunk kind)
  (for/list ([statement (in-list (lua-chunk-statements chunk))]
             #:when (eq? (lua-statement-kind statement) kind))
    statement))

(module+ test
  (require rackunit)

  (define source
    "-- greeting\nlocal message = \"hello\"\nfunction greet(name)\n  return message .. name\nend\n")
  (define chunk (parse-lua source))

  (check-true (lua-chunk? chunk))
  (check-equal? (serialize-lua chunk) source)
  (check-equal? (length (lua-find-statements-by-kind chunk 'local)) 1)
  (check-equal? (length (lua-find-statements-by-kind chunk 'function)) 1)
  (check-equal? (length (lua-find-statements-by-kind chunk 'return)) 1)
  (check-equal? (length (lua-find-statements-by-kind chunk 'end)) 1)
  (check-true (lua-chunk? (parse-lua (open-input-string "return true\n")))))
