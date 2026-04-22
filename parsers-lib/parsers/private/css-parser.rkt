#lang racket/base

;;;
;;; CSS Parser Driver
;;;
;;
;; Internal parser driver and home of parser integration.

(provide css-parser?
         make-css-parser
         parse-css-port)

(require racket/port
         "css-ast.rkt"
         "css-errors.rkt"
         "css-standard.rkt"
         "css-structural.rkt")

(struct css-parser-wrapper (proc)
  #:transparent
  #:property prop:procedure
  (struct-field-index proc))

(define (css-parser? v)
  (css-parser-wrapper? v))

;; parse-css-port : input-port? keyword-arguments -> css-stylesheet?
;;   Parse CSS from an input port using the current parser scaffold.
(define (parse-css-port in #:standard [standard 'current])
  (cond
    [(eq? standard 'current)
     (define source
       (port->string in))
     (with-handlers ([exn:fail:css?
                      (lambda (e)
                        (raise e))]
                     [exn:fail:read?
                      (lambda (e)
                        (raise-css-not-implemented
                         'parse-css
                         #f
                         (exn-message e)))])
       (parse-css-structurally source))]
    [else
     (raise-css-not-implemented
      'parse-css
      #f
      (format "unsupported CSS standard target ~s" standard))]))

;; make-css-parser : keyword-arguments -> parser-procedure?
;;   Construct a CSS parser procedure.
(define (make-css-parser #:standard [standard 'current])
  (unless (css-standard? standard)
    (raise-argument-error 'make-css-parser "css-standard?" standard))
  (css-parser-wrapper
   (lambda (in)
     (parse-css-port in #:standard standard))))

(module+ test
  (require rackunit)

  (check-true (css-parser? (make-css-parser #:standard 'current)))
  (check-true (css-stylesheet? (parse-css-port (open-input-string "")
                                              #:standard 'current)))
  (check-true (css-stylesheet? (parse-css-port (open-input-string "body {}")
                                              #:standard 'current))))
