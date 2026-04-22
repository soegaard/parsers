#lang racket/base

;;;
;;; CSS Parser Errors
;;;
;;
;; Shared error constructors for the CSS parser stack.

(provide exn:fail:css?
         exn:fail:css-source
         exn:fail:css-detail
         raise-css-not-implemented
         raise-css-parse-error)

(struct exn:fail:css exn:fail (source detail)
  #:transparent)

(define (raise-css-not-implemented who source detail)
  (raise
   (exn:fail:css
    (format "~a: CSS parsing is not implemented yet" who)
    (current-continuation-marks)
    source
    detail)))

;; raise-css-parse-error : symbol? any/c any/c any/c any/c string? -> none
;;   Raise a CSS parse error with structured detail.
(define (raise-css-parse-error who token-name token-value start-pos end-pos message)
  (raise
   (exn:fail:css
    (format "~a: ~a" who message)
    (current-continuation-marks)
    token-value
    (list (cons 'token-name token-name)
          (cons 'token-value token-value)
          (cons 'start-pos start-pos)
          (cons 'end-pos end-pos)))))

(module+ test
  (require rackunit)

  (check-exn
   exn:fail:css?
   (lambda ()
     (raise-css-not-implemented 'parse-css "body {}" "placeholder parser")))
  (check-exn
   exn:fail:css?
   (lambda ()
     (raise-css-parse-error 'parse-css 'IDENT "body" #f #f "parse failed"))))
