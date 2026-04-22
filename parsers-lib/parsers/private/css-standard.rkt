#lang racket/base

;;;
;;; CSS Standard Tags
;;;
;;
;; Internal representation of public CSS parser targets.

(provide css-standard?
         css-standards)

(define css-standards '(current))

(define (css-standard? v)
  (and (symbol? v)
       (memq v css-standards)))

(module+ test
  (require rackunit)

  (check-true (css-standard? 'current))
  (check-false (css-standard? 'css-3)))
