#lang racket/base

;;;
;;; CSS Recovery Helpers
;;;
;;
;; Helpers for inspecting recovery nodes in parsed stylesheets.

(provide css-recovery-nodes
         css-has-recovery?
         css-recovery-summary)

(require racket/list
         "css-ast.rkt")

;; css-recovery-nodes : css-stylesheet? -> list?
;;   Collect all recovery nodes in pre-order.
(define (css-recovery-nodes stylesheet)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-recovery-nodes "css-stylesheet?" stylesheet))
  (append-map recovery-nodes-in-node (css-stylesheet-rules stylesheet)))

;; css-has-recovery? : css-stylesheet? -> boolean?
;;   Determine whether a stylesheet contains any recovery nodes.
(define (css-has-recovery? stylesheet)
  (pair? (css-recovery-nodes stylesheet)))

;; css-recovery-summary : css-stylesheet? -> list?
;;   Summarize recovery nodes by kind.
(define (css-recovery-summary stylesheet)
  (define nodes
    (css-recovery-nodes stylesheet))
  (define counts
    (make-hash))
  (for ([node (in-list nodes)])
    (hash-update! counts (css-recovery-kind node) add1 0))
  (sort (hash->list counts)
        symbol<?
        #:key car))

;; recovery-nodes-in-node : any/c -> list?
;;   Collect recovery nodes recursively.
(define (recovery-nodes-in-node node)
  (cond
    [(css-recovery? node)
     (list node)]
    [(css-style-rule? node)
     (append-map recovery-nodes-in-node (css-style-rule-block node))]
    [(css-at-rule? node)
     (if (list? (css-at-rule-block node))
         (append-map recovery-nodes-in-node (css-at-rule-block node))
         '())]
    [else
     '()]))

(module+ test
  (require rackunit)

  (define stylesheet
    (css-stylesheet
     (list (css-style-rule '("body")
                           (list (css-recovery 'declaration "bad declaration" "color red;" #f '()))
                           "body"
                           #f)
           (css-recovery 'statement "bad statement" "/" #f '()))
     #f
     #f))
  (check-true (css-has-recovery? stylesheet))
  (check-equal? (length (css-recovery-nodes stylesheet)) 2)
  (check-equal? (css-recovery-summary stylesheet)
                '((declaration . 1) (statement . 1))))
