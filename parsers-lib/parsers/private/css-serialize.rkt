#lang racket/base

;;;
;;; CSS Serializer
;;;
;;
;; Serialize the current CSS AST back to CSS text.

(provide serialize-stylesheet
         serialize-css
         serialize-stylesheet/normalized)

(require racket/list
         racket/string
         "css-ast.rkt")

;; serialize-css : css-stylesheet? keyword-arguments -> string?
;;   Alias for serialize-stylesheet.
(define (serialize-css stylesheet #:preserve-source? [preserve-source? #f])
  (serialize-stylesheet stylesheet #:preserve-source? preserve-source?))

;; serialize-stylesheet : css-stylesheet? keyword-arguments -> string?
;;   Serialize a stylesheet AST to CSS text.
(define (serialize-stylesheet stylesheet #:preserve-source? [preserve-source? #f])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'serialize-stylesheet "css-stylesheet?" stylesheet))
  (cond
    [(and preserve-source?
          (string? (css-stylesheet-source stylesheet)))
     (css-stylesheet-source stylesheet)]
    [else
     (serialize-stylesheet/normalized stylesheet)]))

;; serialize-stylesheet/normalized : css-stylesheet? -> string?
;;   Serialize a stylesheet AST with normalized spacing.
(define (serialize-stylesheet/normalized stylesheet)
  (string-join (map serialize-node (css-stylesheet-rules stylesheet)) "\n"))

;; serialize-node : any/c -> string?
;;   Serialize one AST node.
(define (serialize-node node)
  (cond
    [(css-comment? node)
     (css-comment-text node)]
    [(css-recovery? node)
     (css-recovery-text node)]
    [(css-style-rule? node)
     (format "~a { ~a }"
             (css-style-rule-raw-selector node)
             (serialize-block-items (css-style-rule-block node)))]
    [(css-at-rule? node)
     (serialize-at-rule node)]
    [(css-declaration? node)
     (serialize-declaration node)]
    [else
     (raise-argument-error 'serialize-node "known CSS AST node" node)]))

;; serialize-at-rule : css-at-rule? -> string?
;;   Serialize one at-rule.
(define (serialize-at-rule rule)
  (define name
    (css-at-rule-name rule))
  (define prelude
    (css-at-rule-prelude rule))
  (define header
    (string-trim (string-join (filter non-empty-string? (list name prelude)) " ")))
  (cond
    [(not (css-at-rule-block rule))
     (string-append header ";")]
    [else
     (format "~a { ~a }"
             header
             (serialize-block-items (css-at-rule-block rule)))]))

;; serialize-block-items : list? -> string?
;;   Serialize a block item list with normalized spacing.
(define (serialize-block-items items)
  (string-join (map serialize-node items) " "))

;; serialize-declaration : css-declaration? -> string?
;;   Serialize one declaration.
(define (serialize-declaration declaration)
  (string-append (css-declaration-name declaration)
                 ": "
                 (css-declaration-value declaration)
                 ";"))

;; non-empty-string? : any/c -> boolean?
;;   Determine whether a value is a non-empty string.
(define (non-empty-string? v)
  (and (string? v)
       (not (string=? v ""))))

(module+ test
  (require rackunit)

  (define stylesheet
    (css-stylesheet
     (list (css-comment "/* top */" #f)
           (css-style-rule '("body")
                           (list (css-comment "/* inner */" #f)
                                 (css-declaration "color" "red" #f #f))
                           "body"
                           #f)
           (css-at-rule "@import" "url(\"x.css\")" #f #f))
     #f
     #f))

  (check-equal?
   (serialize-stylesheet stylesheet)
   "/* top */\nbody { /* inner */ color: red; }\n@import url(\"x.css\");")
  (check-equal?
   (serialize-stylesheet
    (css-stylesheet '() "body { color: red; }" #f)
    #:preserve-source? #t)
   "body { color: red; }"))
