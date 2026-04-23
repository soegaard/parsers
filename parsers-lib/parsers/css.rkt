#lang racket/base

;;;
;;; CSS Parser
;;;
;;
;; Public entry points for the CSS parser.

(provide css-parser?
         make-css-parser
         parse-css
         parse-stylesheet
         serialize-css
         serialize-stylesheet
         serialize-stylesheet/normalized
         css-map-rules
         css-map-at-rules
         css-map-selectors
         css-map-declarations-in-selectors
         css-map-declarations
         css-update-declaration-values
         css-update-declaration-values/preserve-source
         css-remove-declarations
         css-remove-declarations/preserve-source
         css-append-declaration
         css-append-declaration/preserve-source
         css-append-declaration-by-pseudo
         css-append-declaration-by-pseudo/preserve-source
         css-append-declaration-by-class
         css-append-declaration-by-class/preserve-source
         css-append-declaration-by-attribute
         css-append-declaration-by-attribute/preserve-source
         css-rename-class
         css-prefix-selectors
         css-rewrite-media-queries
         css-rewrite-supports-conditions
         css-rewrite-custom-properties
         css-split-grouped-selectors
         css-clone-rule
         css-insert-rule-before
         css-insert-rule-after
         css-remove-rules
         css-remove-at-rules
         css-wrap-rules-in-media
         css-wrap-rules-in-supports
         css-merge-adjacent-rules
         css-dedupe-declarations
         css-sort-declarations
         css-rename-custom-property
         css-rewrite-var-functions
         css-rename-keyframes
         css-rewrite-imports
         css-rewrite-font-face
         css-rewrite-url-values
         css-filter-comments
         css-hoist-nested-rules
         css-lower-nesting
         css-rewrite-attribute-selectors
         css-rewrite-pseudos
         css-rewrite-selector-structure
         css-update-declaration-values-in-media-feature
         css-update-declaration-values-in-media-feature/preserve-source
         css-remove-declarations-in-media-feature
         css-remove-declarations-in-media-feature/preserve-source
         css-update-declaration-values-in-supports-feature
         css-update-declaration-values-in-supports-feature/preserve-source
         css-remove-declarations-in-supports-feature
         css-remove-declarations-in-supports-feature/preserve-source
         css-flatten-rules
         css-find-rules-by-selector-group
         css-find-rules-by-raw-selector
         css-find-declarations-in-selector-group
         css-find-declarations-in-selector-groups
         css-collect-custom-properties-in-selector-group
         css-collect-custom-properties-in-selector-groups
         css-compute-style-for-selector-group
         css-compute-custom-properties-for-selector-group
         css-compute-style-trace?
         css-compute-style-trace-selector-group
         css-compute-style-trace-matched-rules
         css-compute-style-trace-property-results
         css-compute-style-trace-custom-property-results
         css-compute-style-trace-var-resolutions
         css-compute-matched-rule?
         css-compute-matched-rule-selector-group
         css-compute-matched-rule-specificity
         css-compute-matched-rule-source-order
         css-compute-matched-rule-rule
         css-compute-property-result?
         css-compute-property-result-name
         css-compute-property-result-candidates
         css-compute-property-result-winner
         css-compute-candidate?
         css-compute-candidate-name
         css-compute-candidate-value
         css-compute-candidate-important?
         css-compute-candidate-specificity
         css-compute-candidate-source-order
         css-compute-candidate-declaration
         css-compute-candidate-matched-rule
         css-compute-candidate-source-name
         css-compute-var-resolution?
         css-compute-var-resolution-name
         css-compute-var-resolution-raw-value
         css-compute-var-resolution-resolved-value
         css-compute-var-resolution-references
         css-compute-var-resolution-cycle?
         css-find-declarations
         css-query-selector
         css-find-rules-by-pseudo
         css-find-media-queries
         css-find-supports-features
         css-recovery-nodes
         css-has-recovery?
         css-recovery-summary
         css-selector-compounds
         current-css-standard
         css-source-span?
         css-source-span-start
         css-source-span-end
         css-stylesheet?
         css-stylesheet-rules
         css-stylesheet-source
         css-stylesheet-span
         css-comment?
         css-comment-text
         css-comment-span
         css-recovery?
         css-recovery-kind
         css-recovery-reason
         css-recovery-text
         css-recovery-span
         css-recovery-detail
         css-selector?
         css-selector-text
         css-selector-span
         css-selector-parts
         css-selector-compound?
         css-selector-compound-items
         css-selector-compound-span
         css-selector-combinator?
         css-selector-combinator-text
         css-selector-combinator-span
         css-selector-type?
         css-selector-type-name
         css-selector-type-span
         css-selector-namespaced-type?
         css-selector-namespaced-type-namespace
         css-selector-namespaced-type-name
         css-selector-namespaced-type-span
         css-selector-class?
         css-selector-class-name
         css-selector-class-span
         css-selector-id?
         css-selector-id-name
         css-selector-id-span
         css-selector-attribute?
         css-selector-attribute-derived-details
         css-selector-attribute-details?
         css-selector-attribute-details-namespace
         css-selector-attribute-details-name
         css-selector-attribute-details-matcher
         css-selector-attribute-details-value
         css-selector-attribute-details-modifier
         css-selector-attribute-details-text
         css-selector-attribute-details-span
         css-selector-attribute-identifier-value?
         css-selector-attribute-identifier-value-text
         css-selector-attribute-identifier-value-value
         css-selector-attribute-identifier-value-span
         css-selector-attribute-string-value?
         css-selector-attribute-string-value-text
         css-selector-attribute-string-value-value
         css-selector-attribute-string-value-span
         css-selector-attribute-name
         css-selector-attribute-matcher
         css-selector-attribute-value
         css-selector-attribute-modifier
         css-selector-attribute-text
         css-selector-attribute-span
         css-at-rule-prelude-derived-details
         css-media-prelude-details?
         css-media-prelude-details-queries
         css-media-prelude-details-text
         css-media-prelude-details-span
         css-media-query?
         css-media-query-modifier
         css-media-query-media-type
         css-media-query-features
         css-media-query-text
         css-media-query-span
         css-media-feature?
         css-media-feature-text
         css-media-feature-span
         css-media-feature-expression?
         css-media-feature-expression-name
         css-media-feature-expression-operator
         css-media-feature-expression-value
         css-media-feature-expression-text
         css-media-feature-expression-span
         css-media-feature-range?
         css-media-feature-range-lower
         css-media-feature-range-lower-operator
         css-media-feature-range-name
         css-media-feature-range-upper-operator
         css-media-feature-range-upper
         css-media-feature-range-text
         css-media-feature-range-span
         css-supports-prelude-details?
         css-supports-prelude-details-conditions
         css-supports-prelude-details-text
         css-supports-prelude-details-span
         css-supports-condition?
         css-supports-condition-kind
         css-supports-condition-text
         css-supports-condition-arguments
         css-supports-condition-span
         css-supports-feature?
         css-supports-feature-name
         css-supports-feature-value
         css-supports-feature-text
         css-supports-feature-span
         css-selector-pseudo?
         css-selector-pseudo-name
         css-selector-pseudo-arguments
         css-selector-pseudo-argument-structure
         css-selector-pseudo-selector-list?
         css-selector-pseudo-selector-list-selectors
         css-selector-pseudo-selector-list-text
         css-selector-pseudo-selector-list-span
         css-selector-pseudo-value-list?
         css-selector-pseudo-value-list-values
         css-selector-pseudo-value-list-text
         css-selector-pseudo-value-list-span
         css-selector-pseudo-nth-arguments?
         css-selector-pseudo-nth-arguments-formula
         css-selector-pseudo-nth-arguments-selectors
         css-selector-pseudo-nth-arguments-text
         css-selector-pseudo-nth-arguments-span
         css-selector-pseudo-identifier-list?
         css-selector-pseudo-identifier-list-values
         css-selector-pseudo-identifier-list-text
         css-selector-pseudo-identifier-list-span
         css-selector-pseudo-identifier?
         css-selector-pseudo-identifier-text
         css-selector-pseudo-identifier-value
         css-selector-pseudo-identifier-span
         css-selector-pseudo-element?
         css-selector-pseudo-text
         css-selector-pseudo-span
         css-selector-universal?
         css-selector-universal-text
         css-selector-universal-span
         css-selector-namespaced-universal?
         css-selector-namespaced-universal-namespace
         css-selector-namespaced-universal-text
         css-selector-namespaced-universal-span
         css-component-token?
         css-component-token-text
         css-component-token-span
         css-component-an-plus-b?
         css-component-an-plus-b-text
         css-component-an-plus-b-a
         css-component-an-plus-b-b
         css-component-an-plus-b-span
         css-component-number?
         css-component-number-text
         css-component-number-value
         css-component-number-span
         css-component-percentage?
         css-component-percentage-text
         css-component-percentage-value
         css-component-percentage-span
         css-component-dimension?
         css-component-dimension-text
         css-component-dimension-value
         css-component-dimension-unit
         css-component-dimension-span
         css-component-string?
         css-component-string-text
         css-component-string-value
         css-component-string-span
         css-component-hash?
         css-component-hash-text
         css-component-hash-value
         css-component-hash-span
         css-component-url?
         css-component-url-text
         css-component-url-value
         css-component-url-span
         css-component-function?
         css-component-function-name
         css-component-function-arguments
         css-component-function-text
         css-component-function-span
         css-component-block?
         css-component-block-delimiter
         css-component-block-values
         css-component-block-text
         css-component-block-span
         css-style-rule?
         css-style-rule-selector-groups
         css-style-rule-selectors
         css-style-rule-block
         css-style-rule-raw-selector
         css-style-rule-span
         css-at-rule?
         css-at-rule-name
         css-at-rule-prelude
         css-at-rule-prelude-values
         css-at-rule-block
         css-at-rule-span
         css-declaration?
         css-declaration-name
         css-declaration-value
         css-declaration-component-values
         css-declaration-important?
         css-declaration-span
         css-qualified-rule?
         css-qualified-rule-prelude
         css-qualified-rule-block
         exn:fail:css?
         exn:fail:css-source
         exn:fail:css-detail)

(require racket/list
         racket/port
         "private/css-ast.rkt"
         "private/css-compute.rkt"
         "private/css-errors.rkt"
         "private/css-parser.rkt"
         "private/css-query.rkt"
         "private/css-recovery.rkt"
         "private/css-rewrite.rkt"
         "private/css-serialize.rkt"
         "private/css-structure.rkt"
         "private/css-standard.rkt")

(define current-css-standard 'current)

;; parse-css : (or/c string? input-port?) -> any/c
;;   Parse CSS from a string or input port.
(define (parse-css source)
  (define parser (make-css-parser #:standard current-css-standard))
  (cond
    [(input-port? source)
     (parser source)]
    [(string? source)
     (parser (open-input-string source))]
    [else
     (raise-argument-error 'parse-css "(or/c string? input-port?)" source)]))

;; parse-stylesheet : (or/c string? input-port?) -> css-stylesheet?
;;   Alias for parse-css.
(define (parse-stylesheet source)
  (parse-css source))

(module+ test
  (require rackunit)

  (check-true (css-parser? (make-css-parser #:standard current-css-standard)))
  (define empty-stylesheet
    (parse-css ""))
  (define simple-stylesheet
    (parse-css "body { color: red; }"))
  (define grouped-stylesheet
    (parse-css ".a, .b { background: rgb(1 2 3); }"))
  (define typed-values-stylesheet
    (parse-css "body { padding: 10px 50% 2; background-image: url(\"x.png\") #fff; content: \"hi\"; }"))
  (define media-stylesheet
    (parse-css "@media screen { body { color: red; } }"))
  (define media-rich-stylesheet
    (parse-css "@media not screen and (width >= 40rem) and (20rem <= width <= 60rem) and (color) { body { color: red; } }"))
  (define supports-stylesheet
    (parse-css "@supports (display: grid) { .a { color: red; } }"))
  (define import-stylesheet
    (parse-css "@import url(\"x.css\") screen;"))
  (define font-face-stylesheet
    (parse-css "@font-face { font-family: \"X\"; src: url(\"x.woff2\"); }"))
  (define page-stylesheet
    (parse-css "@page { @bottom-right { content: counter(page); } }"))
  (define keyframes-stylesheet
    (parse-css "@keyframes fade { from { opacity: 0; } to { opacity: 1; } }"))
  (define comment-stylesheet
    (parse-css "/* top */ body { /* inner */ color: red; }"))
  (define recovery-stylesheet
    (parse-css "body { color red; width: 10px; } /\n"))
  (check-true (css-stylesheet? empty-stylesheet))
  (check-true (css-stylesheet? simple-stylesheet))
  (check-equal? (length (css-stylesheet-rules simple-stylesheet)) 1)
  (define first-rule
    (car (css-stylesheet-rules simple-stylesheet)))
  (check-true (css-style-rule? first-rule))
  (check-equal? (css-style-rule-raw-selector first-rule) "body")
  (check-equal? (length (css-style-rule-block first-rule)) 1)
  (define first-declaration
    (car (css-style-rule-block first-rule)))
  (check-true (css-declaration? first-declaration))
  (check-equal? (css-declaration-name first-declaration) "color")
  (check-equal? (css-declaration-value first-declaration) "red")
  (check-true (css-stylesheet? grouped-stylesheet))
  (define grouped-rule
    (car (css-stylesheet-rules grouped-stylesheet)))
  (check-equal? (css-style-rule-raw-selector grouped-rule) ".a, .b")
  (check-equal? (css-style-rule-selector-groups grouped-rule)
                '(".a" ".b"))
  (define grouped-declaration
    (car (css-style-rule-block grouped-rule)))
  (check-equal? (css-declaration-name grouped-declaration) "background")
  (check-equal? (css-declaration-value grouped-declaration) "rgb(1 2 3)")
  (define typed-rule
    (car (css-stylesheet-rules typed-values-stylesheet)))
  (define typed-padding
    (car (filter css-declaration? (css-style-rule-block typed-rule))))
  (define typed-padding-values
    (css-declaration-component-values typed-padding))
  (check-true (css-component-dimension? (car typed-padding-values)))
  (check-true (css-component-percentage? (cadr typed-padding-values)))
  (check-true (css-component-number? (caddr typed-padding-values)))
  (define typed-background
    (cadr (filter css-declaration? (css-style-rule-block typed-rule))))
  (define typed-background-values
    (css-declaration-component-values typed-background))
  (check-true (css-component-url? (car typed-background-values)))
  (check-true (css-component-hash? (cadr typed-background-values)))
  (define typed-content
    (caddr (filter css-declaration? (css-style-rule-block typed-rule))))
  (check-true
   (css-component-string?
    (car (css-declaration-component-values typed-content))))
  (define media-rule
    (car (css-stylesheet-rules media-stylesheet)))
  (check-true (css-at-rule? media-rule))
  (check-equal? (css-at-rule-name media-rule) "@media")
  (check-equal? (css-at-rule-prelude media-rule) "screen")
  (check-equal? (length (css-at-rule-block media-rule)) 1)
  (define media-prelude-details
    (css-at-rule-prelude-derived-details media-rule))
  (check-true (css-media-prelude-details? media-prelude-details))
  (check-equal? (css-media-query-media-type
                 (car (css-media-prelude-details-queries media-prelude-details)))
                "screen")
  (define media-rich-rule
    (car (css-stylesheet-rules media-rich-stylesheet)))
  (define media-rich-prelude-details
    (css-at-rule-prelude-derived-details media-rich-rule))
  (check-true
   (css-media-feature-expression?
    (car (css-media-query-features
          (car (css-media-prelude-details-queries media-rich-prelude-details))))))
  (check-equal?
   (css-media-feature-expression-name
    (car (css-media-query-features
          (car (css-media-prelude-details-queries media-rich-prelude-details)))))
   "width")
  (check-true
   (css-media-feature-range?
    (cadr (css-media-query-features
           (car (css-media-prelude-details-queries media-rich-prelude-details))))))
  (check-equal?
   (css-media-feature-range-name
    (cadr (css-media-query-features
           (car (css-media-prelude-details-queries media-rich-prelude-details)))))
   "width")
  (check-true
   (css-media-feature?
    (caddr (css-media-query-features
           (car (css-media-prelude-details-queries media-rich-prelude-details))))))
  (define supports-rule
    (car (css-stylesheet-rules supports-stylesheet)))
  (check-true (css-at-rule? supports-rule))
  (check-equal? (css-at-rule-name supports-rule) "@supports")
  (check-equal? (css-at-rule-prelude supports-rule) "(display: grid)")
  (define supports-prelude-details
    (css-at-rule-prelude-derived-details supports-rule))
  (check-true (css-supports-prelude-details? supports-prelude-details))
  (check-equal? (css-supports-condition-kind
                 (car (css-supports-prelude-details-conditions supports-prelude-details)))
                'feature)
  (check-true
   (css-supports-feature?
    (car (css-supports-condition-arguments
          (car (css-supports-prelude-details-conditions supports-prelude-details))))))
  (check-equal?
   (css-supports-feature-name
    (car (css-supports-condition-arguments
          (car (css-supports-prelude-details-conditions supports-prelude-details)))))
   "display")
  (define import-rule
    (car (css-stylesheet-rules import-stylesheet)))
  (check-true (css-at-rule? import-rule))
  (check-equal? (css-at-rule-name import-rule) "@import")
  (check-false (css-at-rule-block import-rule))
  (define font-face-rule
    (car (css-stylesheet-rules font-face-stylesheet)))
  (check-true (css-at-rule? font-face-rule))
  (check-equal? (css-at-rule-name font-face-rule) "@font-face")
  (check-equal? (length (css-at-rule-block font-face-rule)) 2)
  (define page-rule
    (car (css-stylesheet-rules page-stylesheet)))
  (check-true (css-at-rule? page-rule))
  (check-equal? (css-at-rule-name page-rule) "@page")
  (check-true (css-at-rule? (car (css-at-rule-block page-rule))))
  (define keyframes-rule
    (car (css-stylesheet-rules keyframes-stylesheet)))
  (check-true (css-at-rule? keyframes-rule))
  (check-equal? (css-at-rule-name keyframes-rule) "@keyframes")
  (check-equal? (css-at-rule-prelude keyframes-rule) "fade")
  (check-equal? (length (css-at-rule-block keyframes-rule)) 2)
  (check-true (css-comment? (car (css-stylesheet-rules comment-stylesheet))))
  (define recovered-rule
    (car (css-stylesheet-rules recovery-stylesheet)))
  (check-true
   (ormap css-recovery?
          (css-style-rule-block recovered-rule)))
  (check-true
   (css-recovery? (last (css-stylesheet-rules recovery-stylesheet))))
  (check-equal?
   (map css-selector-text
        (css-style-rule-selectors grouped-rule))
   '(".a" ".b"))
  (define selector-rich-rule
    (car (css-stylesheet-rules
          (parse-css "div#main > a[href^=\"https\"]:hover { color: red; }"))))
  (define selector-rich
    (car (css-style-rule-selectors selector-rich-rule)))
  (check-true (css-selector-type? (car (css-selector-parts selector-rich))))
  (check-true (css-selector-id? (cadr (css-selector-parts selector-rich))))
  (check-true (css-selector-combinator? (caddr (css-selector-parts selector-rich))))
  (check-true (css-selector-attribute? (list-ref (css-selector-parts selector-rich) 4)))
  (define selector-rich-attribute-details
    (css-selector-attribute-derived-details (list-ref (css-selector-parts selector-rich) 4)))
  (check-equal? (css-selector-attribute-details-name selector-rich-attribute-details) "href")
  (check-true
   (css-selector-attribute-string-value?
    (css-selector-attribute-details-value selector-rich-attribute-details)))
  (check-true (css-selector-pseudo? (list-ref (css-selector-parts selector-rich) 5)))
  (check-equal? (length (css-selector-compounds selector-rich)) 3)
  (define selector-namespace-rule
    (car (css-stylesheet-rules
          (parse-css "svg|rect.foo *|a, foo|* { color: red; }"))))
  (define selector-namespace
    (css-style-rule-selectors selector-namespace-rule))
  (check-true
   (css-selector-namespaced-type?
    (car (css-selector-parts (car selector-namespace)))))
  (check-true
   (css-selector-namespaced-type?
    (list-ref (css-selector-parts (car selector-namespace)) 3)))
  (check-true
   (css-selector-namespaced-universal?
    (car (css-selector-parts (cadr selector-namespace)))))
  (define selector-pseudo-rule
    (car (css-stylesheet-rules
          (parse-css "a:not(.x, #y) > span:nth-child(2n+1) { color: red; }"))))
  (define selector-pseudo-of-rule
    (car (css-stylesheet-rules
          (parse-css "li:nth-child(2n+1 of .item, #main) { color: red; }"))))
  (define selector-pseudo-identifier-rule
    (car (css-stylesheet-rules
          (parse-css "html:lang(en-US, da):dir(rtl) { color: red; }"))))
  (define selector-pseudo
    (car (css-style-rule-selectors selector-pseudo-rule)))
  (define selector-pseudo-of
    (car (css-style-rule-selectors selector-pseudo-of-rule)))
  (define selector-pseudo-identifier
    (car (css-style-rule-selectors selector-pseudo-identifier-rule)))
  (define selector-pseudo-parts
    (css-selector-parts selector-pseudo))
  (define selector-pseudo-of-parts
    (css-selector-parts selector-pseudo-of))
  (define selector-pseudo-identifier-parts
    (css-selector-parts selector-pseudo-identifier))
  (check-true
   (css-selector? (car (css-selector-pseudo-arguments (cadr selector-pseudo-parts)))))
  (check-true
   (css-selector-pseudo-selector-list?
    (css-selector-pseudo-argument-structure (cadr selector-pseudo-parts))))
  (check-true
   (css-component-an-plus-b? (car (css-selector-pseudo-arguments (list-ref selector-pseudo-parts 4)))))
  (check-true
   (css-selector-pseudo-nth-arguments?
    (css-selector-pseudo-argument-structure (list-ref selector-pseudo-parts 4))))
  (check-equal?
   (css-component-an-plus-b-a (car (css-selector-pseudo-arguments (list-ref selector-pseudo-parts 4))))
   2)
  (check-equal?
   (css-component-an-plus-b-b (car (css-selector-pseudo-arguments (list-ref selector-pseudo-parts 4))))
   1)
  (check-true
   (css-selector-pseudo-nth-arguments?
    (css-selector-pseudo-argument-structure (cadr selector-pseudo-of-parts))))
  (check-equal?
   (map css-selector-text
        (css-selector-pseudo-nth-arguments-selectors
         (css-selector-pseudo-argument-structure (cadr selector-pseudo-of-parts))))
   '(".item" "#main"))
  (check-true
   (css-selector-pseudo-identifier-list?
    (css-selector-pseudo-argument-structure (cadr selector-pseudo-identifier-parts))))
  (check-equal?
   (map css-selector-pseudo-identifier-value
        (css-selector-pseudo-arguments (cadr selector-pseudo-identifier-parts)))
   '("en-US" "da"))
  (check-equal?
   (map css-selector-pseudo-identifier-value
        (css-selector-pseudo-arguments (caddr selector-pseudo-identifier-parts)))
   '("rtl"))
  (define selector-attribute-rule
    (car (css-stylesheet-rules
          (parse-css "svg|a[foo|href=button i] { color: red; }"))))
  (define selector-attribute
    (cadr (css-selector-parts (car (css-style-rule-selectors selector-attribute-rule)))))
  (define selector-attribute-details*
    (css-selector-attribute-derived-details selector-attribute))
  (check-equal? (css-selector-attribute-details-namespace selector-attribute-details*) "foo")
  (check-equal? (css-selector-attribute-details-modifier selector-attribute-details*) "i")
  (check-true
   (css-selector-attribute-identifier-value?
    (css-selector-attribute-details-value selector-attribute-details*)))
  (check-true
   (css-component-function?
    (car (css-declaration-component-values grouped-declaration))))
  (check-true
   (pair? (css-at-rule-prelude-values supports-rule)))
  (check-true (css-has-recovery? recovery-stylesheet))
  (check-equal? (css-recovery-summary recovery-stylesheet)
                '((declaration . 1) (statement . 1)))
  (check-equal?
   (serialize-stylesheet simple-stylesheet)
   "body { color: red; }")
  (check-equal?
   (serialize-stylesheet simple-stylesheet #:preserve-source? #t)
   "body { color: red; }")
  (check-equal?
   (serialize-css import-stylesheet)
   "@import url(\"x.css\") screen;")
  (check-equal?
   (serialize-stylesheet
    (css-update-declaration-values simple-stylesheet "color" string-upcase))
   "body { color: RED; }")
  (check-equal?
   (serialize-stylesheet
    (css-map-rules
     simple-stylesheet
     (lambda (rule)
       (css-style-rule (css-style-rule-selector-groups rule)
                       (append (css-style-rule-block rule)
                               (list (css-declaration "margin" "0" #f #f)))
                       (css-style-rule-raw-selector rule)
                       #f))))
   "body { color: red; margin: 0; }")
  (check-equal?
   (serialize-stylesheet
    (css-map-at-rules
     media-stylesheet
     (lambda (rule)
       (if (string-ci=? (css-at-rule-name rule) "@media")
           (css-at-rule "@layer"
                        (css-at-rule-prelude rule)
                        (css-at-rule-block rule)
                        #f)
           rule))))
   "@layer screen { body { color: red; } }")
  (check-equal?
   (serialize-stylesheet
    (css-map-selectors grouped-stylesheet string-upcase))
   ".A, .B { background: rgb(1 2 3); }")
  (check-equal?
   (serialize-stylesheet
    (css-map-declarations-in-selectors
     grouped-stylesheet
     ".b"
     (lambda (declaration)
       (if (string-ci=? (css-declaration-name declaration) "background")
           (css-declaration "background" "black" #f #f)
           declaration))))
   ".a, .b { background: black; }")
  (check-equal?
   (serialize-stylesheet
    (css-update-declaration-values/preserve-source
     (parse-css "body {\n  color : red ;\n  margin: 0\n}")
     "color"
     string-upcase)
    #:preserve-source? #t)
   "body {\n  color: RED;\n  margin: 0\n}")
  (check-equal?
   (serialize-stylesheet
    (css-remove-declarations simple-stylesheet "color"))
   "body {  }")
  (check-equal?
   (serialize-stylesheet
    (css-remove-declarations/preserve-source
     (parse-css "body {\n  color : red ;\n  margin: 0\n}")
     "margin")
    #:preserve-source? #t)
   "body {\n  color : red ;\n  \n}")
  (check-equal?
   (serialize-stylesheet
    (css-append-declaration grouped-stylesheet ".b" "padding" "1rem"))
   ".a, .b { background: rgb(1 2 3); padding: 1rem; }")
  (check-equal?
   (serialize-stylesheet
    (css-append-declaration-by-pseudo
     (parse-css "a:hover { color: red; }")
     "hover"
     "text-decoration"
     "underline"))
   "a:hover { color: red; text-decoration: underline; }")
  (check-equal?
   (serialize-stylesheet
    (css-append-declaration-by-class
     (parse-css ".card { color: red; }")
     "card"
     "padding"
     "1rem"))
   ".card { color: red; padding: 1rem; }")
  (check-equal?
   (serialize-stylesheet
    (css-append-declaration-by-class/preserve-source
     (parse-css ".card {\n  color: red;\n}")
     "card"
     "padding"
     "1rem")
    #:preserve-source? #t)
   ".card {\n  color: red;\n  padding: 1rem;\n}")
  (check-equal?
   (serialize-stylesheet
    (css-append-declaration-by-attribute
     (parse-css "a[href] { color: red; }")
     "href"
     "text-decoration"
     "underline"))
   "a[href] { color: red; text-decoration: underline; }")
  (check-equal?
   (serialize-stylesheet
    (css-append-declaration-by-attribute/preserve-source
     (parse-css "a[href] {\n  color: red;\n}")
     "href"
     "text-decoration"
     "underline")
    #:preserve-source? #t)
   "a[href] {\n  color: red;\n  text-decoration: underline;\n}")
  (check-equal?
   (serialize-stylesheet
    (css-rename-class
     (parse-css ".card:hover, .card .title { color: red; }")
     "card"
     "panel"))
   ".panel:hover, .panel .title { color: red; }")
  (check-equal?
   (serialize-stylesheet
    (css-prefix-selectors simple-stylesheet ".scope"))
   ".scope body { color: red; }")
  (check-equal?
   (serialize-stylesheet
    (css-rewrite-media-queries
     (parse-css "@media screen and (width >= 40rem) { .card { color: blue; } }")
     (lambda (prelude details)
       "print")))
   "@media print { .card { color: blue; } }")
  (check-equal?
   (serialize-stylesheet
    (css-rewrite-supports-conditions
     (parse-css "@supports (display: grid) { .layout { display: grid; } }")
     (lambda (prelude details)
       "(display: flex)")))
   "@supports (display: flex) { .layout { display: grid; } }")
  (check-equal?
   (serialize-stylesheet
    (css-rewrite-custom-properties
     (parse-css ":root { --brand: red; color: var(--brand); }")
     (lambda (name)
       (if (string=? name "--brand") "--accent" name))))
   ":root { --accent: red; color: var(--accent); }")
  (check-equal?
   (serialize-stylesheet
    (css-split-grouped-selectors grouped-stylesheet))
   ".a { background: rgb(1 2 3); }\n.b { background: rgb(1 2 3); }")
  (check-equal?
   (serialize-stylesheet
    (css-clone-rule
     simple-stylesheet
     "body"
     #:transform
     (lambda (rule)
       (css-style-rule '(".copy")
                       (css-style-rule-block rule)
                       ".copy"
                       #f))))
   "body { color: red; }\n.copy { color: red; }")
  (check-equal?
   (serialize-stylesheet
    (css-insert-rule-before
     simple-stylesheet
     "body"
     (css-style-rule '(".before")
                     (list (css-declaration "display" "block" #f #f))
                     ".before"
                     #f)))
   ".before { display: block; }\nbody { color: red; }")
  (check-equal?
   (serialize-stylesheet
    (css-insert-rule-after
     simple-stylesheet
     "body"
     (css-style-rule '(".after")
                     (list (css-declaration "display" "none" #f #f))
                     ".after"
                     #f)))
   "body { color: red; }\n.after { display: none; }")
  (check-equal?
   (serialize-stylesheet
    (css-remove-rules
     (parse-css ".drop { color: red; }\n.keep { color: blue; }")
     (lambda (rule)
       (member ".drop" (css-style-rule-selector-groups rule)))))
   ".keep { color: blue; }")
  (check-equal?
   (serialize-stylesheet
    (css-remove-at-rules
     (parse-css "@media screen { body { color: red; } }\n@supports (display: grid) { .x { display: grid; } }")
     (lambda (rule)
       (string-ci=? (css-at-rule-name rule) "@supports"))))
   "@media screen { body { color: red; } }")
  (check-equal?
   (serialize-stylesheet
    (css-wrap-rules-in-media simple-stylesheet "body" "screen"))
   "@media screen { body { color: red; } }")
  (check-equal?
   (serialize-stylesheet
    (css-wrap-rules-in-supports simple-stylesheet "body" "(display: grid)"))
   "@supports (display: grid) { body { color: red; } }")
  (check-equal?
   (serialize-stylesheet
    (css-merge-adjacent-rules
     (parse-css "body { color: red; }\nbody { margin: 0; }\n@media screen { .x { a: b; } }\n@media screen { .y { c: d; } }")))
   "body { color: red; margin: 0; }\n@media screen { .x { a: b; } .y { c: d; } }")
  (check-equal?
   (serialize-stylesheet
    (css-dedupe-declarations
     (parse-css "body { color: red; color: blue; margin: 0; }")))
   "body { color: blue; margin: 0; }")
  (check-equal?
   (serialize-stylesheet
    (css-sort-declarations
     (parse-css "body { z-index: 1; color: red; background: black; }")))
   "body { background: black; color: red; z-index: 1; }")
  (check-equal?
   (serialize-stylesheet
    (css-rename-custom-property
     (parse-css ":root { --brand: red; color: var(--brand); }")
     "--brand"
     "--accent"))
   ":root { --accent: red; color: var(--accent); }")
  (check-equal?
   (serialize-stylesheet
    (css-rewrite-var-functions
     (parse-css "body { color: var(--brand); --brand: red; }")
     (lambda (name) (if (string=? name "--brand") "--accent" name))))
   "body { color: var(--accent); --brand: red; }")
  (check-equal?
   (serialize-stylesheet
    (css-rename-keyframes
     (parse-css "@keyframes fade { from { opacity: 0; } }\n.box { animation: fade 1s; }")
     "fade"
     "appear"))
   "@keyframes appear { from { opacity: 0; } }\n.box { animation: appear 1s; }")
  (check-equal?
   (serialize-stylesheet
    (css-rewrite-imports
     (parse-css "@import url(\"a.css\") screen;")
     (lambda (prelude) "url(\"b.css\") print")))
   "@import url(\"b.css\") print;")
  (check-equal?
   (serialize-stylesheet
    (css-rewrite-font-face
     (parse-css "@font-face { font-family: \"A\"; src: url(\"a.woff\"); }")
     (lambda (declaration)
       (if (string-ci=? (css-declaration-name declaration) "font-family")
           (css-declaration "font-family" "\"B\"" #f #f)
           declaration))))
   "@font-face { font-family: \"B\"; src: url(\"a.woff\"); }")
  (check-equal?
   (serialize-stylesheet
    (css-rewrite-url-values
     (parse-css "body { background: url(\"a.png\"); }\n@import url(\"b.css\") screen;")
     (lambda (inner)
       (cond
         [(string=? inner "\"a.png\"") "\"c.png\""]
         [(string=? inner "\"b.css\"") "\"d.css\""]
         [else inner]))))
   "body { background: url(\"c.png\"); }\n@import url(\"d.css\") screen;")
  (check-equal?
   (serialize-stylesheet
    (css-filter-comments
     (parse-css "/* drop */ body { color: red; } /* keep */")
     (lambda (comment)
       (regexp-match? #px"keep" (css-comment-text comment)))))
   "body { color: red; }\n/* keep */")
  (check-equal?
   (serialize-stylesheet
    (css-hoist-nested-rules
     (css-stylesheet
      (list (css-style-rule '(".card")
                            (list (css-declaration "color" "red" #f #f)
                                  (css-style-rule '(".title")
                                                  (list (css-declaration "color" "blue" #f #f))
                                                  ".title"
                                                  #f))
                            ".card"
                            #f))
      #f
      #f)))
   ".card { color: red; }\n.card .title { color: blue; }")
  (check-equal?
   (serialize-stylesheet
    (css-lower-nesting
     (css-stylesheet
      (list (css-style-rule '(".card")
                            (list (css-declaration "color" "red" #f #f)
                                  (css-style-rule '(".title")
                                                  (list (css-declaration "color" "blue" #f #f))
                                                  ".title"
                                                  #f))
                            ".card"
                            #f))
      #f
      #f)))
   ".card { color: red; }\n.card .title { color: blue; }")
  (check-equal?
   (serialize-stylesheet
    (css-rewrite-attribute-selectors
     (parse-css "a[href] { color: red; }")
     (lambda (text) "[data-href]")))
   "a[data-href] { color: red; }")
  (check-equal?
   (serialize-stylesheet
    (css-rewrite-pseudos
     (parse-css "a:hover::before { color: red; }")
     (lambda (text)
       (cond
         [(string=? text ":hover") ":focus"]
         [else text]))))
   "a:focus::before { color: red; }")
  (check-equal?
   (serialize-stylesheet
    (css-rewrite-selector-structure
     (parse-css ".card, .panel { color: red; }")
     (lambda (group selector)
       (if (regexp-match? #px"card" group)
           ".tile"
           group))))
   ".tile, .panel { color: red; }")
  (check-equal?
   (serialize-stylesheet
    (css-update-declaration-values-in-media-feature
     (parse-css "@media screen and (width >= 40rem) { .card { color: blue; } }")
     "width"
     "color"
     string-upcase))
   "@media screen and (width >= 40rem) { .card { color: BLUE; } }")
  (check-equal?
   (serialize-stylesheet
    (css-update-declaration-values-in-media-feature/preserve-source
     (parse-css "@media screen and (width >= 40rem) {\n  .card {\n    color : blue ;\n  }\n}")
     "width"
     "color"
     string-upcase)
    #:preserve-source? #t)
   "@media screen and (width >= 40rem) {\n  .card {\n    color: BLUE;\n  }\n}")
  (check-equal?
   (serialize-stylesheet
    (css-remove-declarations-in-media-feature
     (parse-css "@media screen and (width >= 40rem) { .card { color: blue; margin: 0; } }")
     "width"
     "color"))
   "@media screen and (width >= 40rem) { .card { margin: 0; } }")
  (check-equal?
   (serialize-stylesheet
    (css-remove-declarations-in-media-feature/preserve-source
     (parse-css "@media screen and (width >= 40rem) {\n  .card {\n    color: blue;\n    margin: 0;\n  }\n}")
     "width"
     "color")
    #:preserve-source? #t)
   "@media screen and (width >= 40rem) {\n  .card {\n    \n    margin: 0;\n  }\n}")
  (check-equal?
   (serialize-stylesheet
    (css-update-declaration-values-in-supports-feature
     (parse-css "@supports (display: grid) { .layout { display: grid; } }")
     "display"
     "display"
     string-upcase))
   "@supports (display: grid) { .layout { display: GRID; } }")
  (check-equal?
   (serialize-stylesheet
    (css-update-declaration-values-in-supports-feature/preserve-source
     (parse-css "@supports (display: grid) {\n  .layout {\n    display : grid ;\n  }\n}")
     "display"
     "display"
     string-upcase)
    #:preserve-source? #t)
   "@supports (display: grid) {\n  .layout {\n    display: GRID;\n  }\n}")
  (check-equal?
   (serialize-stylesheet
    (css-remove-declarations-in-supports-feature
     (parse-css "@supports (display: grid) { .layout { display: grid; gap: 1rem; } }")
     "display"
     "display"))
   "@supports (display: grid) { .layout { gap: 1rem; } }")
  (check-equal?
   (serialize-stylesheet
    (css-remove-declarations-in-supports-feature/preserve-source
     (parse-css "@supports (display: grid) {\n  .layout {\n    display: grid;\n    gap: 1rem;\n  }\n}")
     "display"
     "display")
    #:preserve-source? #t)
   "@supports (display: grid) {\n  .layout {\n    \n    gap: 1rem;\n  }\n}")
  (check-equal? (length (css-flatten-rules media-stylesheet)) 2)
  (define selector-query-stylesheet
    (parse-css ".a, .b { color: red; }\n@media screen { .b { color: blue; --gap: 1rem; } }\n.c { color: green; }\n@media screen { .b { COLOR: navy; --gap: 2rem; --accent: gold; } }"))
  (check-equal?
   (map css-style-rule-raw-selector
        (css-find-rules-by-selector-group selector-query-stylesheet ".b"))
   '(".a, .b" ".b" ".b"))
  (check-equal?
   (map css-style-rule-raw-selector
        (css-find-rules-by-raw-selector selector-query-stylesheet ".a, .b"))
   '(".a, .b"))
  (check-equal?
   (map css-declaration-value
        (css-find-declarations-in-selector-group selector-query-stylesheet ".b"))
   '("red" "blue" "1rem" "navy" "2rem" "gold"))
  (check-equal?
   (map css-declaration-value
        (css-find-declarations-in-selector-group selector-query-stylesheet
                                                 ".b"
                                                 "color"))
   '("red" "blue" "navy"))
  (check-equal?
   (map css-declaration-value
        (css-find-declarations-in-selector-groups selector-query-stylesheet
                                                  '(".a" ".c")))
   '("red" "green"))
  (check-equal?
   (map css-declaration-value
        (css-find-declarations-in-selector-groups selector-query-stylesheet
                                                  '(".a" ".b")
                                                  "CoLoR"))
   '("red" "blue" "navy"))
  (check-equal?
   (hash-ref (css-collect-custom-properties-in-selector-group
              selector-query-stylesheet
              ".b")
             "--gap")
   "2rem")
  (check-equal?
   (hash-ref (css-collect-custom-properties-in-selector-group
              selector-query-stylesheet
              ".b")
             "--accent")
   "gold")
  (check-equal?
   (hash-ref (css-collect-custom-properties-in-selector-groups
              selector-query-stylesheet
              '(".a" ".b"))
             "--gap")
   "2rem")
  (check-equal?
   (hash-ref (css-collect-custom-properties-in-selector-groups
              selector-query-stylesheet
              '(".a" ".b"))
             "--accent")
   "gold")
  (check-equal?
   (hash-count
    (css-collect-custom-properties-in-selector-groups
     selector-query-stylesheet
     '(".a" ".b")))
   2)
  (define computed-style-stylesheet
    (parse-css (string-append
                ".btn { color: red; --accent: blue; }\n"
                ".btn { color: green; }\n"
                ".btn { color: orange !important; }\n"
                "@media screen { .btn { background: var(--accent); --accent: gold; } }\n"
                ".btn { border-color: var(--border, black); }\n"
                ".btn { --a: var(--b); --b: var(--a); }\n"
                ":root { --base: #198754; --semantic: var(--base); --semantic-2: var(--semantic); }\n"
                ":root { --from-default: var(--external-token); }\n"
                ".form-control { border: 0 solid #e0e1e2; }\n"
                ".form-control { border-left-width: 2px; }\n"
                ".chip { margin: 0; margin: 1rem; }\n"
                ".chip { opacity: 0.5 !important; opacity: 0.7 !important; }\n")))
  (check-equal?
   (css-compute-style-for-selector-group computed-style-stylesheet ".btn")
   (hash "color" "orange"
         "background" "var(--accent)"
         "border-color" "var(--border, black)"))
  (check-equal?
   (css-compute-style-for-selector-group computed-style-stylesheet
                                         ".btn"
                                         #:resolve-vars? #t
                                         #:defaults (hash "--border" "gray"))
   (hash "color" "orange"
         "background" "gold"
         "border-color" "gray"))
  (check-equal?
   (css-compute-custom-properties-for-selector-group computed-style-stylesheet
                                                     ".btn"
                                                     #:resolve-vars? #t)
   (hash "--accent" "gold"
         "--a" "var(--b)"
         "--b" "var(--a)"))
  (check-equal?
   (css-compute-custom-properties-for-selector-group computed-style-stylesheet
                                                     ":root"
                                                     #:resolve-vars? #t
                                                     #:defaults (hash "--external-token" "#0d6efd"))
   (hash "--base"         "#198754"
         "--semantic"     "#198754"
         "--semantic-2"   "#198754"
         "--from-default" "#0d6efd"))
  (check-equal?
   (css-compute-style-for-selector-group computed-style-stylesheet ".chip")
   (hash "margin" "1rem"
         "margin-top" "1rem"
         "margin-right" "1rem"
         "margin-bottom" "1rem"
         "margin-left" "1rem"
         "opacity" "0.7"))
  (check-equal?
   (hash-ref (css-compute-style-for-selector-group computed-style-stylesheet
                                                   ".form-control")
             "border-left-width")
   "2px")
  (check-equal?
   (hash-ref (css-compute-style-for-selector-group computed-style-stylesheet
                                                   ".form-control")
             "border-top-width")
   "0")
  (define-values (computed-style computed-trace)
    (css-compute-style-for-selector-group computed-style-stylesheet
                                          ".btn"
                                          #:resolve-vars? #t
                                          #:defaults (hash "--border" "gray")
                                          #:trace? #t))
  (check-equal? (hash-ref computed-style "background") "gold")
  (check-true (css-compute-style-trace? computed-trace))
  (check-equal? (length (css-compute-style-trace-matched-rules computed-trace)) 6)
  (check-true
   (ormap (lambda (result)
            (and (string=? (css-compute-property-result-name result) "color")
                 (css-compute-candidate?
                  (css-compute-property-result-winner result))))
          (css-compute-style-trace-property-results computed-trace)))
  (define-values (_border-style border-trace)
    (css-compute-style-for-selector-group computed-style-stylesheet
                                          ".form-control"
                                          #:trace? #t))
  (check-true
   (ormap (lambda (result)
            (and (string=? (css-compute-property-result-name result)
                           "border-top-width")
                 (string=? (css-compute-candidate-source-name
                            (css-compute-property-result-winner result))
                           "border")))
          (css-compute-style-trace-property-results border-trace)))
  (check-equal? (length (css-find-declarations media-stylesheet "color")) 1)
  (check-equal? (length (css-query-selector grouped-stylesheet ".b")) 1)
  (check-equal? (length (css-find-rules-by-pseudo (parse-css "a:not(.x, #y) > span:nth-child(2n+1) { color: red; }") "not")) 1)
  (check-equal? (length (css-find-media-queries media-rich-stylesheet)) 1)
  (check-equal? (length (css-find-supports-features supports-stylesheet "display")) 1)
  (check-true (css-stylesheet? (parse-stylesheet "body { color: red; }"))))
