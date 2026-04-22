#lang racket/base

;;;
;;; CSS Computed-Style Fixture Checker
;;;
;;
;; Run stable computed-style regression checks against copied real-CSS
;; fixtures stored in the repository.

(require parsers/css
         racket/hash
         racket/runtime-path
         rackunit
         rackunit/text-ui)

;; fixture-0003-loaders : path?
;;   Copied real-CSS fixture for loader styles.
(define-runtime-path fixture-0003-loaders
  "../testdata/css-compute-fixtures/0003-loaders.css")

;; fixture-0011-css-nesting : path?
;;   Copied real-CSS fixture for nesting and custom-property resolution.
(define-runtime-path fixture-0011-css-nesting
  "../testdata/css-compute-fixtures/0011-css-nesting.css")

;; fixture-0042-stylesheet : path?
;;   Copied real-CSS fixture for nested rule-bearing at-rules.
(define-runtime-path fixture-0042-stylesheet
  "../testdata/css-compute-fixtures/0042-stylesheet.css")

;; fixture-0655-td-todos : path?
;;   Copied real-CSS fixture for :host-style rules.
(define-runtime-path fixture-0655-td-todos
  "../testdata/css-compute-fixtures/0655-td-todos.css")

;; load-stylesheet : path-string? -> css-stylesheet?
;;   Parse one copied fixture stylesheet.
(define (load-stylesheet path)
  (call-with-input-file path parse-css))

;; compute-style/trace : css-stylesheet? string? -> values
;;   Compute style and trace for one exact selector-group target.
(define (compute-style/trace stylesheet selector-group)
  (css-compute-style-for-selector-group stylesheet
                                        selector-group
                                        #:trace? #t))

;; compute-resolved-style/trace : css-stylesheet? string? -> values
;;   Compute resolved style and trace for one exact selector-group target.
(define (compute-resolved-style/trace stylesheet selector-group)
  (css-compute-style-for-selector-group stylesheet
                                        selector-group
                                        #:resolve-vars? #t
                                        #:trace? #t))

;; fixture-tests : test-suite?
;;   Stable regression checks derived from copied real-CSS fixtures.
(define fixture-tests
  (test-suite
   "css-compute-fixtures"

   (test-case
    "0003 loaders exact selector group"
    (define stylesheet
      (load-stylesheet fixture-0003-loaders))
    (define style
      (css-compute-style-for-selector-group stylesheet
                                            ".lds-ellipsis div"))
    (define custom-properties
      (css-compute-custom-properties-for-selector-group stylesheet
                                                        ".lds-ellipsis div"))
    (define-values (resolved-style trace)
      (compute-resolved-style/trace stylesheet ".lds-ellipsis div"))
    (check-equal?
     style
     (hash "animation-timing-function" "cubic-bezier(0, 1, 1, 0)"
           "background"                "rgb(128, 128, 128)"
           "border-radius"            "50%"
           "height"                   "13px"
           "position"                 "absolute"
           "top"                      "33px"
           "width"                    "13px"))
    (check-equal? custom-properties (hash))
    (check-equal? resolved-style style)
    (check-equal? (length (css-compute-style-trace-matched-rules trace)) 1))

   (test-case
    "0011 nesting custom properties and var resolution"
    (define stylesheet
      (load-stylesheet fixture-0011-css-nesting))
    (define style
      (css-compute-style-for-selector-group stylesheet ".grid"))
    (define custom-properties
      (css-compute-custom-properties-for-selector-group stylesheet ".grid"))
    (define-values (resolved-style trace)
      (compute-resolved-style/trace stylesheet ".grid"))
    (check-equal?
     style
     (hash "color" "#1a2b3c"
           "gap"   "var(--gap)"))
    (check-equal?
     custom-properties
     (hash "--gap" "1rem"))
    (check-equal?
     resolved-style
     (hash "color" "#1a2b3c"
           "gap"   "1rem"))
    (check-equal? (length (css-compute-style-trace-matched-rules trace)) 1))

   (test-case
    "0042 stylesheet exact selector groups inside nested rules"
    (define stylesheet
      (load-stylesheet fixture-0042-stylesheet))
    (define span-math-style
      (css-compute-style-for-selector-group stylesheet "span.math"))
    (define-values (display-style trace)
      (compute-style/trace stylesheet "span.div.MathJax_Display"))
    (check-equal?
     span-math-style
     (hash "text-align" "left"))
    (check-equal?
     display-style
     (hash "background-color" "red"
           "margin-top"       "0rem"))
    (check-equal? (length (css-compute-style-trace-matched-rules trace)) 1))

   (test-case
    "0655 td todos host selector"
    (define stylesheet
      (load-stylesheet fixture-0655-td-todos))
    (define style
      (css-compute-style-for-selector-group stylesheet ":host"))
    (define custom-properties
      (css-compute-custom-properties-for-selector-group stylesheet ":host"))
    (define-values (resolved-style trace)
      (compute-style/trace stylesheet ":host"))
    (check-equal?
     style
     (hash "position" "relative"))
    (check-equal? custom-properties (hash))
    (check-equal? resolved-style style)
    (check-equal? (length (css-compute-style-trace-matched-rules trace)) 1))))

(module+ test
  (run-tests fixture-tests))

(module+ main
  (define result
    (run-tests fixture-tests))
  (unless (zero? result)
    (exit 1)))
