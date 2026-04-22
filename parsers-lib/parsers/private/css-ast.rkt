#lang racket/base

;;;
;;; CSS AST
;;;
;;
;; Internal AST representation shared by the public facade, parser driver,
;; and future grammar modules.

(provide (struct-out css-source-span)
         (struct-out css-stylesheet)
         (struct-out css-comment)
         (struct-out css-recovery)
         (struct-out css-selector)
         (struct-out css-selector-compound)
         (struct-out css-selector-combinator)
         (struct-out css-selector-type)
         (struct-out css-selector-namespaced-type)
         (struct-out css-selector-class)
         (struct-out css-selector-id)
         (struct-out css-selector-attribute)
         (struct-out css-selector-attribute-details)
         (struct-out css-selector-attribute-identifier-value)
         (struct-out css-selector-attribute-string-value)
         (struct-out css-selector-pseudo)
         (struct-out css-selector-pseudo-selector-list)
         (struct-out css-selector-pseudo-value-list)
         (struct-out css-selector-pseudo-nth-arguments)
         (struct-out css-selector-pseudo-identifier-list)
         (struct-out css-selector-pseudo-identifier)
         (struct-out css-selector-universal)
         (struct-out css-selector-namespaced-universal)
         (struct-out css-media-prelude-details)
         (struct-out css-media-query)
         (struct-out css-media-feature)
         (struct-out css-media-feature-expression)
         (struct-out css-media-feature-range)
         (struct-out css-supports-prelude-details)
         (struct-out css-supports-condition)
         (struct-out css-supports-feature)
         (struct-out css-component-token)
         (struct-out css-component-an-plus-b)
         (struct-out css-component-number)
         (struct-out css-component-percentage)
         (struct-out css-component-dimension)
         (struct-out css-component-string)
         (struct-out css-component-hash)
         (struct-out css-component-url)
         (struct-out css-component-function)
         (struct-out css-component-block)
         (struct-out css-style-rule)
         (struct-out css-at-rule)
         (struct-out css-declaration)
         make-empty-css-stylesheet
         (struct-out css-qualified-rule))

(struct css-source-span (start end)
  #:transparent)

(struct css-stylesheet (rules source span)
  #:transparent)

(struct css-comment (text span)
  #:transparent)

(struct css-recovery (kind reason text span detail)
  #:transparent)

(struct css-selector (text span parts)
  #:transparent)

(struct css-selector-compound (items span)
  #:transparent)

(struct css-selector-combinator (text span)
  #:transparent)

(struct css-selector-type (name span)
  #:transparent)

(struct css-selector-namespaced-type (namespace name span)
  #:transparent)

(struct css-selector-class (name span)
  #:transparent)

(struct css-selector-id (name span)
  #:transparent)

(struct css-selector-attribute (name matcher value modifier text span)
  #:transparent)

(struct css-selector-attribute-details (namespace name matcher value modifier text span)
  #:transparent)

(struct css-selector-attribute-identifier-value (text value span)
  #:transparent)

(struct css-selector-attribute-string-value (text value span)
  #:transparent)

(struct css-selector-pseudo (name arguments argument-structure element? text span)
  #:transparent)

(struct css-selector-pseudo-selector-list (selectors text span)
  #:transparent)

(struct css-selector-pseudo-value-list (values text span)
  #:transparent)

(struct css-selector-pseudo-nth-arguments (formula selectors text span)
  #:transparent)

(struct css-selector-pseudo-identifier-list (values text span)
  #:transparent)

(struct css-selector-pseudo-identifier (text value span)
  #:transparent)

(struct css-selector-universal (text span)
  #:transparent)

(struct css-selector-namespaced-universal (namespace text span)
  #:transparent)

(struct css-media-prelude-details (queries text span)
  #:transparent)

(struct css-media-query (modifier media-type features text span)
  #:transparent)

(struct css-media-feature (text span)
  #:transparent)

(struct css-media-feature-expression (name operator value text span)
  #:transparent)

(struct css-media-feature-range (lower lower-operator name upper-operator upper text span)
  #:transparent)

(struct css-supports-prelude-details (conditions text span)
  #:transparent)

(struct css-supports-condition (kind text arguments span)
  #:transparent)

(struct css-supports-feature (name value text span)
  #:transparent)

(struct css-component-token (text span)
  #:transparent)

(struct css-component-an-plus-b (text a b span)
  #:transparent)

(struct css-component-number (text value span)
  #:transparent)

(struct css-component-percentage (text value span)
  #:transparent)

(struct css-component-dimension (text value unit span)
  #:transparent)

(struct css-component-string (text value span)
  #:transparent)

(struct css-component-hash (text value span)
  #:transparent)

(struct css-component-url (text value span)
  #:transparent)

(struct css-component-function (name arguments text span)
  #:transparent)

(struct css-component-block (delimiter values text span)
  #:transparent)

(struct css-style-rule (selector-groups block raw-selector span)
  #:transparent)

(struct css-at-rule (name prelude block span)
  #:transparent)

(struct css-declaration (name value important? span)
  #:transparent)

(struct css-qualified-rule (prelude block)
  #:transparent)

(define (make-empty-css-stylesheet [source #f])
  (css-stylesheet '() source #f))

(module+ test
  (require rackunit)

  (define stylesheet (make-empty-css-stylesheet "body {}"))
  (define span (css-source-span 1 5))
  (define comment (css-comment "/* note */" span))
  (define recovery (css-recovery 'statement "bad token" "/" span '()))
  (define selector (css-selector "body" span '()))
  (define selector-compound (css-selector-compound '() span))
  (define selector-combinator (css-selector-combinator ">" span))
  (define selector-type (css-selector-type "body" span))
  (define selector-namespaced-type (css-selector-namespaced-type "svg" "rect" span))
  (define selector-class (css-selector-class "lead" span))
  (define selector-id (css-selector-id "main" span))
  (define selector-attribute (css-selector-attribute "href" "^=" "\"https\"" #f "[href^=\"https\"]" span))
  (define selector-attribute-value-ident
    (css-selector-attribute-identifier-value "button" "button" span))
  (define selector-attribute-value-string
    (css-selector-attribute-string-value "\"https\"" "https" span))
  (define selector-attribute-details
    (css-selector-attribute-details "svg" "href" "^=" selector-attribute-value-string "i" "[svg|href^=\"https\" i]" span))
  (define component-token (css-component-token "red" span))
  (define selector-pseudo-selector-list
    (css-selector-pseudo-selector-list (list selector) ".x, #y" span))
  (define selector-pseudo-value-list
    (css-selector-pseudo-value-list (list component-token) "2n+1" span))
  (define component-an-plus-b (css-component-an-plus-b "2n+1" 2 1 span))
  (define selector-pseudo-nth-arguments
    (css-selector-pseudo-nth-arguments (list component-an-plus-b) (list selector) "2n+1 of .x" span))
  (define selector-pseudo-identifier
    (css-selector-pseudo-identifier "en-US" "en-US" span))
  (define selector-pseudo-identifier-list
    (css-selector-pseudo-identifier-list (list selector-pseudo-identifier) "en-US" span))
  (define selector-pseudo
    (css-selector-pseudo "not" '() selector-pseudo-selector-list #f ":not(.x)" span))
  (define selector-universal (css-selector-universal "*" span))
  (define selector-namespaced-universal (css-selector-namespaced-universal "*" "*|*" span))
  (define media-feature (css-media-feature "(color)" span))
  (define media-feature-expression
    (css-media-feature-expression "width" ">=" "40rem" "(width >= 40rem)" span))
  (define media-feature-range
    (css-media-feature-range "20rem" "<=" "width" "<=" "60rem" "(20rem <= width <= 60rem)" span))
  (define media-query (css-media-query "not" "screen" (list media-feature-expression media-feature-range media-feature) "not screen and (width >= 40rem) and (20rem <= width <= 60rem) and (color)" span))
  (define media-prelude (css-media-prelude-details (list media-query) "not screen and (width >= 40rem)" span))
  (define supports-feature (css-supports-feature "display" "grid" "(display: grid)" span))
  (define supports-condition (css-supports-condition 'feature "(display: grid)" (list supports-feature) span))
  (define supports-prelude (css-supports-prelude-details (list supports-condition) "(display: grid)" span))
  (define component-number (css-component-number "12" 12 span))
  (define component-percentage (css-component-percentage "50%" 50 span))
  (define component-dimension (css-component-dimension "10px" 10 "px" span))
  (define component-string (css-component-string "\"x\"" "x" span))
  (define component-hash (css-component-hash "#fff" "fff" span))
  (define component-url (css-component-url "url(\"x\")" "\"x\"" span))
  (define component-function (css-component-function "rgb" (list component-token) "rgb(red)" span))
  (define component-block (css-component-block #\( (list component-token) "(red)" span))
  (define style-rule (css-style-rule '() '() "body" span))
  (define at-rule (css-at-rule "@media" '() '() span))
  (define declaration (css-declaration "color" "#fff" #f span))
  (check-true (css-stylesheet? stylesheet))
  (check-equal? (css-stylesheet-rules stylesheet) '())
  (check-equal? (css-stylesheet-source stylesheet) "body {}")
  (check-false (css-stylesheet-span stylesheet))
  (check-true (css-source-span? span))
  (check-equal? (css-source-span-start span) 1)
  (check-equal? (css-source-span-end span) 5)
  (check-true (css-comment? comment))
  (check-equal? (css-comment-text comment) "/* note */")
  (check-true (css-recovery? recovery))
  (check-equal? (css-recovery-kind recovery) 'statement)
  (check-true (css-selector? selector))
  (check-equal? (css-selector-text selector) "body")
  (check-true (css-selector-compound? selector-compound))
  (check-true (css-selector-combinator? selector-combinator))
  (check-true (css-selector-type? selector-type))
  (check-true (css-selector-namespaced-type? selector-namespaced-type))
  (check-true (css-selector-class? selector-class))
  (check-true (css-selector-id? selector-id))
  (check-true (css-selector-attribute? selector-attribute))
  (check-true (css-selector-attribute-details? selector-attribute-details))
  (check-true (css-selector-attribute-identifier-value? selector-attribute-value-ident))
  (check-true (css-selector-attribute-string-value? selector-attribute-value-string))
  (check-true (css-selector-pseudo? selector-pseudo))
  (check-true (css-selector-pseudo-selector-list? selector-pseudo-selector-list))
  (check-true (css-selector-pseudo-value-list? selector-pseudo-value-list))
  (check-true (css-selector-pseudo-nth-arguments? selector-pseudo-nth-arguments))
  (check-true (css-selector-pseudo-identifier? selector-pseudo-identifier))
  (check-true (css-selector-pseudo-identifier-list? selector-pseudo-identifier-list))
  (check-true (css-selector-universal? selector-universal))
  (check-true (css-selector-namespaced-universal? selector-namespaced-universal))
  (check-true (css-media-feature? media-feature))
  (check-true (css-media-feature-expression? media-feature-expression))
  (check-true (css-media-feature-range? media-feature-range))
  (check-true (css-media-query? media-query))
  (check-true (css-media-prelude-details? media-prelude))
  (check-true (css-supports-condition? supports-condition))
  (check-true (css-supports-feature? supports-feature))
  (check-true (css-supports-prelude-details? supports-prelude))
  (check-true (css-component-token? component-token))
  (check-true (css-component-an-plus-b? component-an-plus-b))
  (check-true (css-component-number? component-number))
  (check-true (css-component-percentage? component-percentage))
  (check-true (css-component-dimension? component-dimension))
  (check-true (css-component-string? component-string))
  (check-true (css-component-hash? component-hash))
  (check-true (css-component-url? component-url))
  (check-true (css-component-function? component-function))
  (check-true (css-component-block? component-block))
  (check-true (css-style-rule? style-rule))
  (check-equal? (css-style-rule-raw-selector style-rule) "body")
  (check-true (css-at-rule? at-rule))
  (check-equal? (css-at-rule-name at-rule) "@media")
  (check-true (css-declaration? declaration))
  (check-equal? (css-declaration-name declaration) "color"))
