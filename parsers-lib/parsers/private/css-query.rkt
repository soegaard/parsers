#lang racket/base

;;;
;;; CSS Query Helpers
;;;
;;
;; Lightweight AST queries for the current CSS parser.

(provide css-flatten-rules
         css-find-declarations
         css-query-selector
         css-find-rules-by-pseudo
         css-find-media-queries
         css-find-supports-features)

(require racket/list
         racket/string
         "css-ast.rkt"
         "css-structure.rkt")

;; css-flatten-rules : css-stylesheet? -> list?
;;   Flatten nested rule-bearing at-rules into one pre-order rule list.
(define (css-flatten-rules stylesheet)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-flatten-rules "css-stylesheet?" stylesheet))
  (append-map flatten-rule-or-comment (css-stylesheet-rules stylesheet)))

;; css-find-declarations : css-stylesheet? string? -> list?
;;   Find declarations whose property matches name case-insensitively.
(define (css-find-declarations stylesheet name)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-find-declarations "css-stylesheet?" stylesheet))
  (unless (string? name)
    (raise-argument-error 'css-find-declarations "string?" name))
  (append-map (lambda (rule)
                (find-declarations-in-node rule name))
              (css-stylesheet-rules stylesheet)))

;; css-query-selector : css-stylesheet? string? -> list?
;;   Find style rules whose selector groups include selector.
(define (css-query-selector stylesheet selector)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-query-selector "css-stylesheet?" stylesheet))
  (unless (string? selector)
    (raise-argument-error 'css-query-selector "string?" selector))
  (filter (lambda (rule)
            (and (css-style-rule? rule)
                 (member selector (css-style-rule-selector-groups rule))))
          (css-flatten-rules stylesheet)))

;; css-find-rules-by-pseudo : css-stylesheet? string? -> list?
;;   Find style rules that use a pseudo selector with the given name.
(define (css-find-rules-by-pseudo stylesheet pseudo-name)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-find-rules-by-pseudo "css-stylesheet?" stylesheet))
  (unless (string? pseudo-name)
    (raise-argument-error 'css-find-rules-by-pseudo "string?" pseudo-name))
  (filter (lambda (rule)
            (and (css-style-rule? rule)
                 (rule-has-pseudo? rule pseudo-name)))
          (css-flatten-rules stylesheet)))

;; css-find-media-queries : css-stylesheet? -> list?
;;   Collect derived media-query nodes from all @media rules.
(define (css-find-media-queries stylesheet)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-find-media-queries "css-stylesheet?" stylesheet))
  (append-map media-queries-in-node (css-stylesheet-rules stylesheet)))

;; css-find-supports-features : css-stylesheet? [string?] -> list?
;;   Collect typed supports-feature nodes, optionally filtered by feature name.
(define (css-find-supports-features stylesheet [name #f])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-find-supports-features "css-stylesheet?" stylesheet))
  (when (and name (not (string? name)))
    (raise-argument-error 'css-find-supports-features "(or/c string? #f)" name))
  (define features
    (append-map supports-features-in-node (css-stylesheet-rules stylesheet)))
  (cond
    [name
     (filter (lambda (feature)
               (string-ci=? (css-supports-feature-name feature) name))
             features)]
    [else
     features]))

;; flatten-rule-or-comment : any/c -> list?
;;   Flatten one node recursively, skipping comments.
(define (flatten-rule-or-comment node)
  (cond
    [(css-comment? node)
     '()]
    [(css-style-rule? node)
     (list node)]
    [(css-at-rule? node)
     (cons node
           (cond
             [(list? (css-at-rule-block node))
              (append-map flatten-rule-or-comment (css-at-rule-block node))]
             [else
              '()]))]
    [else
     '()]))

;; rule-has-pseudo? : css-style-rule? string? -> boolean?
;;   Determine whether a style rule uses a pseudo with the given name.
(define (rule-has-pseudo? rule pseudo-name)
  (ormap (lambda (selector)
           (selector-has-pseudo? selector pseudo-name))
         (css-style-rule-selectors rule)))

;; selector-has-pseudo? : css-selector? string? -> boolean?
;;   Determine whether a selector contains a matching pseudo.
(define (selector-has-pseudo? selector pseudo-name)
  (ormap (lambda (part)
           (and (css-selector-pseudo? part)
                (string-ci=? (css-selector-pseudo-name part) pseudo-name)))
         (css-selector-parts selector)))

;; media-queries-in-node : any/c -> list?
;;   Collect media queries recursively from one AST node.
(define (media-queries-in-node node)
  (cond
    [(css-comment? node)
     '()]
    [(css-style-rule? node)
     '()]
    [(css-at-rule? node)
     (append
      (if (string-ci=? (css-at-rule-name node) "@media")
          (let ([details (css-at-rule-prelude-derived-details node)])
            (if (css-media-prelude-details? details)
                (css-media-prelude-details-queries details)
                '()))
          '())
      (if (list? (css-at-rule-block node))
          (append-map media-queries-in-node (css-at-rule-block node))
          '()))]
    [else
     '()]))

;; supports-features-in-node : any/c -> list?
;;   Collect typed supports-feature nodes recursively from one AST node.
(define (supports-features-in-node node)
  (cond
    [(css-comment? node)
     '()]
    [(css-style-rule? node)
     '()]
    [(css-at-rule? node)
     (append
      (if (string-ci=? (css-at-rule-name node) "@supports")
          (let ([details (css-at-rule-prelude-derived-details node)])
            (if (css-supports-prelude-details? details)
                (append-map supports-features-in-condition
                            (css-supports-prelude-details-conditions details))
                '()))
          '())
      (if (list? (css-at-rule-block node))
          (append-map supports-features-in-node (css-at-rule-block node))
          '()))]
    [else
     '()]))

;; supports-features-in-condition : css-supports-condition? -> list?
;;   Collect supports-feature leaf nodes recursively from one condition.
(define (supports-features-in-condition condition)
  (cond
    [(not (css-supports-condition? condition))
     '()]
    [(eq? (css-supports-condition-kind condition) 'feature)
     (filter css-supports-feature? (css-supports-condition-arguments condition))]
    [else
     (append-map supports-features-in-condition
                 (filter css-supports-condition?
                         (css-supports-condition-arguments condition)))]))

;; find-declarations-in-node : any/c string? -> list?
;;   Find matching declarations within one node recursively.
(define (find-declarations-in-node node name)
  (cond
    [(css-comment? node)
     '()]
    [(css-style-rule? node)
     (find-declarations-in-list (css-style-rule-block node) name)]
    [(css-at-rule? node)
     (cond
       [(list? (css-at-rule-block node))
        (find-declarations-in-list (css-at-rule-block node) name)]
       [else
        '()])]
    [(css-declaration? node)
     (if (string-ci=? (css-declaration-name node) name)
         (list node)
         '())]
    [else
     '()]))

;; find-declarations-in-list : list? string? -> list?
;;   Find matching declarations in a list of nodes.
(define (find-declarations-in-list items name)
  (append-map (lambda (item)
                (find-declarations-in-node item name))
              items))

(module+ test
  (require rackunit)

  (define stylesheet
    (css-stylesheet
     (list (css-comment "/* top */" #f)
           (css-style-rule '("body")
                           (list (css-declaration "color" "red" #f #f))
                           "body"
                           #f)
           (css-at-rule "@media"
                        "screen"
                        (list (css-style-rule '(".a" ".b")
                                              (list (css-declaration "color" "blue" #f #f))
                                              ".a, .b"
                                              #f))
                        #f)
           (css-at-rule "@supports"
                        "(display: grid) and (color: red)"
                        '()
                        #f)
           (css-style-rule '("a:hover")
                           '()
                           "a:hover"
                           #f))
     #f
     #f))

  (check-equal? (length (css-flatten-rules stylesheet)) 5)
  (check-equal? (length (css-find-declarations stylesheet "color")) 2)
  (check-equal? (length (css-query-selector stylesheet ".b")) 1)
  (check-equal? (length (css-find-rules-by-pseudo stylesheet "hover")) 1)
  (check-equal? (length (css-find-media-queries stylesheet)) 1)
  (check-equal? (length (css-find-supports-features stylesheet "display")) 1))
