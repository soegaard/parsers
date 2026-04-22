#lang racket/base

;;;
;;; CSS Rewrite Helpers
;;;
;;
;; Small AST rewrite helpers for common stylesheet edits.

(provide css-map-declarations
         css-map-rules
         css-map-at-rules
         css-map-selectors
         css-map-declarations-in-selectors
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
         css-remove-declarations-in-supports-feature/preserve-source)

(require racket/list
         racket/port
         racket/string
         parser-tools/lex
         "css-ast.rkt"
         "css-parser.rkt"
         "css-structure.rkt")

;; css-map-declarations : css-stylesheet? (css-declaration? -> (or/c css-declaration? #f)) -> css-stylesheet?
;;   Rewrite declarations throughout a stylesheet, optionally removing them.
(define (css-map-declarations stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-map-declarations "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-map-declarations "procedure?" proc))
  (css-stylesheet
   (rewrite-node-list (css-stylesheet-rules stylesheet) proc)
   #f
   #f))

;; css-map-rules : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite style rules throughout a stylesheet, optionally removing or replacing them.
(define (css-map-rules stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-map-rules "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-map-rules "procedure?" proc))
  (css-stylesheet
   (map-style-rule-node-list (css-stylesheet-rules stylesheet) proc)
   #f
   #f))

;; css-map-at-rules : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite at-rules throughout a stylesheet, optionally removing or replacing them.
(define (css-map-at-rules stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-map-at-rules "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-map-at-rules "procedure?" proc))
  (css-stylesheet
   (map-at-rule-node-list (css-stylesheet-rules stylesheet) proc)
   #f
   #f))

;; css-map-selectors : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite selector-group strings throughout a stylesheet.
(define (css-map-selectors stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-map-selectors "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-map-selectors "procedure?" proc))
  (css-map-rules
   stylesheet
   (lambda (rule)
     (define new-groups
       (map (lambda (group)
              (proc group))
            (css-style-rule-selector-groups rule)))
     (unless (andmap string? new-groups)
       (raise-argument-error 'css-map-selectors "(-> string? string?)" proc))
     (css-style-rule new-groups
                     (css-style-rule-block rule)
                     (string-join new-groups ", ")
                     #f))))

;; css-map-declarations-in-selectors : css-stylesheet? string? procedure? -> css-stylesheet?
;;   Rewrite declarations only within rules containing one exact selector group.
(define (css-map-declarations-in-selectors stylesheet selector-group proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-map-declarations-in-selectors "css-stylesheet?" stylesheet))
  (unless (string? selector-group)
    (raise-argument-error 'css-map-declarations-in-selectors "string?" selector-group))
  (unless (procedure? proc)
    (raise-argument-error 'css-map-declarations-in-selectors "procedure?" proc))
  (css-stylesheet
   (rewrite-declarations-in-matching-rules (css-stylesheet-rules stylesheet)
                                           selector-group
                                           proc)
   #f
   #f))

;; css-update-declaration-values : css-stylesheet? string? (string? -> string?) -> css-stylesheet?
;;   Update declaration values for a property name case-insensitively.
(define (css-update-declaration-values stylesheet name updater)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-update-declaration-values "css-stylesheet?" stylesheet))
  (unless (string? name)
    (raise-argument-error 'css-update-declaration-values "string?" name))
  (unless (procedure? updater)
    (raise-argument-error 'css-update-declaration-values "procedure?" updater))
  (css-map-declarations
   stylesheet
   (lambda (declaration)
     (if (string-ci=? (css-declaration-name declaration) name)
         (css-declaration (css-declaration-name declaration)
                          (updater (css-declaration-value declaration))
                          (css-declaration-important? declaration)
                          #f)
         declaration))))

;; css-update-declaration-values/preserve-source : css-stylesheet? string? (string? -> string?) -> css-stylesheet?
;;   Update declaration values while preserving untouched source text when possible.
(define (css-update-declaration-values/preserve-source stylesheet name updater)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-update-declaration-values/preserve-source "css-stylesheet?" stylesheet))
  (unless (string? name)
    (raise-argument-error 'css-update-declaration-values/preserve-source "string?" name))
  (unless (procedure? updater)
    (raise-argument-error 'css-update-declaration-values/preserve-source "procedure?" updater))
  (cond
    [(and (string? (css-stylesheet-source stylesheet))
          (source-editable-declarations? (matching-declarations stylesheet name)))
     (reparse-source
      (apply-source-edits
       (css-stylesheet-source stylesheet)
       (map (lambda (declaration)
              (make-source-edit declaration
                                (serialize-declaration-like
                                 (css-stylesheet-source stylesheet)
                                 declaration
                                 (updater (css-declaration-value declaration)))))
            (matching-declarations stylesheet name))))]
    [else
     (css-update-declaration-values stylesheet name updater)]))

;; css-remove-declarations : css-stylesheet? string? -> css-stylesheet?
;;   Remove declarations whose property matches name case-insensitively.
(define (css-remove-declarations stylesheet name)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-remove-declarations "css-stylesheet?" stylesheet))
  (unless (string? name)
    (raise-argument-error 'css-remove-declarations "string?" name))
  (css-map-declarations
   stylesheet
   (lambda (declaration)
     (and (not (string-ci=? (css-declaration-name declaration) name))
          declaration))))

;; css-remove-declarations/preserve-source : css-stylesheet? string? -> css-stylesheet?
;;   Remove declarations while preserving untouched source text when possible.
(define (css-remove-declarations/preserve-source stylesheet name)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-remove-declarations/preserve-source "css-stylesheet?" stylesheet))
  (unless (string? name)
    (raise-argument-error 'css-remove-declarations/preserve-source "string?" name))
  (cond
    [(and (string? (css-stylesheet-source stylesheet))
          (source-editable-declarations? (matching-declarations stylesheet name)))
     (reparse-source
      (apply-source-edits
       (css-stylesheet-source stylesheet)
       (map (lambda (declaration)
              (make-source-edit declaration ""))
            (matching-declarations stylesheet name))))]
    [else
     (css-remove-declarations stylesheet name)]))

;; css-append-declaration : css-stylesheet? string? string? string? keyword-arguments -> css-stylesheet?
;;   Append a declaration to style rules that include selector-group exactly.
(define (css-append-declaration stylesheet selector-group name value #:important? [important? #f])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-append-declaration "css-stylesheet?" stylesheet))
  (unless (string? selector-group)
    (raise-argument-error 'css-append-declaration "string?" selector-group))
  (unless (string? name)
    (raise-argument-error 'css-append-declaration "string?" name))
  (unless (string? value)
    (raise-argument-error 'css-append-declaration "string?" value))
  (css-stylesheet
   (append-declaration-node-list (css-stylesheet-rules stylesheet)
                                 selector-group
                                 name
                                 value
                                 important?)
   #f
   #f))

;; css-append-declaration/preserve-source : css-stylesheet? string? string? string? keyword-arguments -> css-stylesheet?
;;   Append a declaration while preserving untouched source text when possible.
(define (css-append-declaration/preserve-source stylesheet selector-group name value #:important? [important? #f])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-append-declaration/preserve-source "css-stylesheet?" stylesheet))
  (unless (string? selector-group)
    (raise-argument-error 'css-append-declaration/preserve-source "string?" selector-group))
  (unless (string? name)
    (raise-argument-error 'css-append-declaration/preserve-source "string?" name))
  (unless (string? value)
    (raise-argument-error 'css-append-declaration/preserve-source "string?" value))
  (cond
    [(and (string? (css-stylesheet-source stylesheet))
          (source-editable-rules? (matching-style-rules-by-selector-group stylesheet selector-group)))
     (reparse-source
      (apply-source-edits
       (css-stylesheet-source stylesheet)
       (map (lambda (rule)
              (make-rule-append-edit (css-stylesheet-source stylesheet)
                                     rule
                                     (serialize-new-declaration name value important?)))
            (matching-style-rules-by-selector-group stylesheet selector-group))))]
    [else
     (css-append-declaration stylesheet selector-group name value #:important? important?)]))

;; css-append-declaration-by-pseudo : css-stylesheet? string? string? string? keyword-arguments -> css-stylesheet?
;;   Append a declaration to style rules that use a given pseudo selector name.
(define (css-append-declaration-by-pseudo stylesheet pseudo-name name value #:important? [important? #f])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-append-declaration-by-pseudo "css-stylesheet?" stylesheet))
  (unless (string? pseudo-name)
    (raise-argument-error 'css-append-declaration-by-pseudo "string?" pseudo-name))
  (unless (string? name)
    (raise-argument-error 'css-append-declaration-by-pseudo "string?" name))
  (unless (string? value)
    (raise-argument-error 'css-append-declaration-by-pseudo "string?" value))
  (css-stylesheet
   (append-by-pseudo-node-list (css-stylesheet-rules stylesheet)
                               pseudo-name
                               name
                               value
                               important?)
   #f
   #f))

;; css-append-declaration-by-pseudo/preserve-source : css-stylesheet? string? string? string? keyword-arguments -> css-stylesheet?
;;   Append a declaration by pseudo selector while preserving untouched source when possible.
(define (css-append-declaration-by-pseudo/preserve-source stylesheet pseudo-name name value #:important? [important? #f])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-append-declaration-by-pseudo/preserve-source "css-stylesheet?" stylesheet))
  (unless (string? pseudo-name)
    (raise-argument-error 'css-append-declaration-by-pseudo/preserve-source "string?" pseudo-name))
  (unless (string? name)
    (raise-argument-error 'css-append-declaration-by-pseudo/preserve-source "string?" name))
  (unless (string? value)
    (raise-argument-error 'css-append-declaration-by-pseudo/preserve-source "string?" value))
  (define matching-rules
    (matching-style-rules-by-pseudo stylesheet pseudo-name))
  (cond
    [(and (string? (css-stylesheet-source stylesheet))
          (source-editable-rules? matching-rules))
     (reparse-source
      (apply-source-edits
       (css-stylesheet-source stylesheet)
       (map (lambda (rule)
              (make-rule-append-edit (css-stylesheet-source stylesheet)
                                     rule
                                     (serialize-new-declaration name value important?)))
            matching-rules)))]
    [else
     (css-append-declaration-by-pseudo stylesheet pseudo-name name value #:important? important?)]))

;; css-append-declaration-by-class : css-stylesheet? string? string? string? keyword-arguments -> css-stylesheet?
;;   Append a declaration to style rules that use a given class selector.
(define (css-append-declaration-by-class stylesheet class-name name value #:important? [important? #f])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-append-declaration-by-class "css-stylesheet?" stylesheet))
  (unless (string? class-name)
    (raise-argument-error 'css-append-declaration-by-class "string?" class-name))
  (unless (string? name)
    (raise-argument-error 'css-append-declaration-by-class "string?" name))
  (unless (string? value)
    (raise-argument-error 'css-append-declaration-by-class "string?" value))
  (css-stylesheet
   (append-by-selector-predicate-node-list
    (css-stylesheet-rules stylesheet)
    (lambda (rule)
      (rule-has-class? rule class-name))
    name
    value
   important?)
   #f
   #f))

;; css-append-declaration-by-class/preserve-source : css-stylesheet? string? string? string? keyword-arguments -> css-stylesheet?
;;   Append a declaration by class selector while preserving untouched source when possible.
(define (css-append-declaration-by-class/preserve-source stylesheet class-name name value #:important? [important? #f])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-append-declaration-by-class/preserve-source "css-stylesheet?" stylesheet))
  (unless (string? class-name)
    (raise-argument-error 'css-append-declaration-by-class/preserve-source "string?" class-name))
  (unless (string? name)
    (raise-argument-error 'css-append-declaration-by-class/preserve-source "string?" name))
  (unless (string? value)
    (raise-argument-error 'css-append-declaration-by-class/preserve-source "string?" value))
  (define matching-rules
    (matching-style-rules-by-class stylesheet class-name))
  (cond
    [(and (string? (css-stylesheet-source stylesheet))
          (source-editable-rules? matching-rules))
     (reparse-source
      (apply-source-edits
       (css-stylesheet-source stylesheet)
       (map (lambda (rule)
              (make-rule-append-edit (css-stylesheet-source stylesheet)
                                     rule
                                     (serialize-new-declaration name value important?)))
            matching-rules)))]
    [else
     (css-append-declaration-by-class stylesheet class-name name value #:important? important?)]))

;; css-append-declaration-by-attribute : css-stylesheet? string? string? string? keyword-arguments -> css-stylesheet?
;;   Append a declaration to style rules that use a given attribute selector name.
(define (css-append-declaration-by-attribute stylesheet attribute-name name value #:important? [important? #f])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-append-declaration-by-attribute "css-stylesheet?" stylesheet))
  (unless (string? attribute-name)
    (raise-argument-error 'css-append-declaration-by-attribute "string?" attribute-name))
  (unless (string? name)
    (raise-argument-error 'css-append-declaration-by-attribute "string?" name))
  (unless (string? value)
    (raise-argument-error 'css-append-declaration-by-attribute "string?" value))
  (css-stylesheet
   (append-by-selector-predicate-node-list
    (css-stylesheet-rules stylesheet)
    (lambda (rule)
      (rule-has-attribute? rule attribute-name))
    name
    value
   important?)
   #f
   #f))

;; css-append-declaration-by-attribute/preserve-source : css-stylesheet? string? string? string? keyword-arguments -> css-stylesheet?
;;   Append a declaration by attribute selector while preserving untouched source when possible.
(define (css-append-declaration-by-attribute/preserve-source stylesheet attribute-name name value #:important? [important? #f])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-append-declaration-by-attribute/preserve-source "css-stylesheet?" stylesheet))
  (unless (string? attribute-name)
    (raise-argument-error 'css-append-declaration-by-attribute/preserve-source "string?" attribute-name))
  (unless (string? name)
    (raise-argument-error 'css-append-declaration-by-attribute/preserve-source "string?" name))
  (unless (string? value)
    (raise-argument-error 'css-append-declaration-by-attribute/preserve-source "string?" value))
  (define matching-rules
    (matching-style-rules-by-attribute stylesheet attribute-name))
  (cond
    [(and (string? (css-stylesheet-source stylesheet))
          (source-editable-rules? matching-rules))
     (reparse-source
      (apply-source-edits
       (css-stylesheet-source stylesheet)
       (map (lambda (rule)
              (make-rule-append-edit (css-stylesheet-source stylesheet)
                                     rule
                                     (serialize-new-declaration name value important?)))
            matching-rules)))]
    [else
     (css-append-declaration-by-attribute stylesheet attribute-name name value #:important? important?)]))

;; css-rename-class : css-stylesheet? string? string? -> css-stylesheet?
;;   Rename one class selector throughout the stylesheet.
(define (css-rename-class stylesheet old-name new-name)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rename-class "css-stylesheet?" stylesheet))
  (unless (string? old-name)
    (raise-argument-error 'css-rename-class "string?" old-name))
  (unless (string? new-name)
    (raise-argument-error 'css-rename-class "string?" new-name))
  (css-map-selectors
   stylesheet
   (lambda (group)
     (rename-class-in-selector-group group old-name new-name))))

;; css-prefix-selectors : css-stylesheet? string? -> css-stylesheet?
;;   Prefix each selector group with one selector prefix.
(define (css-prefix-selectors stylesheet prefix)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-prefix-selectors "css-stylesheet?" stylesheet))
  (unless (string? prefix)
    (raise-argument-error 'css-prefix-selectors "string?" prefix))
  (define trimmed-prefix
    (string-trim prefix))
  (css-map-selectors
   stylesheet
   (lambda (group)
     (cond
       [(string=? trimmed-prefix "") group]
       [(string=? (string-trim group) "") trimmed-prefix]
       [else
        (string-append trimmed-prefix " " (string-trim group))]))))

;; css-rewrite-media-queries : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite @media preludes using a callback over the raw prelude and derived details.
(define (css-rewrite-media-queries stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rewrite-media-queries "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-rewrite-media-queries "procedure?" proc))
  (css-map-at-rules
   stylesheet
   (lambda (rule)
     (if (string-ci=? (css-at-rule-name rule) "@media")
         (let ([next-prelude (proc (css-at-rule-prelude rule)
                                   (css-at-rule-prelude-derived-details rule))])
           (unless (string? next-prelude)
             (raise-argument-error 'css-rewrite-media-queries "string?" next-prelude))
           (css-at-rule (css-at-rule-name rule)
                        next-prelude
                        (css-at-rule-block rule)
                        #f))
         rule))))

;; css-rewrite-supports-conditions : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite @supports preludes using a callback over the raw prelude and derived details.
(define (css-rewrite-supports-conditions stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rewrite-supports-conditions "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-rewrite-supports-conditions "procedure?" proc))
  (css-map-at-rules
   stylesheet
   (lambda (rule)
     (if (string-ci=? (css-at-rule-name rule) "@supports")
         (let ([next-prelude (proc (css-at-rule-prelude rule)
                                   (css-at-rule-prelude-derived-details rule))])
           (unless (string? next-prelude)
             (raise-argument-error 'css-rewrite-supports-conditions "string?" next-prelude))
           (css-at-rule (css-at-rule-name rule)
                        next-prelude
                        (css-at-rule-block rule)
                        #f))
         rule))))

;; css-rewrite-custom-properties : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite custom-property names in declarations and var() references.
(define (css-rewrite-custom-properties stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rewrite-custom-properties "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-rewrite-custom-properties "procedure?" proc))
  (css-stylesheet
   (rewrite-custom-properties-node-list (css-stylesheet-rules stylesheet) proc)
   #f
   #f))

;; css-split-grouped-selectors : css-stylesheet? -> css-stylesheet?
;;   Split grouped selector rules into one rule per selector group.
(define (css-split-grouped-selectors stylesheet)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-split-grouped-selectors "css-stylesheet?" stylesheet))
  (css-map-rules
   stylesheet
   (lambda (rule)
     (if (<= (length (css-style-rule-selector-groups rule)) 1)
         rule
         (map (lambda (group)
                (css-style-rule (list group)
                                (css-style-rule-block rule)
                                group
                                #f))
              (css-style-rule-selector-groups rule))))))

;; css-clone-rule : css-stylesheet? string? keyword-arguments -> css-stylesheet?
;;   Clone matching rules immediately after themselves, optionally transforming the clone.
(define (css-clone-rule stylesheet selector-group #:transform [proc values])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-clone-rule "css-stylesheet?" stylesheet))
  (unless (string? selector-group)
    (raise-argument-error 'css-clone-rule "string?" selector-group))
  (unless (procedure? proc)
    (raise-argument-error 'css-clone-rule "procedure?" proc))
  (css-stylesheet
   (clone-rule-node-list (css-stylesheet-rules stylesheet) selector-group proc)
   #f
   #f))

;; css-insert-rule-before : css-stylesheet? string? css-style-rule? -> css-stylesheet?
;;   Insert a style rule before each matching style rule.
(define (css-insert-rule-before stylesheet selector-group new-rule)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-insert-rule-before "css-stylesheet?" stylesheet))
  (unless (string? selector-group)
    (raise-argument-error 'css-insert-rule-before "string?" selector-group))
  (unless (css-style-rule? new-rule)
    (raise-argument-error 'css-insert-rule-before "css-style-rule?" new-rule))
  (css-stylesheet
   (insert-rule-node-list (css-stylesheet-rules stylesheet) selector-group new-rule 'before)
   #f
   #f))

;; css-insert-rule-after : css-stylesheet? string? css-style-rule? -> css-stylesheet?
;;   Insert a style rule after each matching style rule.
(define (css-insert-rule-after stylesheet selector-group new-rule)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-insert-rule-after "css-stylesheet?" stylesheet))
  (unless (string? selector-group)
    (raise-argument-error 'css-insert-rule-after "string?" selector-group))
  (unless (css-style-rule? new-rule)
    (raise-argument-error 'css-insert-rule-after "css-style-rule?" new-rule))
  (css-stylesheet
   (insert-rule-node-list (css-stylesheet-rules stylesheet) selector-group new-rule 'after)
   #f
   #f))

;; css-remove-rules : css-stylesheet? procedure? -> css-stylesheet?
;;   Remove style rules matching one predicate.
(define (css-remove-rules stylesheet pred?)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-remove-rules "css-stylesheet?" stylesheet))
  (unless (procedure? pred?)
    (raise-argument-error 'css-remove-rules "procedure?" pred?))
  (css-map-rules stylesheet (lambda (rule) (and (not (pred? rule)) rule))))

;; css-remove-at-rules : css-stylesheet? procedure? -> css-stylesheet?
;;   Remove at-rules matching one predicate.
(define (css-remove-at-rules stylesheet pred?)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-remove-at-rules "css-stylesheet?" stylesheet))
  (unless (procedure? pred?)
    (raise-argument-error 'css-remove-at-rules "procedure?" pred?))
  (css-map-at-rules stylesheet (lambda (rule) (and (not (pred? rule)) rule))))

;; css-wrap-rules-in-media : css-stylesheet? string? string? -> css-stylesheet?
;;   Wrap matching rules inside one new @media at-rule.
(define (css-wrap-rules-in-media stylesheet selector-group prelude)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-wrap-rules-in-media "css-stylesheet?" stylesheet))
  (unless (string? selector-group)
    (raise-argument-error 'css-wrap-rules-in-media "string?" selector-group))
  (unless (string? prelude)
    (raise-argument-error 'css-wrap-rules-in-media "string?" prelude))
  (css-stylesheet
   (wrap-rules-node-list (css-stylesheet-rules stylesheet) selector-group "@media" prelude)
   #f
   #f))

;; css-wrap-rules-in-supports : css-stylesheet? string? string? -> css-stylesheet?
;;   Wrap matching rules inside one new @supports at-rule.
(define (css-wrap-rules-in-supports stylesheet selector-group prelude)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-wrap-rules-in-supports "css-stylesheet?" stylesheet))
  (unless (string? selector-group)
    (raise-argument-error 'css-wrap-rules-in-supports "string?" selector-group))
  (unless (string? prelude)
    (raise-argument-error 'css-wrap-rules-in-supports "string?" prelude))
  (css-stylesheet
   (wrap-rules-node-list (css-stylesheet-rules stylesheet) selector-group "@supports" prelude)
   #f
   #f))

;; css-merge-adjacent-rules : css-stylesheet? -> css-stylesheet?
;;   Merge adjacent compatible rules recursively.
(define (css-merge-adjacent-rules stylesheet)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-merge-adjacent-rules "css-stylesheet?" stylesheet))
  (css-stylesheet
   (merge-adjacent-node-list (css-stylesheet-rules stylesheet))
   #f
   #f))

;; css-dedupe-declarations : css-stylesheet? keyword-arguments -> css-stylesheet?
;;   Remove duplicate declarations within style rules, keeping either the first or last.
(define (css-dedupe-declarations stylesheet #:keep [keep 'last])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-dedupe-declarations "css-stylesheet?" stylesheet))
  (unless (member keep '(first last))
    (raise-argument-error 'css-dedupe-declarations "(or/c 'first 'last)" keep))
  (css-stylesheet
   (dedupe-declarations-node-list (css-stylesheet-rules stylesheet) keep)
   #f
   #f))

;; css-sort-declarations : css-stylesheet? keyword-arguments -> css-stylesheet?
;;   Sort declarations within style rules using one comparator on declaration names.
(define (css-sort-declarations stylesheet #:less-than [less-than string<?])
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-sort-declarations "css-stylesheet?" stylesheet))
  (unless (procedure? less-than)
    (raise-argument-error 'css-sort-declarations "procedure?" less-than))
  (css-stylesheet
   (sort-declarations-node-list (css-stylesheet-rules stylesheet) less-than)
   #f
   #f))

;; css-rename-custom-property : css-stylesheet? string? string? -> css-stylesheet?
;;   Rename one custom property in declarations and var() references.
(define (css-rename-custom-property stylesheet old-name new-name)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rename-custom-property "css-stylesheet?" stylesheet))
  (unless (string? old-name)
    (raise-argument-error 'css-rename-custom-property "string?" old-name))
  (unless (string? new-name)
    (raise-argument-error 'css-rename-custom-property "string?" new-name))
  (css-rewrite-custom-properties stylesheet (lambda (name) (if (string=? name old-name) new-name name))))

;; css-rewrite-var-functions : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite custom-property names only in var() references.
(define (css-rewrite-var-functions stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rewrite-var-functions "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-rewrite-var-functions "procedure?" proc))
  (css-stylesheet
   (rewrite-var-functions-node-list (css-stylesheet-rules stylesheet) proc)
   #f
   #f))

;; css-rename-keyframes : css-stylesheet? string? string? -> css-stylesheet?
;;   Rename one keyframes identifier and matching animation references.
(define (css-rename-keyframes stylesheet old-name new-name)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rename-keyframes "css-stylesheet?" stylesheet))
  (unless (string? old-name)
    (raise-argument-error 'css-rename-keyframes "string?" old-name))
  (unless (string? new-name)
    (raise-argument-error 'css-rename-keyframes "string?" new-name))
  (css-stylesheet
   (rename-keyframes-node-list (css-stylesheet-rules stylesheet) old-name new-name)
   #f
   #f))

;; css-rewrite-imports : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite @import preludes using one callback over the raw prelude string.
(define (css-rewrite-imports stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rewrite-imports "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-rewrite-imports "procedure?" proc))
  (css-map-at-rules
   stylesheet
   (lambda (rule)
     (if (string-ci=? (css-at-rule-name rule) "@import")
         (let ([next-prelude (proc (css-at-rule-prelude rule))])
           (unless (string? next-prelude)
             (raise-argument-error 'css-rewrite-imports "string?" next-prelude))
           (css-at-rule "@import" next-prelude (css-at-rule-block rule) #f))
         rule))))

;; css-rewrite-font-face : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite declarations inside @font-face blocks.
(define (css-rewrite-font-face stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rewrite-font-face "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-rewrite-font-face "procedure?" proc))
  (css-stylesheet
   (rewrite-font-face-node-list (css-stylesheet-rules stylesheet) proc)
   #f
   #f))

;; css-rewrite-url-values : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite url(...) inner text in declarations and at-rule preludes.
(define (css-rewrite-url-values stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rewrite-url-values "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-rewrite-url-values "procedure?" proc))
  (css-stylesheet
   (rewrite-url-values-node-list (css-stylesheet-rules stylesheet) proc)
   #f
   #f))

;; css-filter-comments : css-stylesheet? procedure? -> css-stylesheet?
;;   Keep only comments for which the predicate returns true.
(define (css-filter-comments stylesheet pred?)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-filter-comments "css-stylesheet?" stylesheet))
  (unless (procedure? pred?)
    (raise-argument-error 'css-filter-comments "procedure?" pred?))
  (css-stylesheet
   (filter-comments-node-list (css-stylesheet-rules stylesheet) pred?)
   #f
   #f))

;; css-hoist-nested-rules : css-stylesheet? -> css-stylesheet?
;;   Hoist nested style rules into flat rule lists by combining selectors.
(define (css-hoist-nested-rules stylesheet)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-hoist-nested-rules "css-stylesheet?" stylesheet))
  (css-stylesheet
   (hoist-nested-node-list (css-stylesheet-rules stylesheet) #f)
   #f
   #f))

;; css-lower-nesting : css-stylesheet? -> css-stylesheet?
;;   Lower nesting by hoisting nested rules into flat rule lists.
(define (css-lower-nesting stylesheet)
  (css-hoist-nested-rules stylesheet))

;; css-rewrite-attribute-selectors : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite raw attribute-selector text in selector groups.
(define (css-rewrite-attribute-selectors stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rewrite-attribute-selectors "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-rewrite-attribute-selectors "procedure?" proc))
  (css-map-selectors stylesheet
                     (lambda (group)
                       (regexp-replace* #px"\\[[^\\]]*\\]"
                                        group
                                        (lambda (text)
                                          (define next (proc text))
                                          (unless (string? next)
                                            (raise-argument-error 'css-rewrite-attribute-selectors "string?" next))
                                          next)))))

;; css-rewrite-pseudos : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite raw pseudo-selector text in selector groups.
(define (css-rewrite-pseudos stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rewrite-pseudos "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-rewrite-pseudos "procedure?" proc))
  (css-map-selectors stylesheet
                     (lambda (group)
                       (regexp-replace* #px"::?[A-Za-z_-][A-Za-z0-9_-]*(\\([^)]*\\))?"
                                        group
                                        (lambda (text _args)
                                          (define next (proc text))
                                          (unless (string? next)
                                            (raise-argument-error 'css-rewrite-pseudos "string?" next))
                                          next)))))

;; css-rewrite-selector-structure : css-stylesheet? procedure? -> css-stylesheet?
;;   Rewrite selector groups using both raw text and derived selector structure.
(define (css-rewrite-selector-structure stylesheet proc)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-rewrite-selector-structure "css-stylesheet?" stylesheet))
  (unless (procedure? proc)
    (raise-argument-error 'css-rewrite-selector-structure "procedure?" proc))
  (css-map-rules
   stylesheet
   (lambda (rule)
     (define derived
       (css-style-rule-selectors rule))
     (define new-groups
       (map (lambda (group selector)
              (define next (proc group selector))
              (unless (string? next)
                (raise-argument-error 'css-rewrite-selector-structure "string?" next))
              next)
            (css-style-rule-selector-groups rule)
            derived))
     (css-style-rule new-groups
                     (css-style-rule-block rule)
                     (string-join new-groups ", ")
                     #f))))

;; css-update-declaration-values-in-media-feature : css-stylesheet? string? string? (string? -> string?) -> css-stylesheet?
;;   Update declaration values only inside @media rules matching feature-name.
(define (css-update-declaration-values-in-media-feature stylesheet feature-name property-name updater)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-update-declaration-values-in-media-feature "css-stylesheet?" stylesheet))
  (unless (string? feature-name)
    (raise-argument-error 'css-update-declaration-values-in-media-feature "string?" feature-name))
  (unless (string? property-name)
    (raise-argument-error 'css-update-declaration-values-in-media-feature "string?" property-name))
  (unless (procedure? updater)
    (raise-argument-error 'css-update-declaration-values-in-media-feature "procedure?" updater))
  (css-stylesheet
   (rewrite-node-list-in-context (css-stylesheet-rules stylesheet)
                                 (lambda (declaration context)
                                   (if (and (context-has-key? context 'media-feature-names)
                                            (member-ci? feature-name (context-ref context 'media-feature-names))
                                            (string-ci=? (css-declaration-name declaration) property-name))
                                       (css-declaration (css-declaration-name declaration)
                                                        (updater (css-declaration-value declaration))
                                                        (css-declaration-important? declaration)
                                                        #f)
                                       declaration))
                                 '())
   #f
   #f))

;; css-update-declaration-values-in-media-feature/preserve-source : css-stylesheet? string? string? (string? -> string?) -> css-stylesheet?
;;   Update declaration values in @media rules while preserving untouched source text when possible.
(define (css-update-declaration-values-in-media-feature/preserve-source stylesheet feature-name property-name updater)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-update-declaration-values-in-media-feature/preserve-source "css-stylesheet?" stylesheet))
  (unless (string? feature-name)
    (raise-argument-error 'css-update-declaration-values-in-media-feature/preserve-source "string?" feature-name))
  (unless (string? property-name)
    (raise-argument-error 'css-update-declaration-values-in-media-feature/preserve-source "string?" property-name))
  (unless (procedure? updater)
    (raise-argument-error 'css-update-declaration-values-in-media-feature/preserve-source "procedure?" updater))
  (define matching-decls
    (matching-declarations-in-context stylesheet 'media-feature-names feature-name property-name))
  (cond
    [(and (string? (css-stylesheet-source stylesheet))
          (source-editable-declarations? matching-decls))
     (reparse-source
      (apply-source-edits
       (css-stylesheet-source stylesheet)
       (map (lambda (declaration)
              (make-source-edit declaration
                                (serialize-declaration-like
                                 (css-stylesheet-source stylesheet)
                                 declaration
                                 (updater (css-declaration-value declaration)))))
            matching-decls)))]
    [else
     (css-update-declaration-values-in-media-feature stylesheet feature-name property-name updater)]))

;; css-remove-declarations-in-media-feature : css-stylesheet? string? string? -> css-stylesheet?
;;   Remove declarations only inside @media rules matching feature-name.
(define (css-remove-declarations-in-media-feature stylesheet feature-name property-name)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-remove-declarations-in-media-feature "css-stylesheet?" stylesheet))
  (unless (string? feature-name)
    (raise-argument-error 'css-remove-declarations-in-media-feature "string?" feature-name))
  (unless (string? property-name)
    (raise-argument-error 'css-remove-declarations-in-media-feature "string?" property-name))
  (css-stylesheet
   (rewrite-node-list-in-context (css-stylesheet-rules stylesheet)
                                 (lambda (declaration context)
                                   (and (not (and (context-has-key? context 'media-feature-names)
                                                  (member-ci? feature-name (context-ref context 'media-feature-names))
                                                  (string-ci=? (css-declaration-name declaration) property-name)))
                                        declaration))
                                 '())
   #f
   #f))

;; css-remove-declarations-in-media-feature/preserve-source : css-stylesheet? string? string? -> css-stylesheet?
;;   Remove declarations in @media rules while preserving untouched source text when possible.
(define (css-remove-declarations-in-media-feature/preserve-source stylesheet feature-name property-name)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-remove-declarations-in-media-feature/preserve-source "css-stylesheet?" stylesheet))
  (unless (string? feature-name)
    (raise-argument-error 'css-remove-declarations-in-media-feature/preserve-source "string?" feature-name))
  (unless (string? property-name)
    (raise-argument-error 'css-remove-declarations-in-media-feature/preserve-source "string?" property-name))
  (define matching-decls
    (matching-declarations-in-context stylesheet 'media-feature-names feature-name property-name))
  (cond
    [(and (string? (css-stylesheet-source stylesheet))
          (source-editable-declarations? matching-decls))
     (reparse-source
      (apply-source-edits
       (css-stylesheet-source stylesheet)
       (map (lambda (declaration)
              (make-source-edit declaration ""))
            matching-decls)))]
    [else
     (css-remove-declarations-in-media-feature stylesheet feature-name property-name)]))

;; css-update-declaration-values-in-supports-feature : css-stylesheet? string? string? (string? -> string?) -> css-stylesheet?
;;   Update declaration values only inside @supports rules matching feature-name.
(define (css-update-declaration-values-in-supports-feature stylesheet feature-name property-name updater)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-update-declaration-values-in-supports-feature "css-stylesheet?" stylesheet))
  (unless (string? feature-name)
    (raise-argument-error 'css-update-declaration-values-in-supports-feature "string?" feature-name))
  (unless (string? property-name)
    (raise-argument-error 'css-update-declaration-values-in-supports-feature "string?" property-name))
  (unless (procedure? updater)
    (raise-argument-error 'css-update-declaration-values-in-supports-feature "procedure?" updater))
  (css-stylesheet
   (rewrite-node-list-in-context (css-stylesheet-rules stylesheet)
                                 (lambda (declaration context)
                                   (if (and (context-has-key? context 'supports-feature-names)
                                            (member-ci? feature-name (context-ref context 'supports-feature-names))
                                            (string-ci=? (css-declaration-name declaration) property-name))
                                       (css-declaration (css-declaration-name declaration)
                                                        (updater (css-declaration-value declaration))
                                                        (css-declaration-important? declaration)
                                                        #f)
                                       declaration))
                                 '())
   #f
   #f))

;; css-update-declaration-values-in-supports-feature/preserve-source : css-stylesheet? string? string? (string? -> string?) -> css-stylesheet?
;;   Update declaration values in @supports rules while preserving untouched source text when possible.
(define (css-update-declaration-values-in-supports-feature/preserve-source stylesheet feature-name property-name updater)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-update-declaration-values-in-supports-feature/preserve-source "css-stylesheet?" stylesheet))
  (unless (string? feature-name)
    (raise-argument-error 'css-update-declaration-values-in-supports-feature/preserve-source "string?" feature-name))
  (unless (string? property-name)
    (raise-argument-error 'css-update-declaration-values-in-supports-feature/preserve-source "string?" property-name))
  (unless (procedure? updater)
    (raise-argument-error 'css-update-declaration-values-in-supports-feature/preserve-source "procedure?" updater))
  (define matching-decls
    (matching-declarations-in-context stylesheet 'supports-feature-names feature-name property-name))
  (cond
    [(and (string? (css-stylesheet-source stylesheet))
          (source-editable-declarations? matching-decls))
     (reparse-source
      (apply-source-edits
       (css-stylesheet-source stylesheet)
       (map (lambda (declaration)
              (make-source-edit declaration
                                (serialize-declaration-like
                                 (css-stylesheet-source stylesheet)
                                 declaration
                                 (updater (css-declaration-value declaration)))))
            matching-decls)))]
    [else
     (css-update-declaration-values-in-supports-feature stylesheet feature-name property-name updater)]))

;; css-remove-declarations-in-supports-feature : css-stylesheet? string? string? -> css-stylesheet?
;;   Remove declarations only inside @supports rules matching feature-name.
(define (css-remove-declarations-in-supports-feature stylesheet feature-name property-name)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-remove-declarations-in-supports-feature "css-stylesheet?" stylesheet))
  (unless (string? feature-name)
    (raise-argument-error 'css-remove-declarations-in-supports-feature "string?" feature-name))
  (unless (string? property-name)
    (raise-argument-error 'css-remove-declarations-in-supports-feature "string?" property-name))
  (css-stylesheet
   (rewrite-node-list-in-context (css-stylesheet-rules stylesheet)
                                 (lambda (declaration context)
                                   (and (not (and (context-has-key? context 'supports-feature-names)
                                                  (member-ci? feature-name (context-ref context 'supports-feature-names))
                                                  (string-ci=? (css-declaration-name declaration) property-name)))
                                        declaration))
                                 '())
   #f
   #f))

;; css-remove-declarations-in-supports-feature/preserve-source : css-stylesheet? string? string? -> css-stylesheet?
;;   Remove declarations in @supports rules while preserving untouched source text when possible.
(define (css-remove-declarations-in-supports-feature/preserve-source stylesheet feature-name property-name)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error 'css-remove-declarations-in-supports-feature/preserve-source "css-stylesheet?" stylesheet))
  (unless (string? feature-name)
    (raise-argument-error 'css-remove-declarations-in-supports-feature/preserve-source "string?" feature-name))
  (unless (string? property-name)
    (raise-argument-error 'css-remove-declarations-in-supports-feature/preserve-source "string?" property-name))
  (define matching-decls
    (matching-declarations-in-context stylesheet 'supports-feature-names feature-name property-name))
  (cond
    [(and (string? (css-stylesheet-source stylesheet))
          (source-editable-declarations? matching-decls))
     (reparse-source
      (apply-source-edits
       (css-stylesheet-source stylesheet)
       (map (lambda (declaration)
              (make-source-edit declaration ""))
            matching-decls)))]
    [else
     (css-remove-declarations-in-supports-feature stylesheet feature-name property-name)]))

;; rewrite-node-list : list? procedure? -> list?
;;   Rewrite a list of AST nodes recursively.
(define (rewrite-node-list nodes proc)
  (append-map (lambda (node)
                (rewrite-node node proc))
              nodes))

;; rewrite-node-list-in-context : list? procedure? list? -> list?
;;   Rewrite a list of AST nodes while carrying a rewrite context.
(define (rewrite-node-list-in-context nodes proc context)
  (append-map (lambda (node)
                (rewrite-node-in-context node proc context))
              nodes))

;; rewrite-node : any/c procedure? -> list?
;;   Rewrite one AST node recursively, returning zero or one replacement node.
(define (rewrite-node node proc)
  (cond
    [(css-comment? node)
     (list node)]
    [(css-recovery? node)
     (list node)]
    [(css-declaration? node)
     (define rewritten
       (proc node))
     (if rewritten
         (list rewritten)
         '())]
    [(css-style-rule? node)
     (list
      (css-style-rule (css-style-rule-selector-groups node)
                      (rewrite-node-list (css-style-rule-block node) proc)
                      (css-style-rule-raw-selector node)
                      (css-style-rule-span node)))]
    [(css-at-rule? node)
     (list
      (css-at-rule (css-at-rule-name node)
                   (css-at-rule-prelude node)
                   (and (list? (css-at-rule-block node))
                        (rewrite-node-list (css-at-rule-block node) proc))
                   (css-at-rule-span node)))]
    [else
     (list node)]))

;; rewrite-node-in-context : any/c procedure? list? -> list?
;;   Rewrite one AST node recursively while carrying contextual scopes.
(define (rewrite-node-in-context node proc context)
  (cond
    [(css-comment? node)
     (list node)]
    [(css-recovery? node)
     (list node)]
    [(css-declaration? node)
     (define rewritten
       (proc node context))
     (if rewritten (list rewritten) '())]
    [(css-style-rule? node)
     (list
      (css-style-rule (css-style-rule-selector-groups node)
                      (rewrite-node-list-in-context (css-style-rule-block node) proc context)
                      (css-style-rule-raw-selector node)
                      (css-style-rule-span node)))]
    [(css-at-rule? node)
     (define next-context
       (extend-context-for-at-rule context node))
     (list
      (css-at-rule (css-at-rule-name node)
                   (css-at-rule-prelude node)
                   (and (list? (css-at-rule-block node))
                        (rewrite-node-list-in-context (css-at-rule-block node) proc next-context))
                   (css-at-rule-span node)))]
    [else
     (list node)]))

;; map-style-rule-node-list : list? procedure? -> list?
;;   Rewrite style rules recursively, allowing removal or replacement.
(define (map-style-rule-node-list nodes proc)
  (append-map (lambda (node)
                (map-style-rule-node node proc))
              nodes))

;; map-style-rule-node : any/c procedure? -> list?
;;   Rewrite style rules recursively in one node.
(define (map-style-rule-node node proc)
  (cond
    [(css-style-rule? node)
     (normalize-style-rule-results
      (proc (css-style-rule (css-style-rule-selector-groups node)
                            (map-style-rule-node-list (css-style-rule-block node) proc)
                            (css-style-rule-raw-selector node)
                            #f)))]
    [(css-at-rule? node)
     (list
      (css-at-rule (css-at-rule-name node)
                   (css-at-rule-prelude node)
                   (and (list? (css-at-rule-block node))
                        (map-style-rule-node-list (css-at-rule-block node) proc))
                   #f))]
    [else
     (list node)]))

;; map-at-rule-node-list : list? procedure? -> list?
;;   Rewrite at-rules recursively, allowing removal or replacement.
(define (map-at-rule-node-list nodes proc)
  (append-map (lambda (node)
                (map-at-rule-node node proc))
              nodes))

;; map-at-rule-node : any/c procedure? -> list?
;;   Rewrite at-rules recursively in one node.
(define (map-at-rule-node node proc)
  (cond
    [(css-style-rule? node)
     (list
      (css-style-rule (css-style-rule-selector-groups node)
                      (map-at-rule-node-list (css-style-rule-block node) proc)
                      (css-style-rule-raw-selector node)
                      #f))]
    [(css-at-rule? node)
     (normalize-at-rule-results
      (proc (css-at-rule (css-at-rule-name node)
                         (css-at-rule-prelude node)
                         (and (list? (css-at-rule-block node))
                              (map-at-rule-node-list (css-at-rule-block node) proc))
                         #f)))]
    [else
     (list node)]))

;; normalize-style-rule-results : any/c -> list?
;;   Normalize style-rule mapper results into zero or more style-rule nodes.
(define (normalize-style-rule-results result)
  (cond
    [(not result) '()]
    [(css-style-rule? result) (list result)]
    [(and (list? result) (andmap css-style-rule? result)) result]
    [else
     (raise-argument-error 'css-map-rules
                           "(or/c #f css-style-rule? (listof css-style-rule?))"
                           result)]))

;; normalize-at-rule-results : any/c -> list?
;;   Normalize at-rule mapper results into zero or more at-rule nodes.
(define (normalize-at-rule-results result)
  (cond
    [(not result) '()]
    [(css-at-rule? result) (list result)]
    [(and (list? result) (andmap css-at-rule? result)) result]
    [else
     (raise-argument-error 'css-map-at-rules
                           "(or/c #f css-at-rule? (listof css-at-rule?))"
                           result)]))

;; append-declaration-node-list : list? string? string? string? boolean? -> list?
;;   Append declarations recursively where selector-group matches.
(define (append-declaration-node-list nodes selector-group name value important?)
  (map (lambda (node)
         (append-declaration-node node selector-group name value important?))
       nodes))

;; append-declaration-node : any/c string? string? string? boolean? -> any/c
;;   Append one declaration recursively where applicable.
(define (append-declaration-node node selector-group name value important?)
  (cond
    [(css-style-rule? node)
     (if (member selector-group (css-style-rule-selector-groups node))
         (css-style-rule (css-style-rule-selector-groups node)
                         (append (css-style-rule-block node)
                                 (list (css-declaration name value important? #f)))
                         (css-style-rule-raw-selector node)
                         (css-style-rule-span node))
         (css-style-rule (css-style-rule-selector-groups node)
                         (append-declaration-node-list (css-style-rule-block node)
                                                       selector-group
                                                       name
                                                       value
                                                       important?)
                         (css-style-rule-raw-selector node)
                         (css-style-rule-span node)))]
    [(css-at-rule? node)
     (css-at-rule (css-at-rule-name node)
                  (css-at-rule-prelude node)
                  (and (list? (css-at-rule-block node))
                       (append-declaration-node-list (css-at-rule-block node)
                                                     selector-group
                                                     name
                                                     value
                                                     important?))
                  (css-at-rule-span node))]
    [else
     node]))

;; append-by-pseudo-node-list : list? string? string? string? boolean? -> list?
;;   Append declarations recursively where a rule uses pseudo-name.
(define (append-by-pseudo-node-list nodes pseudo-name name value important?)
  (map (lambda (node)
         (append-by-pseudo-node node pseudo-name name value important?))
       nodes))

;; append-by-pseudo-node : any/c string? string? string? boolean? -> any/c
;;   Append a declaration where a style rule uses the given pseudo.
(define (append-by-pseudo-node node pseudo-name name value important?)
  (append-by-selector-predicate-node node
                                     (lambda (rule)
                                       (rule-has-pseudo? rule pseudo-name))
                                     name
                                     value
                                     important?))

;; append-by-selector-predicate-node-list : list? procedure? string? string? boolean? -> list?
;;   Append declarations recursively where a style-rule predicate matches.
(define (append-by-selector-predicate-node-list nodes pred? name value important?)
  (map (lambda (node)
         (append-by-selector-predicate-node node pred? name value important?))
       nodes))

;; append-by-selector-predicate-node : any/c procedure? string? string? boolean? -> any/c
;;   Append a declaration where one style-rule predicate matches.
(define (append-by-selector-predicate-node node pred? name value important?)
  (cond
    [(css-style-rule? node)
     (if (pred? node)
         (css-style-rule (css-style-rule-selector-groups node)
                         (append (css-style-rule-block node)
                                 (list (css-declaration name value important? #f)))
                         (css-style-rule-raw-selector node)
                         (css-style-rule-span node))
         (css-style-rule (css-style-rule-selector-groups node)
                         (append-by-selector-predicate-node-list (css-style-rule-block node)
                                                                 pred?
                                                                 name
                                                                 value
                                                                 important?)
                         (css-style-rule-raw-selector node)
                         (css-style-rule-span node)))]
    [(css-at-rule? node)
     (css-at-rule (css-at-rule-name node)
                  (css-at-rule-prelude node)
                  (and (list? (css-at-rule-block node))
                       (append-by-selector-predicate-node-list (css-at-rule-block node)
                                                               pred?
                                                               name
                                                               value
                                                               important?))
                  (css-at-rule-span node))]
    [else
     node]))

;; extend-context-for-at-rule : list? css-at-rule? -> list?
;;   Extend rewrite context with derived at-rule information.
(define (extend-context-for-at-rule context rule)
  (cond
    [(string-ci=? (css-at-rule-name rule) "@media")
     (define details
       (css-at-rule-prelude-derived-details rule))
     (if (css-media-prelude-details? details)
         (context-set context 'media-feature-names (media-feature-names details))
         context)]
    [(string-ci=? (css-at-rule-name rule) "@supports")
     (define details
       (css-at-rule-prelude-derived-details rule))
     (if (css-supports-prelude-details? details)
         (context-set context 'supports-feature-names (supports-feature-names details))
         context)]
    [else
     context]))

;; media-feature-names : css-media-prelude-details? -> list?
;;   Collect feature names from derived media details.
(define (media-feature-names details)
  (remove-duplicates
   (append-map media-query-feature-names
               (css-media-prelude-details-queries details))
   string-ci=?))

;; media-query-feature-names : css-media-query? -> list?
;;   Collect feature names from one derived media query.
(define (media-query-feature-names query)
  (append-map (lambda (feature)
                (cond
                  [(css-media-feature-expression? feature)
                   (list (css-media-feature-expression-name feature))]
                  [(css-media-feature-range? feature)
                   (list (css-media-feature-range-name feature))]
                  [else
                   '()]))
              (css-media-query-features query)))

;; supports-feature-names : css-supports-prelude-details? -> list?
;;   Collect feature names from derived supports details.
(define (supports-feature-names details)
  (remove-duplicates
   (append-map supports-feature-names-in-condition
               (css-supports-prelude-details-conditions details))
   string-ci=?))

;; supports-feature-names-in-condition : css-supports-condition? -> list?
;;   Collect feature names recursively from one supports condition.
(define (supports-feature-names-in-condition condition)
  (cond
    [(not (css-supports-condition? condition))
     '()]
    [(eq? (css-supports-condition-kind condition) 'feature)
     (map css-supports-feature-name
          (filter css-supports-feature?
                  (css-supports-condition-arguments condition)))]
    [else
     (append-map supports-feature-names-in-condition
                 (filter css-supports-condition?
                         (css-supports-condition-arguments condition)))]))

;; rule-has-pseudo? : css-style-rule? string? -> boolean?
;;   Determine whether a rule uses the given pseudo selector name.
(define (rule-has-pseudo? rule pseudo-name)
  (ormap (lambda (selector)
           (ormap (lambda (part)
                    (and (css-selector-pseudo? part)
                         (string-ci=? (css-selector-pseudo-name part) pseudo-name)))
                  (css-selector-parts selector)))
         (css-style-rule-selectors rule)))

;; rule-has-class? : css-style-rule? string? -> boolean?
;;   Determine whether a rule uses the given class selector name.
(define (rule-has-class? rule class-name)
  (ormap (lambda (selector)
           (ormap (lambda (part)
                    (and (css-selector-class? part)
                         (string-ci=? (css-selector-class-name part) class-name)))
                  (css-selector-parts selector)))
         (css-style-rule-selectors rule)))

;; rule-has-attribute? : css-style-rule? string? -> boolean?
;;   Determine whether a rule uses the given attribute selector name.
(define (rule-has-attribute? rule attribute-name)
  (ormap (lambda (selector)
           (ormap (lambda (part)
                    (and (css-selector-attribute? part)
                         (string-ci=? (css-selector-attribute-name part) attribute-name)))
                  (css-selector-parts selector)))
         (css-style-rule-selectors rule)))

;; rename-class-in-selector-group : string? string? string? -> string?
;;   Rename one class selector inside one selector group string.
(define (rename-class-in-selector-group group old-name new-name)
  (regexp-replace*
   (pregexp (string-append "(^|[^A-Za-z0-9_-])\\."
                           (regexp-quote old-name)
                           "(?![A-Za-z0-9_-])"))
   group
   (lambda (match prefix)
     (string-append prefix "." new-name))))

;; replace-css-identifier : string? string? string? -> string?
;;   Replace one CSS identifier occurrence using simple boundary checks.
(define (replace-css-identifier text old-name new-name)
  (regexp-replace*
   (pregexp (string-append "(^|[^A-Za-z0-9_-])"
                           "("
                           (regexp-quote old-name)
                           ")"
                           "(?![A-Za-z0-9_-])"))
   text
   (lambda (full prefix _name)
     (string-append prefix new-name))))

;; rewrite-custom-properties-node-list : list? procedure? -> list?
;;   Rewrite custom-property declarations and value references recursively.
(define (rewrite-custom-properties-node-list nodes proc)
  (map (lambda (node)
         (rewrite-custom-properties-node node proc))
       nodes))

;; rewrite-custom-properties-node : any/c procedure? -> any/c
;;   Rewrite custom-property declarations and value references in one node.
(define (rewrite-custom-properties-node node proc)
  (cond
    [(css-declaration? node)
     (define next-name
       (if (custom-property-name? (css-declaration-name node))
           (proc (css-declaration-name node))
           (css-declaration-name node)))
     (unless (string? next-name)
       (raise-argument-error 'css-rewrite-custom-properties "string?" next-name))
     (css-declaration next-name
                      (rewrite-custom-property-references (css-declaration-value node) proc)
                      (css-declaration-important? node)
                      #f)]
    [(css-style-rule? node)
     (css-style-rule (css-style-rule-selector-groups node)
                     (rewrite-custom-properties-node-list (css-style-rule-block node) proc)
                     (css-style-rule-raw-selector node)
                     #f)]
    [(css-at-rule? node)
     (css-at-rule (css-at-rule-name node)
                  (rewrite-custom-property-references (css-at-rule-prelude node) proc)
                  (and (list? (css-at-rule-block node))
                       (rewrite-custom-properties-node-list (css-at-rule-block node) proc))
                  #f)]
    [else
     node]))

;; custom-property-name? : string? -> boolean?
;;   Determine whether a string names a CSS custom property.
(define (custom-property-name? text)
  (string-prefix? text "--"))

;; rewrite-custom-property-references : string? procedure? -> string?
;;   Rewrite var() references to custom properties in one string.
(define (rewrite-custom-property-references text proc)
  (regexp-replace*
   #px"var\\((\\s*)(--[A-Za-z0-9_-]+)"
   text
   (lambda (full leading-space property-name)
     (define next-name
       (proc property-name))
     (unless (string? next-name)
       (raise-argument-error 'css-rewrite-custom-properties "string?" next-name))
     (string-append "var(" leading-space next-name))))

;; rewrite-url-text : string? procedure? -> string?
;;   Rewrite inner url(...) text using one callback.
(define (rewrite-url-text text proc)
  (regexp-replace*
   #px"url\\(([^)]*)\\)"
   text
   (lambda (full inner)
     (define next
       (proc inner))
     (unless (string? next)
       (raise-argument-error 'css-rewrite-url-values "string?" next))
     (string-append "url(" next ")"))))

;; clone-rule-node-list : list? string? procedure? -> list?
;;   Clone matching rules recursively in node lists.
(define (clone-rule-node-list nodes selector-group proc)
  (append-map (lambda (node)
                (clone-rule-node node selector-group proc))
              nodes))

;; clone-rule-node : any/c string? procedure? -> list?
;;   Clone matching rules recursively in one node.
(define (clone-rule-node node selector-group proc)
  (cond
    [(css-style-rule? node)
     (define rewritten-rule
       (css-style-rule (css-style-rule-selector-groups node)
                       (clone-rule-node-list (css-style-rule-block node) selector-group proc)
                       (css-style-rule-raw-selector node)
                       #f))
     (if (member selector-group (css-style-rule-selector-groups rewritten-rule))
         (let ([clone (proc rewritten-rule)])
           (unless (css-style-rule? clone)
             (raise-argument-error 'css-clone-rule "css-style-rule?" clone))
           (list rewritten-rule clone))
         (list rewritten-rule))]
    [(css-at-rule? node)
     (list
      (css-at-rule (css-at-rule-name node)
                   (css-at-rule-prelude node)
                   (and (list? (css-at-rule-block node))
                        (clone-rule-node-list (css-at-rule-block node) selector-group proc))
                   #f))]
    [else
     (list node)]))

;; insert-rule-node-list : list? string? css-style-rule? symbol? -> list?
;;   Insert a rule before or after matching rules recursively in node lists.
(define (insert-rule-node-list nodes selector-group new-rule placement)
  (append-map (lambda (node)
                (insert-rule-node node selector-group new-rule placement))
              nodes))

;; insert-rule-node : any/c string? css-style-rule? symbol? -> list?
;;   Insert a rule before or after matching rules recursively in one node.
(define (insert-rule-node node selector-group new-rule placement)
  (cond
    [(css-style-rule? node)
     (define rewritten-rule
       (css-style-rule (css-style-rule-selector-groups node)
                       (insert-rule-node-list (css-style-rule-block node) selector-group new-rule placement)
                       (css-style-rule-raw-selector node)
                       #f))
     (if (member selector-group (css-style-rule-selector-groups rewritten-rule))
         (if (eq? placement 'before)
             (list new-rule rewritten-rule)
             (list rewritten-rule new-rule))
         (list rewritten-rule))]
    [(css-at-rule? node)
     (list
      (css-at-rule (css-at-rule-name node)
                   (css-at-rule-prelude node)
                   (and (list? (css-at-rule-block node))
                        (insert-rule-node-list (css-at-rule-block node) selector-group new-rule placement))
                   #f))]
    [else
     (list node)]))

;; rewrite-declarations-in-matching-rules : list? string? procedure? -> list?
;;   Rewrite declarations recursively in rules that contain one exact selector group.
(define (rewrite-declarations-in-matching-rules nodes selector-group proc)
  (map (lambda (node)
         (rewrite-declarations-in-matching-rules/node node selector-group proc))
       nodes))

;; rewrite-declarations-in-matching-rules/node : any/c string? procedure? -> any/c
;;   Rewrite declarations in one node when the containing rule matches.
(define (rewrite-declarations-in-matching-rules/node node selector-group proc)
  (cond
    [(css-style-rule? node)
     (define matched?
       (member selector-group (css-style-rule-selector-groups node)))
     (css-style-rule (css-style-rule-selector-groups node)
                     (if matched?
                         (rewrite-node-list (css-style-rule-block node) proc)
                         (rewrite-declarations-in-matching-rules (css-style-rule-block node)
                                                                 selector-group
                                                                 proc))
                     (css-style-rule-raw-selector node)
                     #f)]
    [(css-at-rule? node)
     (css-at-rule (css-at-rule-name node)
                  (css-at-rule-prelude node)
                  (and (list? (css-at-rule-block node))
                       (rewrite-declarations-in-matching-rules (css-at-rule-block node)
                                                               selector-group
                                                               proc))
                  #f)]
    [else
     node]))

;; wrap-rules-node-list : list? string? string? string? -> list?
;;   Wrap matching rules recursively in one at-rule.
(define (wrap-rules-node-list nodes selector-group at-rule-name prelude)
  (append-map (lambda (node)
                (wrap-rules-node node selector-group at-rule-name prelude))
              nodes))

;; wrap-rules-node : any/c string? string? string? -> list?
;;   Wrap matching rules recursively in one node.
(define (wrap-rules-node node selector-group at-rule-name prelude)
  (cond
    [(css-style-rule? node)
     (define rewritten
       (css-style-rule (css-style-rule-selector-groups node)
                       (wrap-rules-node-list (css-style-rule-block node) selector-group at-rule-name prelude)
                       (css-style-rule-raw-selector node)
                       #f))
     (if (member selector-group (css-style-rule-selector-groups rewritten))
         (list (css-at-rule at-rule-name prelude (list rewritten) #f))
         (list rewritten))]
    [(css-at-rule? node)
     (list
      (css-at-rule (css-at-rule-name node)
                   (css-at-rule-prelude node)
                   (and (list? (css-at-rule-block node))
                        (wrap-rules-node-list (css-at-rule-block node) selector-group at-rule-name prelude))
                   #f))]
    [else
     (list node)]))

;; merge-adjacent-node-list : list? -> list?
;;   Merge adjacent compatible rules recursively in one node list.
(define (merge-adjacent-node-list nodes)
  (let loop ([remaining (map merge-node-recursively nodes)]
             [acc '()])
    (cond
      [(null? remaining)
       (reverse acc)]
      [(null? (cdr remaining))
       (reverse (cons (car remaining) acc))]
      [else
       (define first (car remaining))
       (define second (cadr remaining))
       (cond
         [(mergeable-style-rules? first second)
          (loop (cons (merge-style-rules first second) (cddr remaining)) acc)]
         [(mergeable-at-rules? first second)
          (loop (cons (merge-at-rules first second) (cddr remaining)) acc)]
         [else
          (loop (cdr remaining) (cons first acc))])])))

;; merge-node-recursively : any/c -> any/c
;;   Recursively merge inside nested blocks before outer merging.
(define (merge-node-recursively node)
  (cond
    [(css-style-rule? node)
     (css-style-rule (css-style-rule-selector-groups node)
                     (merge-adjacent-node-list (css-style-rule-block node))
                     (css-style-rule-raw-selector node)
                     #f)]
    [(css-at-rule? node)
     (css-at-rule (css-at-rule-name node)
                  (css-at-rule-prelude node)
                  (and (list? (css-at-rule-block node))
                       (merge-adjacent-node-list (css-at-rule-block node)))
                  #f)]
    [else node]))

;; mergeable-style-rules? : any/c any/c -> boolean?
(define (mergeable-style-rules? a b)
  (and (css-style-rule? a)
       (css-style-rule? b)
       (equal? (css-style-rule-selector-groups a)
               (css-style-rule-selector-groups b))))

;; merge-style-rules : css-style-rule? css-style-rule? -> css-style-rule?
(define (merge-style-rules a b)
  (css-style-rule (css-style-rule-selector-groups a)
                  (append (css-style-rule-block a)
                          (css-style-rule-block b))
                  (css-style-rule-raw-selector a)
                  #f))

;; mergeable-at-rules? : any/c any/c -> boolean?
(define (mergeable-at-rules? a b)
  (and (css-at-rule? a)
       (css-at-rule? b)
       (string-ci=? (css-at-rule-name a) (css-at-rule-name b))
       (equal? (css-at-rule-prelude a) (css-at-rule-prelude b))
       (list? (css-at-rule-block a))
       (list? (css-at-rule-block b))))

;; merge-at-rules : css-at-rule? css-at-rule? -> css-at-rule?
(define (merge-at-rules a b)
  (css-at-rule (css-at-rule-name a)
               (css-at-rule-prelude a)
               (append (css-at-rule-block a)
                       (css-at-rule-block b))
               #f))

;; dedupe-declarations-node-list : list? symbol? -> list?
(define (dedupe-declarations-node-list nodes keep)
  (map (lambda (node)
         (dedupe-declarations-node node keep))
       nodes))

;; dedupe-declarations-node : any/c symbol? -> any/c
(define (dedupe-declarations-node node keep)
  (cond
    [(css-style-rule? node)
     (css-style-rule (css-style-rule-selector-groups node)
                     (dedupe-declarations-block (dedupe-declarations-node-list (css-style-rule-block node) keep) keep)
                     (css-style-rule-raw-selector node)
                     #f)]
    [(css-at-rule? node)
     (css-at-rule (css-at-rule-name node)
                  (css-at-rule-prelude node)
                  (and (list? (css-at-rule-block node))
                       (dedupe-declarations-node-list (css-at-rule-block node) keep))
                  #f)]
    [else node]))

;; dedupe-declarations-block : list? symbol? -> list?
(define (dedupe-declarations-block nodes keep)
  (if (eq? keep 'last)
      (reverse (dedupe-declarations-block/keep-first (reverse nodes)))
      (dedupe-declarations-block/keep-first nodes)))

;; dedupe-declarations-block/keep-first : list? -> list?
;;   Remove duplicate declarations while keeping the first occurrence.
(define (dedupe-declarations-block/keep-first nodes)
  (let loop ([remaining nodes]
             [seen '()]
             [acc '()])
    (cond
      [(null? remaining)
       (reverse acc)]
      [else
       (define node
         (car remaining))
       (cond
         [(css-declaration? node)
          (define key
            (string-downcase (css-declaration-name node)))
          (if (member key seen)
              (loop (cdr remaining) seen acc)
              (loop (cdr remaining) (cons key seen) (cons node acc)))]
         [else
          (loop (cdr remaining) seen (cons node acc))])])))

;; sort-declarations-node-list : list? procedure? -> list?
(define (sort-declarations-node-list nodes less-than)
  (map (lambda (node)
         (sort-declarations-node node less-than))
       nodes))

;; sort-declarations-node : any/c procedure? -> any/c
(define (sort-declarations-node node less-than)
  (cond
    [(css-style-rule? node)
     (css-style-rule (css-style-rule-selector-groups node)
                     (sort-declarations-block (sort-declarations-node-list (css-style-rule-block node) less-than)
                                              less-than)
                     (css-style-rule-raw-selector node)
                     #f)]
    [(css-at-rule? node)
     (css-at-rule (css-at-rule-name node)
                  (css-at-rule-prelude node)
                  (and (list? (css-at-rule-block node))
                       (sort-declarations-node-list (css-at-rule-block node) less-than))
                  #f)]
    [else node]))

;; sort-declarations-block : list? procedure? -> list?
(define (sort-declarations-block nodes less-than)
  (define declarations
    (sort (filter css-declaration? nodes)
          (lambda (a b)
            (less-than (css-declaration-name a) (css-declaration-name b)))))
  (append declarations
          (filter (lambda (node) (not (css-declaration? node))) nodes)))

;; rewrite-var-functions-node-list : list? procedure? -> list?
(define (rewrite-var-functions-node-list nodes proc)
  (map (lambda (node)
         (rewrite-var-functions-node node proc))
       nodes))

;; rewrite-var-functions-node : any/c procedure? -> any/c
(define (rewrite-var-functions-node node proc)
  (cond
    [(css-declaration? node)
     (css-declaration (css-declaration-name node)
                      (rewrite-custom-property-references (css-declaration-value node) proc)
                      (css-declaration-important? node)
                      #f)]
    [(css-style-rule? node)
     (css-style-rule (css-style-rule-selector-groups node)
                     (rewrite-var-functions-node-list (css-style-rule-block node) proc)
                     (css-style-rule-raw-selector node)
                     #f)]
    [(css-at-rule? node)
     (css-at-rule (css-at-rule-name node)
                  (rewrite-custom-property-references (css-at-rule-prelude node) proc)
                  (and (list? (css-at-rule-block node))
                       (rewrite-var-functions-node-list (css-at-rule-block node) proc))
                  #f)]
    [else node]))

;; rename-keyframes-node-list : list? string? string? -> list?
(define (rename-keyframes-node-list nodes old-name new-name)
  (map (lambda (node)
         (rename-keyframes-node node old-name new-name))
       nodes))

;; rename-keyframes-node : any/c string? string? -> any/c
(define (rename-keyframes-node node old-name new-name)
  (cond
    [(css-declaration? node)
     (if (member (string-downcase (css-declaration-name node))
                 '("animation" "animation-name"))
         (css-declaration (css-declaration-name node)
                          (replace-css-identifier (css-declaration-value node) old-name new-name)
                          (css-declaration-important? node)
                          #f)
         node)]
    [(css-style-rule? node)
     (css-style-rule (css-style-rule-selector-groups node)
                     (rename-keyframes-node-list (css-style-rule-block node) old-name new-name)
                     (css-style-rule-raw-selector node)
                     #f)]
    [(css-at-rule? node)
     (css-at-rule (css-at-rule-name node)
                  (if (string-ci=? (css-at-rule-name node) "@keyframes")
                      (replace-css-identifier (css-at-rule-prelude node) old-name new-name)
                      (css-at-rule-prelude node))
                  (and (list? (css-at-rule-block node))
                       (rename-keyframes-node-list (css-at-rule-block node) old-name new-name))
                  #f)]
    [else node]))

;; rewrite-font-face-node-list : list? procedure? -> list?
(define (rewrite-font-face-node-list nodes proc)
  (map (lambda (node)
         (rewrite-font-face-node node proc))
       nodes))

;; rewrite-font-face-node : any/c procedure? -> any/c
(define (rewrite-font-face-node node proc)
  (cond
    [(css-at-rule? node)
     (define rewritten-block
       (and (list? (css-at-rule-block node))
            (rewrite-font-face-node-list (css-at-rule-block node) proc)))
     (if (string-ci=? (css-at-rule-name node) "@font-face")
         (css-at-rule (css-at-rule-name node)
                      (css-at-rule-prelude node)
                      (map (lambda (child)
                             (if (css-declaration? child)
                                 (let ([next (proc child)])
                                   (if next next child))
                                 child))
                           rewritten-block)
                      #f)
         (css-at-rule (css-at-rule-name node)
                      (css-at-rule-prelude node)
                      rewritten-block
                      #f))]
    [(css-style-rule? node)
     (css-style-rule (css-style-rule-selector-groups node)
                     (rewrite-font-face-node-list (css-style-rule-block node) proc)
                     (css-style-rule-raw-selector node)
                     #f)]
    [else node]))

;; rewrite-url-values-node-list : list? procedure? -> list?
(define (rewrite-url-values-node-list nodes proc)
  (map (lambda (node)
         (rewrite-url-values-node node proc))
       nodes))

;; rewrite-url-values-node : any/c procedure? -> any/c
(define (rewrite-url-values-node node proc)
  (cond
    [(css-declaration? node)
     (css-declaration (css-declaration-name node)
                      (rewrite-url-text (css-declaration-value node) proc)
                      (css-declaration-important? node)
                      #f)]
    [(css-style-rule? node)
     (css-style-rule (css-style-rule-selector-groups node)
                     (rewrite-url-values-node-list (css-style-rule-block node) proc)
                     (css-style-rule-raw-selector node)
                     #f)]
    [(css-at-rule? node)
     (css-at-rule (css-at-rule-name node)
                  (rewrite-url-text (css-at-rule-prelude node) proc)
                  (and (list? (css-at-rule-block node))
                       (rewrite-url-values-node-list (css-at-rule-block node) proc))
                  #f)]
    [else node]))

;; filter-comments-node-list : list? procedure? -> list?
(define (filter-comments-node-list nodes pred?)
  (append-map (lambda (node)
                (filter-comments-node node pred?))
              nodes))

;; filter-comments-node : any/c procedure? -> list?
(define (filter-comments-node node pred?)
  (cond
    [(css-comment? node)
     (if (pred? node) (list node) '())]
    [(css-style-rule? node)
     (list (css-style-rule (css-style-rule-selector-groups node)
                           (filter-comments-node-list (css-style-rule-block node) pred?)
                           (css-style-rule-raw-selector node)
                           #f))]
    [(css-at-rule? node)
     (list (css-at-rule (css-at-rule-name node)
                        (css-at-rule-prelude node)
                        (and (list? (css-at-rule-block node))
                             (filter-comments-node-list (css-at-rule-block node) pred?))
                        #f))]
    [else (list node)]))

;; hoist-nested-node-list : list? (or/c #f list?) -> list?
(define (hoist-nested-node-list nodes parent-selectors)
  (append-map (lambda (node)
                (hoist-nested-node node parent-selectors))
              nodes))

;; hoist-nested-node : any/c (or/c #f list?) -> list?
(define (hoist-nested-node node parent-selectors)
  (cond
    [(css-style-rule? node)
     (define current-selectors
       (if parent-selectors
           (combine-selector-groups parent-selectors (css-style-rule-selector-groups node))
           (css-style-rule-selector-groups node)))
     (define-values (plain nested)
       (partition (lambda (child) (not (css-style-rule? child)))
                  (css-style-rule-block node)))
     (append
      (list (css-style-rule current-selectors
                            (hoist-nested-node-list plain #f)
                            (string-join current-selectors ", ")
                            #f))
      (hoist-nested-node-list nested current-selectors))]
    [(css-at-rule? node)
     (list (css-at-rule (css-at-rule-name node)
                        (css-at-rule-prelude node)
                        (and (list? (css-at-rule-block node))
                             (hoist-nested-node-list (css-at-rule-block node) parent-selectors))
                        #f))]
    [else (list node)]))

;; combine-selector-groups : list? list? -> list?
(define (combine-selector-groups parents children)
  (append-map (lambda (parent)
                (map (lambda (child)
                       (string-append (string-trim parent) " " (string-trim child)))
                     children))
              parents))

;; context-set : list? symbol? any/c -> list?
;;   Associate one context key with a value.
(define (context-set context key value)
  (cons (cons key value)
        (filter (lambda (entry)
                  (not (eq? (car entry) key)))
                context)))

;; context-ref : list? symbol? -> any/c
;;   Look up one context key.
(define (context-ref context key)
  (define entry
    (assoc key context))
  (and entry (cdr entry)))

;; context-has-key? : list? symbol? -> boolean?
;;   Determine whether a context key is present.
(define (context-has-key? context key)
  (pair? (assoc key context)))

;; member-ci? : string? list? -> boolean?
;;   Case-insensitive membership for lists of strings.
(define (member-ci? needle haystack)
  (ormap (lambda (item)
           (and (string? item)
                (string-ci=? needle item)))
         haystack))

;; matching-declarations : css-stylesheet? string? -> list?
;;   Collect declarations whose property matches a name case-insensitively.
(define (matching-declarations stylesheet name)
  (collect-matching-declarations (css-stylesheet-rules stylesheet) name))

;; matching-declarations-in-context : css-stylesheet? symbol? string? string? -> list?
;;   Collect declarations that match a property name within a contextual feature scope.
(define (matching-declarations-in-context stylesheet context-key feature-name property-name)
  (collect-matching-declarations-in-context (css-stylesheet-rules stylesheet)
                                            context-key
                                            feature-name
                                            property-name
                                            '()))

;; collect-matching-declarations : list? string? -> list?
;;   Collect matching declarations recursively from a node list.
(define (collect-matching-declarations nodes name)
  (append-map (lambda (node)
                (collect-matching-declarations-in-node node name))
              nodes))

;; collect-matching-declarations-in-context : list? symbol? string? string? list? -> list?
;;   Collect matching declarations recursively from a node list within a contextual feature scope.
(define (collect-matching-declarations-in-context nodes context-key feature-name property-name context)
  (append-map (lambda (node)
                (collect-matching-declarations-in-node/context node
                                                               context-key
                                                               feature-name
                                                               property-name
                                                               context))
              nodes))

;; collect-matching-declarations-in-node : any/c string? -> list?
;;   Collect matching declarations recursively from one node.
(define (collect-matching-declarations-in-node node name)
  (cond
    [(css-declaration? node)
     (if (string-ci=? (css-declaration-name node) name)
         (list node)
         '())]
    [(css-style-rule? node)
     (collect-matching-declarations (css-style-rule-block node) name)]
    [(css-at-rule? node)
     (if (list? (css-at-rule-block node))
         (collect-matching-declarations (css-at-rule-block node) name)
         '())]
    [else
     '()]))

;; collect-matching-declarations-in-node/context : any/c symbol? string? string? list? -> list?
;;   Collect matching declarations recursively from one node within a contextual feature scope.
(define (collect-matching-declarations-in-node/context node context-key feature-name property-name context)
  (cond
    [(css-declaration? node)
     (if (and (context-has-key? context context-key)
              (member-ci? feature-name (context-ref context context-key))
              (string-ci=? (css-declaration-name node) property-name))
         (list node)
         '())]
    [(css-style-rule? node)
     (collect-matching-declarations-in-context (css-style-rule-block node)
                                               context-key
                                               feature-name
                                               property-name
                                               context)]
    [(css-at-rule? node)
     (define next-context
       (extend-context-for-at-rule context node))
     (if (list? (css-at-rule-block node))
         (collect-matching-declarations-in-context (css-at-rule-block node)
                                                   context-key
                                                   feature-name
                                                   property-name
                                                   next-context)
         '())]
    [else
     '()]))

;; source-editable-declarations? : list? -> boolean?
;;   Determine whether declarations have enough span data for source edits.
(define (source-editable-declarations? declarations)
  (and (pair? declarations)
       (andmap declaration-has-editable-span? declarations)))

;; source-editable-rules? : list? -> boolean?
;;   Determine whether style rules have enough span data for source edits.
(define (source-editable-rules? rules)
  (and (pair? rules)
       (andmap style-rule-has-editable-span? rules)))

;; declaration-has-editable-span? : css-declaration? -> boolean?
;;   Determine whether a declaration has valid source span positions.
(define (declaration-has-editable-span? declaration)
  (define span
    (css-declaration-span declaration))
  (and (css-source-span? span)
       (position? (css-source-span-start span))
       (position? (css-source-span-end span))))

;; style-rule-has-editable-span? : css-style-rule? -> boolean?
;;   Determine whether a style rule has valid source span positions.
(define (style-rule-has-editable-span? rule)
  (define span
    (css-style-rule-span rule))
  (and (css-source-span? span)
       (position? (css-source-span-start span))
       (position? (css-source-span-end span))))

;; make-source-edit : css-declaration? string? -> list?
;;   Build one textual source edit from a declaration span.
(define (make-source-edit declaration replacement)
  (define span
    (css-declaration-span declaration))
  (list (position-offset (css-source-span-start span))
        (position-offset (css-source-span-end span))
        replacement))

;; make-rule-append-edit : string? css-style-rule? string? -> list?
;;   Build one source edit that inserts a declaration before a rule's close brace.
(define (make-rule-append-edit source rule declaration-text)
  (define span
    (css-style-rule-span rule))
  (define start-offset
    (position-offset (css-source-span-start span)))
  (define end-offset
    (position-offset (css-source-span-end span)))
  (define close-brace-index
    (max 0 (- end-offset 2)))
  (define rule-text
    (substring source
               (max 0 (sub1 start-offset))
               (max 0 (sub1 end-offset))))
  (define insertion
    (build-rule-append-insertion source rule-text close-brace-index declaration-text))
  (list (add1 close-brace-index)
        (add1 close-brace-index)
        insertion))

;; serialize-new-declaration : string? string? boolean? -> string?
;;   Serialize one appended declaration.
(define (serialize-new-declaration name value important?)
  (string-append name
                 ": "
                 value
                 (if important?
                     " !important;"
                     ";")))

;; serialize-declaration-like : string? css-declaration? string? -> string?
;;   Serialize a declaration replacement matching the original semicolon style.
(define (serialize-declaration-like source declaration replacement-value)
  (string-append (css-declaration-name declaration)
                 ": "
                 replacement-value
                 (if (declaration-span-ended-with-semicolon? source declaration) ";" "")))

;; build-rule-append-insertion : string? string? exact-nonnegative-integer? string? -> string?
;;   Construct insertion text for a source-preserving append before a close brace.
(define (build-rule-append-insertion source rule-text close-brace-index declaration-text)
  (define multiline?
    (regexp-match? #px"\n" rule-text))
  (cond
    [multiline?
     (define line-start
       (find-line-start source close-brace-index))
     (define closing-indent
       (substring source line-start close-brace-index))
     (define inner-indent
       (string-append closing-indent "  "))
     (string-append inner-indent declaration-text "\n")]
    [else
     (string-append " " declaration-text)]))

;; declaration-span-ended-with-semicolon? : string? css-declaration? -> boolean?
;;   Determine whether the original declaration span likely ended with a semicolon.
(define (declaration-span-ended-with-semicolon? source declaration)
  (define span
    (css-declaration-span declaration))
  (and (css-source-span? span)
       (position? (css-source-span-start span))
       (position? (css-source-span-end span))
       (regexp-match? #px";\\s*$"
                      (substring source
                                 (max 0 (sub1 (position-offset (css-source-span-start span))))
                                 (max 0 (sub1 (position-offset (css-source-span-end span))))))))

;; apply-source-edits : string? list? -> string?
;;   Apply edits of the form (list start-offset end-offset replacement) descending by span.
(define (apply-source-edits source edits)
  (define sorted-edits
    (sort edits > #:key car))
  (for/fold ([current source])
            ([edit (in-list sorted-edits)])
    (define start-offset
      (list-ref edit 0))
    (define end-offset
      (list-ref edit 1))
    (define replacement
      (list-ref edit 2))
    (string-append
     (substring current 0 (max 0 (sub1 start-offset)))
     replacement
     (substring current (max 0 (sub1 end-offset))))))

;; find-line-start : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Find the index just after the previous newline before a position.
(define (find-line-start source index)
  (let loop ([i (sub1 index)])
    (cond
      [(< i 0) 0]
      [(char=? (string-ref source i) #\newline)
       (add1 i)]
      [else
       (loop (sub1 i))])))

;; reparse-source : string? -> css-stylesheet?
;;   Parse a rewritten source string back into a stylesheet AST.
(define (reparse-source source)
  (parse-css-port (open-input-string source)))

;; matching-style-rules-by-selector-group : css-stylesheet? string? -> list?
;;   Collect style rules whose selector groups include one exact selector group.
(define (matching-style-rules-by-selector-group stylesheet selector-group)
  (collect-style-rules
   (css-stylesheet-rules stylesheet)
   (lambda (rule)
     (member selector-group (css-style-rule-selector-groups rule)))))

;; matching-style-rules-by-pseudo : css-stylesheet? string? -> list?
;;   Collect style rules whose derived selector structure uses a pseudo.
(define (matching-style-rules-by-pseudo stylesheet pseudo-name)
  (collect-style-rules
   (css-stylesheet-rules stylesheet)
   (lambda (rule)
     (rule-has-pseudo? rule pseudo-name))))

;; matching-style-rules-by-class : css-stylesheet? string? -> list?
;;   Collect style rules whose derived selector structure uses a class selector.
(define (matching-style-rules-by-class stylesheet class-name)
  (collect-style-rules
   (css-stylesheet-rules stylesheet)
   (lambda (rule)
     (rule-has-class? rule class-name))))

;; matching-style-rules-by-attribute : css-stylesheet? string? -> list?
;;   Collect style rules whose derived selector structure uses an attribute selector.
(define (matching-style-rules-by-attribute stylesheet attribute-name)
  (collect-style-rules
   (css-stylesheet-rules stylesheet)
   (lambda (rule)
     (rule-has-attribute? rule attribute-name))))

;; collect-style-rules : list? procedure? -> list?
;;   Collect style rules recursively from a node list.
(define (collect-style-rules nodes pred?)
  (append-map (lambda (node)
                (collect-style-rules-in-node node pred?))
              nodes))

;; collect-style-rules-in-node : any/c procedure? -> list?
;;   Collect style rules recursively from one node.
(define (collect-style-rules-in-node node pred?)
  (cond
    [(css-style-rule? node)
     (if (pred? node) (list node) '())]
    [(css-at-rule? node)
     (if (list? (css-at-rule-block node))
         (collect-style-rules (css-at-rule-block node) pred?)
         '())]
    [else
     '()]))

(module+ test
  (require rackunit
           "css-serialize.rkt")

  (define stylesheet
    (css-stylesheet
     (list (css-style-rule '("body")
                           (list (css-declaration "color" "red" #f #f)
                                 (css-declaration "margin" "0" #f #f))
                           "body"
                           #f)
           (css-at-rule "@media"
                        "screen"
                        (list (css-style-rule '(".card")
                                              (list (css-declaration "color" "blue" #f #f))
                                              ".card"
                                              #f))
                        #f))
     "body { color: red; margin: 0; }"
     #f))

  (define rewritten-values
    (css-update-declaration-values stylesheet "color" string-upcase))
  (check-equal?
   (serialize-stylesheet rewritten-values)
   "body { color: RED; margin: 0; }\n@media screen { .card { color: BLUE; } }")
  (define removed
    (css-remove-declarations stylesheet "margin"))
  (check-equal?
   (serialize-stylesheet removed)
   "body { color: red; }\n@media screen { .card { color: blue; } }")
  (define appended
    (css-append-declaration stylesheet ".card" "padding" "1rem"))
  (check-equal?
   (serialize-stylesheet appended)
   "body { color: red; margin: 0; }\n@media screen { .card { color: blue; padding: 1rem; } }")
  (define sourceful-append
    (parse-css-port
     (open-input-string "body {\n  color : red ;\n}\n/* tail */")))
  (check-equal?
   (serialize-stylesheet
    (css-append-declaration/preserve-source sourceful-append "body" "margin" "0")
    #:preserve-source? #t)
   "body {\n  color : red ;\n  margin: 0;\n}\n/* tail */")
  (define pseudo-appended
    (css-append-declaration-by-pseudo
     (css-stylesheet
      (list (css-style-rule '("a:hover")
                            (list (css-declaration "color" "red" #f #f))
                            "a:hover"
                            #f))
      #f
      #f)
     "hover"
     "text-decoration"
     "underline"))
  (check-equal?
   (serialize-stylesheet pseudo-appended)
   "a:hover { color: red; text-decoration: underline; }")
  (check-equal?
   (serialize-stylesheet
    (css-append-declaration-by-pseudo/preserve-source
     (parse-css-port (open-input-string "a:hover {\n  color: red;\n}"))
     "hover"
     "text-decoration"
     "underline")
    #:preserve-source? #t)
   "a:hover {\n  color: red;\n  text-decoration: underline;\n}")
  (define class-appended
    (css-append-declaration-by-class
     (css-stylesheet
      (list (css-style-rule '(".card")
                            (list (css-declaration "color" "red" #f #f))
                            ".card"
                            #f))
      #f
      #f)
     "card"
     "padding"
     "1rem"))
  (check-equal?
   (serialize-stylesheet class-appended)
   ".card { color: red; padding: 1rem; }")
  (check-equal?
   (serialize-stylesheet
    (css-append-declaration-by-class/preserve-source
     (parse-css-port (open-input-string ".card {\n  color: red;\n}"))
     "card"
     "padding"
     "1rem")
    #:preserve-source? #t)
   ".card {\n  color: red;\n  padding: 1rem;\n}")
  (define attribute-appended
    (css-append-declaration-by-attribute
     (css-stylesheet
      (list (css-style-rule '("a[href]")
                            (list (css-declaration "color" "red" #f #f))
                            "a[href]"
                            #f))
      #f
      #f)
     "href"
     "text-decoration"
     "underline"))
  (check-equal?
   (serialize-stylesheet attribute-appended)
   "a[href] { color: red; text-decoration: underline; }")
  (check-equal?
   (serialize-stylesheet
    (css-append-declaration-by-attribute/preserve-source
     (parse-css-port (open-input-string "a[href] {\n  color: red;\n}"))
     "href"
     "text-decoration"
     "underline")
    #:preserve-source? #t)
   "a[href] {\n  color: red;\n  text-decoration: underline;\n}")
  (define media-updated
    (css-update-declaration-values-in-media-feature stylesheet "screen" "color" string-upcase))
  (check-equal?
   (serialize-stylesheet media-updated)
   "body { color: red; margin: 0; }\n@media screen { .card { color: blue; } }")
  (define media-feature-stylesheet
    (css-stylesheet
     (list (css-at-rule "@media"
                        "screen and (width >= 40rem)"
                        (list (css-style-rule '(".card")
                                              (list (css-declaration "color" "blue" #f #f))
                                              ".card"
                                              #f))
                        #f))
     #f
     #f))
  (check-equal?
   (serialize-stylesheet
    (css-update-declaration-values-in-media-feature media-feature-stylesheet "width" "color" string-upcase))
   "@media screen and (width >= 40rem) { .card { color: BLUE; } }")
  (check-equal?
   (serialize-stylesheet
    (css-update-declaration-values-in-media-feature/preserve-source
     (parse-css-port
      (open-input-string "@media screen and (width >= 40rem) {\n  .card {\n    color : blue ;\n  }\n}"))
     "width"
     "color"
     string-upcase)
    #:preserve-source? #t)
   "@media screen and (width >= 40rem) {\n  .card {\n    color: BLUE;\n  }\n}")
  (check-equal?
   (serialize-stylesheet
    (css-remove-declarations-in-media-feature media-feature-stylesheet "width" "color"))
   "@media screen and (width >= 40rem) { .card {  } }")
  (check-equal?
   (serialize-stylesheet
    (css-remove-declarations-in-media-feature/preserve-source
     (parse-css-port
      (open-input-string "@media screen and (width >= 40rem) {\n  .card {\n    color: blue;\n    margin: 0;\n  }\n}"))
     "width"
     "color")
    #:preserve-source? #t)
   "@media screen and (width >= 40rem) {\n  .card {\n    \n    margin: 0;\n  }\n}")
  (define supports-feature-stylesheet
    (css-stylesheet
     (list (css-at-rule "@supports"
                        "(display: grid)"
                        (list (css-style-rule '(".layout")
                                              (list (css-declaration "display" "grid" #f #f))
                                              ".layout"
                                              #f))
                        #f))
     #f
     #f))
  (check-equal?
   (serialize-stylesheet
    (css-update-declaration-values-in-supports-feature supports-feature-stylesheet "display" "display" string-upcase))
   "@supports (display: grid) { .layout { display: GRID; } }")
  (check-equal?
   (serialize-stylesheet
    (css-update-declaration-values-in-supports-feature/preserve-source
     (parse-css-port
      (open-input-string "@supports (display: grid) {\n  .layout {\n    display : grid ;\n  }\n}"))
     "display"
     "display"
     string-upcase)
    #:preserve-source? #t)
   "@supports (display: grid) {\n  .layout {\n    display: GRID;\n  }\n}")
  (check-equal?
   (serialize-stylesheet
    (css-remove-declarations-in-supports-feature supports-feature-stylesheet "display" "display"))
   "@supports (display: grid) { .layout {  } }")
  (check-equal?
   (serialize-stylesheet
    (css-remove-declarations-in-supports-feature/preserve-source
     (parse-css-port
      (open-input-string "@supports (display: grid) {\n  .layout {\n    display: grid;\n    gap: 1rem;\n  }\n}"))
     "display"
     "display")
    #:preserve-source? #t)
   "@supports (display: grid) {\n  .layout {\n    \n    gap: 1rem;\n  }\n}")
  (define sourceful
    (parse-css-port
     (open-input-string "body {\n  color : red ;\n  margin: 0\n}\n/* tail */")))
  (check-equal?
   (serialize-stylesheet
    (css-update-declaration-values/preserve-source sourceful "color" string-upcase)
    #:preserve-source? #t)
   "body {\n  color: RED;\n  margin: 0\n}\n/* tail */")
  (check-equal?
   (serialize-stylesheet
    (css-remove-declarations/preserve-source sourceful "margin")
    #:preserve-source? #t)
   "body {\n  color : red ;\n  \n}\n/* tail */")
  (check-false (css-stylesheet-source appended)))
