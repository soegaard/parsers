#lang racket/base

;;;
;;; CSS Computed Style Helpers
;;;
;;
;; Narrow exact-target computed-style helpers for stylesheet tooling.

(provide css-compute-style-for-selector-group
         css-compute-custom-properties-for-selector-group
         (struct-out css-compute-style-trace)
         (struct-out css-compute-matched-rule)
         (struct-out css-compute-property-result)
         (struct-out css-compute-candidate)
         (struct-out css-compute-var-resolution))

(require racket/hash
         racket/list
         racket/match
         racket/port
         racket/string
         "css-ast.rkt"
         "css-query.rkt"
         "css-structure.rkt")

(struct css-compute-style-trace (selector-group
                                 matched-rules
                                 property-results
                                 custom-property-results
                                 var-resolutions)
  #:transparent)

(struct css-compute-matched-rule (selector-group specificity source-order rule)
  #:transparent)

(struct css-compute-property-result (name candidates winner)
  #:transparent)

(struct css-compute-candidate (name value important? specificity source-order declaration matched-rule source-name)
  #:transparent)

(struct css-compute-var-resolution (name raw-value resolved-value references cycle?)
  #:transparent)

;; css-compute-style-for-selector-group : css-stylesheet? string? keyword-arguments -> hash?
;;   Compute final declarations for one exact selector-group target.
(define (css-compute-style-for-selector-group stylesheet
                                              selector-group
                                              #:resolve-vars? [resolve-vars? #f]
                                              #:defaults [defaults #f]
                                              #:trace? [trace? #f])
  (validate-compute-inputs 'css-compute-style-for-selector-group
                           stylesheet
                           selector-group
                           defaults
                           resolve-vars?
                           trace?)
  (define-values (style-hash _custom-hash trace)
    (compute-style-data stylesheet selector-group defaults resolve-vars?))
  (if trace?
      (values style-hash trace)
      style-hash))

;; css-compute-custom-properties-for-selector-group : css-stylesheet? string? keyword-arguments -> hash?
;;   Compute the final custom-property environment for one exact selector-group target.
(define (css-compute-custom-properties-for-selector-group stylesheet
                                                          selector-group
                                                          #:defaults [defaults #f]
                                                          #:resolve-vars? [resolve-vars? #f]
                                                          #:trace? [trace? #f])
  (validate-compute-inputs 'css-compute-custom-properties-for-selector-group
                           stylesheet
                           selector-group
                           defaults
                           resolve-vars?
                           trace?)
  (define-values (_style-hash custom-hash trace)
    (compute-style-data stylesheet selector-group defaults resolve-vars?))
  (if trace?
      (values custom-hash trace)
      custom-hash))

;; compute-style-data : css-stylesheet? string? (or/c hash? #f) boolean? -> values
;;   Compute final standard declarations, custom properties, and trace data.
(define (compute-style-data stylesheet selector-group defaults resolve-vars?)
  (define matched-rules
    (matching-rule-candidates-for-selector-group stylesheet selector-group))
  (define declaration-candidates
    (collect-declaration-candidates matched-rules))
  (define custom-candidates
    (filter custom-property-candidate? declaration-candidates))
  (define property-candidates
    (filter standard-property-candidate? declaration-candidates))
  (define custom-results
    (pick-winning-candidates custom-candidates))
  (define property-results
    (pick-winning-candidates property-candidates))
  (define-values (custom-hash custom-resolutions)
    (if resolve-vars?
        (resolve-custom-property-results custom-results defaults)
        (values (property-results->hash custom-results)
                '())))
  (define-values (style-hash style-resolutions)
    (if resolve-vars?
        (resolve-style-property-results property-results custom-hash defaults)
        (values (property-results->hash property-results)
                '())))
  (values (augment-style-hash-with-derived-shorthands style-hash)
          custom-hash
          (css-compute-style-trace selector-group
                                   matched-rules
                                   property-results
                                   custom-results
                                   (append custom-resolutions style-resolutions))))

;; validate-compute-inputs : symbol? any/c any/c any/c any/c any/c -> void?
;;   Validate common computed-style helper arguments.
(define (validate-compute-inputs who stylesheet selector-group defaults resolve-vars? trace?)
  (unless (css-stylesheet? stylesheet)
    (raise-argument-error who "css-stylesheet?" stylesheet))
  (unless (string? selector-group)
    (raise-argument-error who "string?" selector-group))
  (when (and defaults
             (not (hash? defaults)))
    (raise-argument-error who "(or/c hash? #f)" defaults))
  (unless (boolean? resolve-vars?)
    (raise-argument-error who "boolean?" resolve-vars?))
  (unless (boolean? trace?)
    (raise-argument-error who "boolean?" trace?)))

;; matching-rule-candidates-for-selector-group : css-stylesheet? string? -> list?
;;   Collect exact selector-group matches with specificity and source order.
(define (matching-rule-candidates-for-selector-group stylesheet selector-group)
  (for/list ([node (in-list (css-flatten-rules stylesheet))]
             [source-order (in-naturals)]
             #:when (css-style-rule? node)
             #:do [(define specificity
                      (matching-selector-group-specificity node selector-group))]
             #:when specificity)
    (css-compute-matched-rule selector-group
                              specificity
                              source-order
                              node)))

;; matching-selector-group-specificity : css-style-rule? string? -> (or/c list? #f)
;;   Return specificity for one exact selector-group match, if present.
(define (matching-selector-group-specificity rule selector-group)
  (and (member selector-group (css-style-rule-selector-groups rule))
       (selector-group-specificity/text selector-group)))

;; selector-group-specificity/text : string? -> list?
;;   Compute specificity directly from one exact selector-group string.
(define (selector-group-specificity/text selector-group)
  (cond
    [(keyframes-selector-group? selector-group)
     '(0 0 0)]
    [else
     (selector-group-specificity
      (car (css-style-rule-selectors
            (css-style-rule (list selector-group)
                            '()
                            selector-group
                            #f))))]))

;; collect-declaration-candidates : list? -> list?
;;   Collect declaration candidates from matching rules in declaration source order.
(define (collect-declaration-candidates matched-rules)
  (reverse
   (let loop ([remaining-rules matched-rules]
              [source-order 0]
              [candidates '()])
     (cond
       [(null? remaining-rules)
        candidates]
       [else
        (define-values (next-order next-candidates)
          (collect-declaration-candidates-from-block
           (css-compute-matched-rule-rule (car remaining-rules))
           (car remaining-rules)
           source-order
           candidates))
        (loop (cdr remaining-rules) next-order next-candidates)]))))

;; collect-declaration-candidates-from-block : css-style-rule? css-compute-matched-rule? exact-nonnegative-integer? list? -> values
;;   Collect declaration candidates from one matched rule block.
(define (collect-declaration-candidates-from-block rule matched-rule source-order candidates)
  (for/fold ([next-order source-order]
             [next-candidates candidates])
            ([item (in-list (css-style-rule-block rule))])
    (cond
      [(css-declaration? item)
       (define declaration-candidates
         (declaration->compute-candidates item
                                          matched-rule
                                          next-order))
       (values (add1 next-order)
               (append (reverse declaration-candidates)
                       next-candidates))]
      [else
       (values next-order next-candidates)])))

;; declaration->compute-candidates : css-declaration? css-compute-matched-rule? exact-nonnegative-integer? -> list?
;;   Produce authored and shorthand-expanded candidates for one declaration.
(define (declaration->compute-candidates declaration matched-rule source-order)
  (define source-name
    (normalized-property-name (css-declaration-name declaration)))
  (define raw-value
    (declaration-computed-value declaration))
  (define base-candidate
    (make-compute-candidate source-name
                            raw-value
                            source-name
                            declaration
                            matched-rule
                            source-order))
  (append (list base-candidate)
          (expand-shorthand-candidates declaration
                                       matched-rule
                                       source-order
                                       source-name)))

;; make-compute-candidate : string? string? string? css-declaration? css-compute-matched-rule? exact-nonnegative-integer? -> css-compute-candidate?
;;   Build one compute candidate, preserving the authored source property.
(define (make-compute-candidate name
                                value
                                source-name
                                declaration
                                matched-rule
                                source-order)
  (css-compute-candidate name
                         value
                         (css-declaration-important? declaration)
                         (css-compute-matched-rule-specificity matched-rule)
                         source-order
                         declaration
                         matched-rule
                         source-name))

;; expand-shorthand-candidates : css-declaration? css-compute-matched-rule? exact-nonnegative-integer? string? -> list?
;;   Expand the small supported shorthand set into synthetic longhand candidates.
(define (expand-shorthand-candidates declaration
                                     matched-rule
                                     source-order
                                     source-name)
  (case (string->symbol source-name)
    [(border)
     (expand-border-shorthand-candidates declaration
                                         matched-rule
                                         source-order
                                         source-name
                                         #f)]
    [(border-top border-right border-bottom border-left)
     (expand-border-shorthand-candidates declaration
                                         matched-rule
                                         source-order
                                         source-name
                                         (substring source-name
                                                    (string-length "border-")
                                                    (string-length source-name)))]
    [(padding margin)
     (expand-box-shorthand-candidates declaration
                                      matched-rule
                                      source-order
                                      source-name)]
    [else
     '()]))

;; -----------------------------------------------------------------------------
;; Shorthand expansion constants

(define border-sides
  '("top" "right" "bottom" "left"))

(define border-style-keywords
  '("none"
    "hidden"
    "dotted"
    "dashed"
    "solid"
    "double"
    "groove"
    "ridge"
    "inset"
    "outset"))

(define border-width-keywords
  '("thin" "medium" "thick"))

;; expand-border-shorthand-candidates : css-declaration? css-compute-matched-rule? exact-nonnegative-integer? string? (or/c string? #f) -> list?
;;   Expand border and side-specific border shorthands into longhand candidates.
(define (expand-border-shorthand-candidates declaration
                                            matched-rule
                                            source-order
                                            source-name
                                            side-name)
  (define components
    (declaration-computed-components declaration))
  (define-values (width style color)
    (parse-border-shorthand-components components))
  (cond
    [(and (not width) (not style) (not color))
     '()]
    [else
     (define target-sides
       (if side-name
           (list side-name)
           border-sides))
     (append-map (lambda (side)
                   (append (make-border-side-candidates declaration
                                                        matched-rule
                                                        source-order
                                                        source-name
                                                        side
                                                        "width"
                                                        width)
                           (make-border-side-candidates declaration
                                                        matched-rule
                                                        source-order
                                                        source-name
                                                        side
                                                        "style"
                                                        style)
                           (make-border-side-candidates declaration
                                                        matched-rule
                                                        source-order
                                                        source-name
                                                        side
                                                        "color"
                                                        color)))
                 target-sides)]))

;; make-border-side-candidates : css-declaration? css-compute-matched-rule? exact-nonnegative-integer? string? string? string? (or/c string? #f) -> list?
;;   Build zero or one side-specific border longhand candidate.
(define (make-border-side-candidates declaration
                                     matched-rule
                                     source-order
                                     source-name
                                     side
                                     suffix
                                     value)
  (cond
    [value
     (list (make-compute-candidate (string-append "border-" side "-" suffix)
                                   value
                                   source-name
                                   declaration
                                   matched-rule
                                   source-order))]
    [else
     '()]))

;; parse-border-shorthand-components : list? -> values
;;   Parse a reduced border shorthand into width, style, and color texts.
(define (parse-border-shorthand-components components)
  (let loop ([remaining components]
             [width #f]
             [style #f]
             [color #f])
    (cond
      [(null? remaining)
       (values width style color)]
      [else
       (define component
         (car remaining))
       (cond
         [(border-width-component? component)
          (cond
            [width
             (values #f #f #f)]
            [else
             (loop (cdr remaining)
                   (component-value-text component)
                   style
                   color)])]
         [(border-style-component? component)
          (cond
            [style
             (values #f #f #f)]
            [else
             (loop (cdr remaining)
                   width
                   (component-value-text component)
                   color)])]
         [else
          (cond
            [color
             (values #f #f #f)]
            [else
             (loop (cdr remaining)
                   width
                   style
                   (component-value-text component))])])])))

;; expand-box-shorthand-candidates : css-declaration? css-compute-matched-rule? exact-nonnegative-integer? string? -> list?
;;   Expand padding and margin shorthand declarations into four side longhands.
(define (expand-box-shorthand-candidates declaration
                                         matched-rule
                                         source-order
                                         source-name)
  (define component-texts
    (map component-value-text
         (declaration-computed-components declaration)))
  (define expanded-values
    (expand-box-shorthand-values component-texts))
  (cond
    [(not expanded-values)
     '()]
    [else
     (append-map (lambda (side value)
                   (list (make-compute-candidate (string-append source-name "-" side)
                                                 value
                                                 source-name
                                                 declaration
                                                 matched-rule
                                                 source-order)))
                 border-sides
                 expanded-values)]))

;; expand-box-shorthand-values : list? -> (or/c list? #f)
;;   Expand 1/2/3/4-value margin/padding forms to top/right/bottom/left values.
(define (expand-box-shorthand-values component-texts)
  (case (length component-texts)
    [(1)
     (list (car component-texts)
           (car component-texts)
           (car component-texts)
           (car component-texts))]
    [(2)
     (list (first component-texts)
           (second component-texts)
           (first component-texts)
           (second component-texts))]
    [(3)
     (list (first component-texts)
           (second component-texts)
           (third component-texts)
           (second component-texts))]
    [(4)
     component-texts]
    [else
     #f]))

;; pick-winning-candidates : list? -> list?
;;   Group candidates by property name and keep the winning candidate for each.
(define (pick-winning-candidates candidates)
  (define groups
    (for/fold ([grouped (hash)])
              ([candidate (in-list candidates)])
      (hash-update grouped
                   (css-compute-candidate-name candidate)
                   (lambda (existing)
                     (cons candidate existing))
                   '())))
  (for/list ([entry (in-list (sort (hash->list groups) string<? #:key car))])
    (define key
      (car entry))
    (define grouped-candidates
      (reverse (cdr entry)))
    (css-compute-property-result key
                                 grouped-candidates
                                 (pick-winning-candidate grouped-candidates))))

;; pick-winning-candidate : list? -> css-compute-candidate?
;;   Choose the winning declaration candidate by importance, specificity, and order.
(define (pick-winning-candidate candidates)
  (foldl (lambda (candidate current-winner)
           (if (candidate-better? candidate current-winner)
               candidate
               current-winner))
         (car candidates)
         (cdr candidates)))

;; candidate-better? : css-compute-candidate? css-compute-candidate? -> boolean?
;;   Determine whether candidate outranks current-winner.
(define (candidate-better? candidate current-winner)
  (cond
    [(and (css-compute-candidate-important? candidate)
          (not (css-compute-candidate-important? current-winner)))
     #t]
    [(and (not (css-compute-candidate-important? candidate))
          (css-compute-candidate-important? current-winner))
     #f]
    [(specificity>? (css-compute-candidate-specificity candidate)
                    (css-compute-candidate-specificity current-winner))
     #t]
    [(specificity>? (css-compute-candidate-specificity current-winner)
                    (css-compute-candidate-specificity candidate))
     #f]
    [else
     (> (css-compute-candidate-source-order candidate)
        (css-compute-candidate-source-order current-winner))]))

;; resolve-custom-property-results : list? (or/c hash? #f) -> values
;;   Resolve winning custom properties against each other and defaults.
(define (resolve-custom-property-results property-results defaults)
  (define winners
    (for/hash ([result (in-list property-results)])
      (values (css-compute-property-result-name result)
              (css-compute-candidate-value
               (css-compute-property-result-winner result)))))
  (define memo
    (make-hash))
  (define default-memo
    (make-hash))
  ;; resolve-custom-value-text : string? list? -> values
  ;;   Resolve var() references in a custom-property value.
  (define (resolve-custom-value-text text stack)
    (let loop ([start 0]
               [pieces '()]
               [references '()]
               [cycle? #f])
      (define next-var-start
        (find-substring text "var(" start))
      (cond
        [(not next-var-start)
         (values (apply string-append
                        (reverse
                         (cons (substring text start (string-length text))
                               pieces)))
                 (reverse (remove-duplicates references))
                 cycle?)]
        [else
         (define var-end
           (find-matching-close-paren text (+ next-var-start 3)))
         (cond
           [(not var-end)
            (values text
                    (reverse (remove-duplicates references))
                    cycle?)]
           [else
            (define before
              (substring text start next-var-start))
            (define whole-var
              (substring text next-var-start (add1 var-end)))
            (define inner
              (substring text (+ next-var-start 4) var-end))
            (define-values (reference-name fallback)
              (parse-var-inner inner))
            (define-values (replacement next-references next-cycle?)
              (resolve-custom-var-reference whole-var
                                            reference-name
                                            fallback
                                            stack))
            (loop (add1 var-end)
                  (cons replacement (cons before pieces))
                  (append next-references references)
                  (or cycle? next-cycle?))])])))
  ;; resolve-default-value : string? list? -> values
  ;;   Resolve one default custom property value with memoized fallback handling.
  ;; resolve-custom-property : string? string? hash? (or/c hash? #f) hash? list? -> values
  ;;   Resolve one winning custom property through the local recursive resolver.
  (define (resolve-custom-property name raw-value winners defaults memo stack)
    (cond
      [(hash-has-key? memo name)
       (apply values (hash-ref memo name))]
      [(member name stack)
       (values raw-value (list name) #t)]
      [else
       (define-values (resolved-value references cycle?)
         (resolve-custom-value-text raw-value (cons name stack)))
       (define final-value
         (if cycle?
             raw-value
             resolved-value))
       (define final-result
         (list final-value
               (cons name references)
               cycle?))
       (hash-set! memo name final-result)
       (apply values final-result)]))
  ;; resolve-default-value : string? list? -> values
  ;;   Resolve one default custom property value with memoized fallback handling.
  (define (resolve-default-value name stack)
    (cond
      [(hash-has-key? default-memo name)
       (apply values (hash-ref default-memo name))]
      [(member name stack)
       (values (hash-ref defaults name)
               (list name)
               #t)]
      [else
       (define raw-value
         (hash-ref defaults name))
       (define-values (resolved-value references cycle?)
         (resolve-custom-value-text raw-value (cons name stack)))
       (define final-value
         (if cycle?
             raw-value
             resolved-value))
       (define final-result
         (list final-value
               (cons name references)
               cycle?))
       (hash-set! default-memo name final-result)
       (apply values final-result)]))
  ;; resolve-custom-var-reference : string? string? (or/c string? #f) list? -> values
  ;;   Resolve one var() reference in a custom-property value.
  (define (resolve-custom-var-reference whole-var reference-name fallback stack)
    (cond
      [(hash-has-key? winners reference-name)
       (cond
         [(member reference-name stack)
          (values whole-var (list reference-name) #t)]
         [else
          (define-values (resolved-value references cycle?)
            (resolve-custom-property reference-name
                                     (hash-ref winners reference-name)
                                     winners
                                     defaults
                                     memo
                                     stack))
          (values (if cycle? whole-var resolved-value)
                  (cons reference-name references)
                  cycle?)])]
      [(and defaults
            (hash-has-key? defaults reference-name))
       (define-values (resolved-value references cycle?)
         (resolve-default-value reference-name stack))
       (values (if cycle? whole-var resolved-value)
               (cons reference-name references)
               cycle?)]
      [fallback
       (define-values (resolved-fallback references cycle?)
         (resolve-custom-value-text fallback stack))
       (values resolved-fallback references cycle?)]
      [else
       (values whole-var (list reference-name) #f)]))
  (define resolutions '())
  (define resolved-hash
    (for/hash ([result (in-list property-results)])
      (define name
        (css-compute-property-result-name result))
      (define raw-value
        (css-compute-candidate-value
         (css-compute-property-result-winner result)))
      (define-values (resolved-value references cycle?)
        (resolve-custom-property name raw-value winners defaults memo '()))
      (set! resolutions
            (cons (css-compute-var-resolution name
                                              raw-value
                                              resolved-value
                                              references
                                              cycle?)
                  resolutions))
      (values name resolved-value)))
  (values resolved-hash
          (reverse resolutions)))

;; resolve-style-property-results : list? hash? (or/c hash? #f) -> values
;;   Resolve winning standard properties against computed custom properties and defaults.
(define (resolve-style-property-results property-results custom-properties defaults)
  (define resolutions '())
  (define resolved-hash
    (for/hash ([result (in-list property-results)])
      (define winner
        (css-compute-property-result-winner result))
      (define raw-value
        (css-compute-candidate-value winner))
      (define-values (resolved-value references cycle?)
        (resolve-value-text raw-value custom-properties defaults '()))
      (set! resolutions
            (cons (css-compute-var-resolution (css-compute-property-result-name result)
                                              raw-value
                                              resolved-value
                                              references
                                              cycle?)
                  resolutions))
      (values (css-compute-property-result-name result)
              resolved-value)))
  (values resolved-hash
          (reverse resolutions)))

;; resolve-custom-property : string? string? hash? (or/c hash? #f) hash? list? -> values
;;   Resolve one custom property, leaving the raw value unchanged on cycles.
(define (resolve-custom-property name raw-value winners defaults memo stack)
  (cond
    [(hash-has-key? memo name)
     (apply values (hash-ref memo name))]
    [(member name stack)
     (values raw-value (list name) #t)]
    [else
     (define-values (resolved-value references cycle?)
       (resolve-value-text raw-value winners defaults (cons name stack)))
     (define final-value
       (if cycle?
           raw-value
           resolved-value))
     (define final-references
       (cons name references))
     (define final-cycle?
       cycle?)
     (hash-set! memo name (list final-value final-references final-cycle?))
     (values final-value final-references final-cycle?)]))

;; resolve-value-text : string? hash? (or/c hash? #f) list? -> values
;;   Resolve var() references, leaving unresolved forms intact.
(define (resolve-value-text text environment defaults stack)
  (let loop ([start 0]
             [pieces '()]
             [references '()]
             [cycle? #f])
    (define next-var-start
      (find-substring text "var(" start))
    (cond
      [(not next-var-start)
       (values (apply string-append
                      (reverse
                       (cons (substring text start (string-length text))
                             pieces)))
               (reverse (remove-duplicates references))
               cycle?)]
      [else
       (define var-end
         (find-matching-close-paren text (+ next-var-start 3)))
       (cond
         [(not var-end)
          (values text
                  (reverse (remove-duplicates references))
                  cycle?)]
         [else
          (define before
            (substring text start next-var-start))
          (define whole-var
            (substring text next-var-start (add1 var-end)))
          (define inner
            (substring text (+ next-var-start 4) var-end))
          (define-values (reference-name fallback)
            (parse-var-inner inner))
          (define-values (replacement next-references next-cycle?)
            (resolve-var-reference whole-var
                                   reference-name
                                   fallback
                                   environment
                                   defaults
                                   stack))
          (loop (add1 var-end)
                (cons replacement (cons before pieces))
                (append next-references references)
                (or cycle? next-cycle?))])])))

;; resolve-var-reference : string? string? (or/c string? #f) hash? (or/c hash? #f) list? -> values
;;   Resolve one var() reference using computed properties and defaults.
(define (resolve-var-reference whole-var reference-name fallback environment defaults stack)
  (cond
    [(hash-has-key? environment reference-name)
     (cond
       [(member reference-name stack)
        (values whole-var (list reference-name) #t)]
       [else
        (define raw-reference
          (hash-ref environment reference-name))
        (define-values (resolved-value references cycle?)
          (resolve-value-text raw-reference environment defaults (cons reference-name stack)))
        (values (if cycle? whole-var resolved-value)
                (cons reference-name references)
                cycle?)])]
    [(and defaults
          (hash-has-key? defaults reference-name))
     (define default-value
       (hash-ref defaults reference-name))
     (define-values (resolved-value references cycle?)
       (resolve-value-text default-value environment defaults stack))
     (values resolved-value
             (cons reference-name references)
             cycle?)]
    [fallback
     (define-values (resolved-fallback references cycle?)
       (resolve-value-text fallback environment defaults stack))
     (values resolved-fallback references cycle?)]
    [else
     (values whole-var (list reference-name) #f)]))

;; property-results->hash : list? -> hash?
;;   Convert winning property results into a simple name->value hash.
(define (property-results->hash property-results)
  (for/hash ([result (in-list property-results)])
    (values (css-compute-property-result-name result)
            (css-compute-candidate-value
             (css-compute-property-result-winner result)))))

;; augment-style-hash-with-derived-shorthands : hash? -> hash?
;;   Add reduced aggregate border properties when all four sides share one value.
(define (augment-style-hash-with-derived-shorthands style-hash)
  (define with-border-width
    (maybe-add-shared-border-aggregate style-hash
                                       "border-width"
                                       "border-top-width"
                                       "border-right-width"
                                       "border-bottom-width"
                                       "border-left-width"))
  (define with-border-style
    (maybe-add-shared-border-aggregate with-border-width
                                       "border-style"
                                       "border-top-style"
                                       "border-right-style"
                                       "border-bottom-style"
                                       "border-left-style"))
  (maybe-add-shared-border-aggregate with-border-style
                                     "border-color"
                                     "border-top-color"
                                     "border-right-color"
                                     "border-bottom-color"
                                     "border-left-color"))

;; maybe-add-shared-border-aggregate : hash? string? string? string? string? string? -> hash?
;;   Add one aggregate border property when all side values are present and equal.
(define (maybe-add-shared-border-aggregate style-hash aggregate-name top-name right-name bottom-name left-name)
  (cond
    [(and (hash-has-key? style-hash top-name)
          (hash-has-key? style-hash right-name)
          (hash-has-key? style-hash bottom-name)
          (hash-has-key? style-hash left-name))
     (define top-value
       (hash-ref style-hash top-name))
     (define right-value
       (hash-ref style-hash right-name))
     (define bottom-value
       (hash-ref style-hash bottom-name))
     (define left-value
       (hash-ref style-hash left-name))
     (cond
       [(and (string=? top-value right-value)
             (string=? top-value bottom-value)
             (string=? top-value left-value))
        (hash-set style-hash aggregate-name top-value)]
       [else
        style-hash])]
    [else
     style-hash]))

;; custom-property-candidate? : css-compute-candidate? -> boolean?
;;   Determine whether a candidate is a custom property.
(define (custom-property-candidate? candidate)
  (string-prefix? (css-compute-candidate-name candidate) "--"))

;; standard-property-candidate? : css-compute-candidate? -> boolean?
;;   Determine whether a candidate is a standard property.
(define (standard-property-candidate? candidate)
  (not (custom-property-candidate? candidate)))

;; normalized-property-name : string? -> string?
;;   Normalize property names for winner selection and result hashes.
(define (normalized-property-name name)
  (if (string-prefix? name "--")
      name
      (string-downcase name)))

;; declaration-computed-value : css-declaration? -> string?
;;   Remove a trailing !important marker from the raw declaration value.
(define (declaration-computed-value declaration)
  (string-trim
   (regexp-replace #px"(?i:\\s*!important\\s*$)"
                   (css-declaration-value declaration)
                   "")))

;; declaration-computed-components : css-declaration? -> list?
;;   Parse declaration components after trimming a trailing !important marker.
(define (declaration-computed-components declaration)
  (css-declaration-component-values
   (css-declaration (css-declaration-name declaration)
                    (declaration-computed-value declaration)
                    #f
                    (css-declaration-span declaration))))

;; component-value-text : any/c -> string?
;;   Extract preserved source text from one component value node.
(define (component-value-text component)
  (cond
    [(css-component-token? component)
     (css-component-token-text component)]
    [(css-component-an-plus-b? component)
     (css-component-an-plus-b-text component)]
    [(css-component-number? component)
     (css-component-number-text component)]
    [(css-component-percentage? component)
     (css-component-percentage-text component)]
    [(css-component-dimension? component)
     (css-component-dimension-text component)]
    [(css-component-string? component)
     (css-component-string-text component)]
    [(css-component-hash? component)
     (css-component-hash-text component)]
    [(css-component-url? component)
     (css-component-url-text component)]
    [(css-component-function? component)
     (css-component-function-text component)]
    [(css-component-block? component)
     (css-component-block-text component)]
    [else
     ""]))

;; border-width-component? : any/c -> boolean?
;;   Determine whether one component is a reduced border-width value.
(define (border-width-component? component)
  (cond
    [(css-component-dimension? component)
     #t]
    [(css-component-percentage? component)
     #t]
    [(css-component-number? component)
     (zero? (css-component-number-value component))]
    [(css-component-token? component)
     (member (string-downcase (css-component-token-text component))
             border-width-keywords)]
    [else
     #f]))

;; border-style-component? : any/c -> boolean?
;;   Determine whether one component is a reduced border-style value.
(define (border-style-component? component)
  (and (css-component-token? component)
       (member (string-downcase (css-component-token-text component))
               border-style-keywords)))

;; selector-group-specificity : css-selector? -> list?
;;   Compute specificity as (list ids classes types).
(define (selector-group-specificity selector)
  (selector-parts-specificity (css-selector-parts selector)))

;; keyframes-selector-group? : string? -> boolean?
;;   Recognize @keyframes step selectors in the reduced computed-style model.
(define (keyframes-selector-group? selector-group)
  (or (string-ci=? selector-group "from")
      (string-ci=? selector-group "to")
      (regexp-match? #px"^[0-9]+(?:\\.[0-9]+)?%$"
                     (string-trim selector-group))))

;; selector-parts-specificity : list? -> list?
;;   Sum specificity over flat selector parts.
(define (selector-parts-specificity parts)
  (foldl specificity+ '(0 0 0)
         (map selector-part-specificity parts)))

;; selector-part-specificity : any/c -> list?
;;   Compute specificity contribution from one selector part.
(define (selector-part-specificity part)
  (cond
    [(css-selector-id? part)
     '(1 0 0)]
    [(or (css-selector-class? part)
         (css-selector-attribute? part))
     '(0 1 0)]
    [(or (css-selector-type? part)
         (css-selector-namespaced-type? part))
     '(0 0 1)]
    [(or (css-selector-universal? part)
         (css-selector-namespaced-universal? part)
         (css-selector-combinator? part))
     '(0 0 0)]
    [(css-selector-compound? part)
     (foldl specificity+ '(0 0 0)
            (map selector-part-specificity
                 (css-selector-compound-items part)))]
    [(css-selector-pseudo? part)
     (pseudo-specificity part)]
    [else
     '(0 0 0)]))

;; pseudo-specificity : css-selector-pseudo? -> list?
;;   Compute specificity for one pseudo selector.
(define (pseudo-specificity pseudo)
  (define name
    (string-downcase (css-selector-pseudo-name pseudo)))
  (define structure
    (css-selector-pseudo-argument-structure pseudo))
  (cond
    [(css-selector-pseudo-element? pseudo)
     '(0 0 1)]
    [(string=? name "where")
     '(0 0 0)]
    [(member name '("is" "not" "has"))
     (max-selector-list-specificity structure)]
    [(css-selector-pseudo-nth-arguments? structure)
     (specificity+ '(0 1 0)
                   (max-specificity (map selector-group-specificity
                                         (css-selector-pseudo-nth-arguments-selectors structure))))]
    [else
     '(0 1 0)]))

;; max-selector-list-specificity : any/c -> list?
;;   Compute max specificity from a selector-list pseudo wrapper.
(define (max-selector-list-specificity structure)
  (cond
    [(css-selector-pseudo-selector-list? structure)
     (max-specificity (map selector-group-specificity
                           (css-selector-pseudo-selector-list-selectors structure)))]
    [else
     '(0 0 0)]))

;; specificity+ : list? list? -> list?
;;   Add two specificity tuples.
(define (specificity+ a b)
  (map + a b))

;; max-specificity : list? -> list?
;;   Select the highest specificity tuple from tuples.
(define (max-specificity tuples)
  (cond
    [(null? tuples)
     '(0 0 0)]
    [else
     (foldl (lambda (tuple current-max)
              (if (specificity>? tuple current-max)
                  tuple
                  current-max))
            (car tuples)
            (cdr tuples))]))

;; specificity>? : list? list? -> boolean?
;;   Compare specificity tuples lexicographically.
(define (specificity>? a b)
  (cond
    [(> (car a) (car b)) #t]
    [(< (car a) (car b)) #f]
    [(> (cadr a) (cadr b)) #t]
    [(< (cadr a) (cadr b)) #f]
    [else (> (caddr a) (caddr b))]))

;; parse-var-inner : string? -> values
;;   Split a var() argument string into reference name and optional fallback.
(define (parse-var-inner inner)
  (define comma-index
    (find-top-level-comma inner))
  (cond
    [comma-index
     (values (string-trim (substring inner 0 comma-index))
             (string-trim (substring inner (add1 comma-index) (string-length inner))))]
    [else
     (values (string-trim inner) #f)]))

;; find-top-level-comma : string? -> (or/c exact-nonnegative-integer? #f)
;;   Find the first comma outside nested delimiters and strings.
(define (find-top-level-comma text)
  (let loop ([i 0]
             [paren-depth 0]
             [bracket-depth 0]
             [brace-depth 0]
             [quote #f])
    (cond
      [(>= i (string-length text))
       #f]
      [quote
       (cond
         [(char=? (string-ref text i) #\\)
          (loop (+ i 2) paren-depth bracket-depth brace-depth quote)]
         [(char=? (string-ref text i) quote)
          (loop (add1 i) paren-depth bracket-depth brace-depth #f)]
         [else
          (loop (add1 i) paren-depth bracket-depth brace-depth quote)])]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(or (char=? ch #\') (char=? ch #\"))
          (loop (add1 i) paren-depth bracket-depth brace-depth ch)]
         [(char=? ch #\()
          (loop (add1 i) (add1 paren-depth) bracket-depth brace-depth #f)]
         [(char=? ch #\))
          (loop (add1 i) (max 0 (sub1 paren-depth)) bracket-depth brace-depth #f)]
         [(char=? ch #\[)
          (loop (add1 i) paren-depth (add1 bracket-depth) brace-depth #f)]
         [(char=? ch #\])
          (loop (add1 i) paren-depth (max 0 (sub1 bracket-depth)) brace-depth #f)]
         [(char=? ch #\{)
          (loop (add1 i) paren-depth bracket-depth (add1 brace-depth) #f)]
         [(char=? ch #\})
          (loop (add1 i) paren-depth bracket-depth (max 0 (sub1 brace-depth)) #f)]
         [(and (char=? ch #\,)
               (zero? paren-depth)
               (zero? bracket-depth)
               (zero? brace-depth))
          i]
         [else
          (loop (add1 i) paren-depth bracket-depth brace-depth #f)])])))

;; find-substring : string? string? exact-nonnegative-integer? -> (or/c exact-nonnegative-integer? #f)
;;   Find needle in haystack at or after start.
(define (find-substring haystack needle start)
  (define haystack-length
    (string-length haystack))
  (define needle-length
    (string-length needle))
  (let loop ([i start])
    (cond
      [(> (+ i needle-length) haystack-length)
       #f]
      [(string=? (substring haystack i (+ i needle-length))
                 needle)
       i]
      [else
       (loop (add1 i))])))

;; find-matching-close-paren : string? exact-nonnegative-integer? -> (or/c exact-nonnegative-integer? #f)
;;   Find the closing parenthesis for the opening one at open-index.
(define (find-matching-close-paren text open-index)
  (let loop ([i open-index]
             [depth 0]
             [quote #f])
    (cond
      [(>= i (string-length text))
       #f]
      [quote
       (cond
         [(char=? (string-ref text i) #\\)
          (loop (+ i 2) depth quote)]
         [(char=? (string-ref text i) quote)
          (loop (add1 i) depth #f)]
         [else
          (loop (add1 i) depth quote)])]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(or (char=? ch #\') (char=? ch #\"))
          (loop (add1 i) depth ch)]
         [(char=? ch #\()
          (loop (add1 i) (add1 depth) #f)]
         [(char=? ch #\))
          (cond
            [(= depth 1) i]
            [else
             (loop (add1 i) (sub1 depth) #f)])]
         [else
          (loop (add1 i) depth #f)])])))

(module+ test
  (require rackunit
           "css-parser.rkt"
           "css-standard.rkt")

  (define parser
    (make-css-parser #:standard 'current))

  ;; parse-css* : string? -> css-stylesheet?
  ;;   Parse test CSS through the private parser entry point.
  (define (parse-css* text)
    (parser (open-input-string text)))

  (define specificity-stylesheet
    (parse-css* ".btn, #main, div.btn, :where(.x), :not(.x, #y), button::before, li:nth-child(2n+1 of .item, #main) { color: red; }"))
  (define specificity-rule
    (car (css-stylesheet-rules specificity-stylesheet)))
  (define specificity-selectors
    (css-style-rule-selectors specificity-rule))

  (check-equal? (selector-group-specificity (list-ref specificity-selectors 0))
                '(0 1 0))
  (check-equal? (selector-group-specificity (list-ref specificity-selectors 1))
                '(1 0 0))
  (check-equal? (selector-group-specificity (list-ref specificity-selectors 2))
                '(0 1 1))
  (check-equal? (selector-group-specificity (list-ref specificity-selectors 3))
                '(0 0 0))
  (check-equal? (selector-group-specificity (list-ref specificity-selectors 4))
                '(1 0 0))
  (check-equal? (selector-group-specificity (list-ref specificity-selectors 5))
                '(0 0 2))
  (check-equal? (selector-group-specificity (list-ref specificity-selectors 6))
                '(1 1 1))

  (define computed-stylesheet
    (parse-css* (string-append
                 ".btn { color: red; --accent: blue; }\n"
                 ".btn { color: green; --accent: teal; }\n"
                 ".btn { color: orange !important; }\n"
                 ".btn { color: purple; }\n"
                 "@media screen { .btn { background: var(--accent); --accent: gold; } }\n"
                 ".btn { border-color: var(--border, black); }\n"
                 ".btn { --a: var(--b); --b: var(--a); }\n"
                 ".chip { margin: 0; margin: 1rem; }\n"
                 ".chip { opacity: 0.5 !important; opacity: 0.7 !important; }\n"
                 ".chip { --tone: red !important; --tone: blue; --tone: green !important; }\n")))
  (define custom-root-stylesheet
    (parse-css* (string-append
                 ":root { --a: #198754; --b: var(--a); }\n"
                 ":root { --base: #198754; --semantic: var(--base); --semantic-2: var(--semantic); }\n"
                 ":root { --bs-font-sans-serif: \"Nunito Sans\", Arial, sans-serif; --bs-body-font-family: var(--bs-font-sans-serif); }\n"
                 ":root { --bs-success: #198754; --bs-form-valid-color: var(--bs-success); }\n"
                 ":root { --from-default: var(--external-token); }\n"
                 ":root { --cyc-a: var(--cyc-b); --cyc-b: var(--cyc-a); }\n")))
  (define custom-alias-stylesheet
    (parse-css* (string-append
                 ":root { --bs-success: #198754; --bs-form-valid-color: var(--bs-success); }\n"
                 ":root { --bs-semantic: var(--bs-success); --bs-form-valid-color-2: var(--bs-semantic); }\n"
                 ":root { --bs-form-valid-color-3: var(--external); }\n")))

  (check-equal?
   (css-compute-style-for-selector-group computed-stylesheet ".btn")
   (hash "color" "orange"
         "background" "var(--accent)"
         "border-color" "var(--border, black)"))
  (check-equal?
   (css-compute-custom-properties-for-selector-group computed-stylesheet ".btn")
   (hash "--accent" "gold"
         "--a" "var(--b)"
         "--b" "var(--a)"))
  (check-equal?
   (css-compute-style-for-selector-group computed-stylesheet
                                         ".btn"
                                         #:resolve-vars? #t
                                         #:defaults (hash "--border" "gray"))
   (hash "color" "orange"
         "background" "gold"
         "border-color" "gray"))
  (check-equal?
   (css-compute-custom-properties-for-selector-group computed-stylesheet
                                                     ".btn"
                                                     #:resolve-vars? #t)
   (hash "--accent" "gold"
         "--a" "var(--b)"
         "--b" "var(--a)"))
  (check-equal?
   (css-compute-style-for-selector-group computed-stylesheet ".chip")
   (hash "margin" "1rem"
         "margin-top" "1rem"
         "margin-right" "1rem"
         "margin-bottom" "1rem"
         "margin-left" "1rem"
         "opacity" "0.7"))
  (check-equal?
   (css-compute-custom-properties-for-selector-group computed-stylesheet ".chip")
   (hash "--tone" "green"))
  (define-values (computed-hash trace)
    (css-compute-style-for-selector-group computed-stylesheet
                                          ".btn"
                                          #:resolve-vars? #t
                                          #:defaults (hash "--border" "gray")
                                          #:trace? #t))
  (check-equal? (hash-ref computed-hash "background") "gold")
  (check-equal? (length (css-compute-style-trace-matched-rules trace)) 7)
  (check-equal? (css-compute-property-result-name
                 (car (filter (lambda (result)
                                (string=? (css-compute-property-result-name result)
                                          "color"))
                              (css-compute-style-trace-property-results trace))))
                "color")
  (check-true
   (ormap (lambda (resolution)
            (and (string=? (css-compute-var-resolution-name resolution) "--a")
                 (css-compute-var-resolution-cycle? resolution)))
          (css-compute-style-trace-var-resolutions trace)))
  (check-equal?
   (css-compute-custom-properties-for-selector-group custom-root-stylesheet
                                                     ":root"
                                                     #:resolve-vars? #t
                                                     #:defaults (hash "--external-token" "#0d6efd"))
   (hash "--a"            "#198754"
         "--b"            "#198754"
         "--base"         "#198754"
         "--semantic"     "#198754"
         "--semantic-2"   "#198754"
         "--bs-font-sans-serif" "\"Nunito Sans\", Arial, sans-serif"
         "--bs-body-font-family" "\"Nunito Sans\", Arial, sans-serif"
         "--bs-success" "#198754"
         "--bs-form-valid-color" "#198754"
         "--from-default" "#0d6efd"
         "--cyc-a"        "var(--cyc-b)"
         "--cyc-b"        "var(--cyc-a)"))
  (define-values (root-custom-props root-custom-trace)
    (css-compute-custom-properties-for-selector-group custom-root-stylesheet
                                                      ":root"
                                                      #:resolve-vars? #t
                                                      #:defaults (hash "--external-token" "#0d6efd")
                                                      #:trace? #t))
  (check-equal? (hash-ref root-custom-props "--b") "#198754")
  (check-equal? (hash-ref root-custom-props "--semantic-2") "#198754")
  (check-equal? (hash-ref root-custom-props "--bs-body-font-family")
                "\"Nunito Sans\", Arial, sans-serif")
  (check-equal? (hash-ref root-custom-props "--bs-form-valid-color") "#198754")
  (check-equal? (hash-ref root-custom-props "--from-default") "#0d6efd")
  (check-equal? (hash-ref root-custom-props "--cyc-a") "var(--cyc-b)")
  (check-true
   (ormap (lambda (resolution)
            (and (string=? (css-compute-var-resolution-name resolution)
                           "--from-default")
                 (not (not (member "--external-token"
                                   (css-compute-var-resolution-references resolution))))))
          (css-compute-style-trace-var-resolutions root-custom-trace)))
  (check-true
   (ormap (lambda (resolution)
            (and (string=? (css-compute-var-resolution-name resolution)
                           "--cyc-a")
                 (css-compute-var-resolution-cycle? resolution)))
          (css-compute-style-trace-var-resolutions root-custom-trace)))
  (check-equal?
   (css-compute-custom-properties-for-selector-group custom-alias-stylesheet
                                                     ":root")
   (hash "--bs-success"             "#198754"
         "--bs-form-valid-color"    "var(--bs-success)"
         "--bs-semantic"            "var(--bs-success)"
         "--bs-form-valid-color-2"  "var(--bs-semantic)"
         "--bs-form-valid-color-3"  "var(--external)"))
  (check-equal?
   (css-compute-custom-properties-for-selector-group custom-alias-stylesheet
                                                     ":root"
                                                     #:resolve-vars? #t
                                                     #:defaults (hash "--external" "#198754"))
   (hash "--bs-success"             "#198754"
         "--bs-form-valid-color"    "#198754"
         "--bs-semantic"            "#198754"
         "--bs-form-valid-color-2"  "#198754"
         "--bs-form-valid-color-3"  "#198754"))

  (define shorthand-stylesheet
    (parse-css* (string-append
                 ".x { border: 0 solid #e0e1e2; }\n"
                 ".rgba { border: 1px solid rgba(0, 0, 0, 0.2); }\n"
                 ".rgb { border: 1px solid rgb(1, 2, 3); }\n"
                 ".var-border { border: 1px solid var(--bs-border-color); }\n"
                 ".x { border-left-width: 2px; }\n"
                 ".bottom { border-bottom: 3px dashed rgba(10, 20, 30, 0.5); }\n"
                 ".important { border: 0 solid red !important; }\n"
                 ".important { border-left-width: 2px; }\n"
                 ".specific { border: 1px solid blue; }\n"
                 ".specific { border-left-width: 4px; }\n"
                 ".pad1 { padding: 1rem; }\n"
                 ".pad2 { padding: 1rem 2rem; }\n"
                 ".pad3 { padding: 1rem 2rem 3rem; }\n"
                 ".pad4 { padding: 1rem 2rem 3rem 4rem; }\n"
                 ".mar1 { margin: 5px; }\n"
                 ".mar2 { margin: 5px 10px; }\n"
                 ".mar3 { margin: 5px 10px 15px; }\n"
                 ".mar4 { margin: 5px 10px 15px 20px; }\n"
                 ".form-control { border: 0 solid #e0e1e2; }\n"
                 "@media screen { .media-box { border: 1px solid green; } }\n")))

  (check-equal?
   (css-compute-style-for-selector-group shorthand-stylesheet ".x")
   (hash "border"             "0 solid #e0e1e2"
         "border-top-width"   "0"
         "border-right-width" "0"
         "border-bottom-width" "0"
         "border-left-width"  "2px"
         "border-top-style"   "solid"
         "border-right-style" "solid"
         "border-bottom-style" "solid"
         "border-left-style"  "solid"
         "border-top-color"   "#e0e1e2"
         "border-right-color" "#e0e1e2"
         "border-bottom-color" "#e0e1e2"
         "border-left-color"  "#e0e1e2"
         "border-style"       "solid"
         "border-color"       "#e0e1e2"))
  (check-equal?
   (css-compute-style-for-selector-group shorthand-stylesheet ".bottom")
   (hash "border-bottom"       "3px dashed rgba(10, 20, 30, 0.5)"
         "border-bottom-width" "3px"
         "border-bottom-style" "dashed"
         "border-bottom-color" "rgba(10, 20, 30, 0.5)"))
  (check-equal?
   (hash-ref (css-compute-style-for-selector-group shorthand-stylesheet ".rgba")
             "border-top-color")
   "rgba(0, 0, 0, 0.2)")
  (check-equal?
   (hash-ref (css-compute-style-for-selector-group shorthand-stylesheet ".rgba")
             "border-color")
   "rgba(0, 0, 0, 0.2)")
  (check-equal?
   (hash-ref (css-compute-style-for-selector-group shorthand-stylesheet ".rgb")
             "border-top-color")
   "rgb(1, 2, 3)")
  (check-equal?
   (hash-ref (css-compute-style-for-selector-group shorthand-stylesheet ".var-border")
             "border-top-color")
   "var(--bs-border-color)")
  (check-equal?
   (css-compute-style-for-selector-group shorthand-stylesheet ".important")
   (hash "border"              "0 solid red"
         "border-top-width"    "0"
         "border-right-width"  "0"
         "border-bottom-width" "0"
         "border-left-width"   "0"
         "border-top-style"    "solid"
         "border-right-style"  "solid"
         "border-bottom-style" "solid"
         "border-left-style"   "solid"
         "border-top-color"    "red"
         "border-right-color"  "red"
         "border-bottom-color" "red"
         "border-left-color"   "red"
         "border-width"        "0"
         "border-style"        "solid"
         "border-color"        "red"))
  (check-equal?
   (hash-ref (css-compute-style-for-selector-group shorthand-stylesheet ".specific")
             "border-left-width")
   "4px")
  (check-equal?
   (css-compute-style-for-selector-group shorthand-stylesheet ".pad1")
   (hash "padding"        "1rem"
         "padding-top"    "1rem"
         "padding-right"  "1rem"
         "padding-bottom" "1rem"
         "padding-left"   "1rem"))
  (check-equal?
   (css-compute-style-for-selector-group shorthand-stylesheet ".pad2")
   (hash "padding"        "1rem 2rem"
         "padding-top"    "1rem"
         "padding-right"  "2rem"
         "padding-bottom" "1rem"
         "padding-left"   "2rem"))
  (check-equal?
   (css-compute-style-for-selector-group shorthand-stylesheet ".pad3")
   (hash "padding"        "1rem 2rem 3rem"
         "padding-top"    "1rem"
         "padding-right"  "2rem"
         "padding-bottom" "3rem"
         "padding-left"   "2rem"))
  (check-equal?
   (css-compute-style-for-selector-group shorthand-stylesheet ".pad4")
   (hash "padding"        "1rem 2rem 3rem 4rem"
         "padding-top"    "1rem"
         "padding-right"  "2rem"
         "padding-bottom" "3rem"
         "padding-left"   "4rem"))
  (check-equal?
   (css-compute-style-for-selector-group shorthand-stylesheet ".mar1")
   (hash "margin"        "5px"
         "margin-top"    "5px"
         "margin-right"  "5px"
         "margin-bottom" "5px"
         "margin-left"   "5px"))
  (check-equal?
   (css-compute-style-for-selector-group shorthand-stylesheet ".mar2")
   (hash "margin"        "5px 10px"
         "margin-top"    "5px"
         "margin-right"  "10px"
         "margin-bottom" "5px"
         "margin-left"   "10px"))
  (check-equal?
   (css-compute-style-for-selector-group shorthand-stylesheet ".mar3")
   (hash "margin"        "5px 10px 15px"
         "margin-top"    "5px"
         "margin-right"  "10px"
         "margin-bottom" "15px"
         "margin-left"   "10px"))
  (check-equal?
   (css-compute-style-for-selector-group shorthand-stylesheet ".mar4")
   (hash "margin"        "5px 10px 15px 20px"
         "margin-top"    "5px"
         "margin-right"  "10px"
         "margin-bottom" "15px"
         "margin-left"   "20px"))
  (check-equal?
   (hash-ref (css-compute-style-for-selector-group shorthand-stylesheet ".form-control")
             "border-width")
   "0")
  (check-equal?
   (hash-ref (css-compute-style-for-selector-group shorthand-stylesheet ".media-box")
             "border-top-width")
   "1px")
  (define-values (shorthand-computed shorthand-trace)
    (css-compute-style-for-selector-group shorthand-stylesheet
                                          ".x"
                                          #:trace? #t))
  (check-equal? (hash-ref shorthand-computed "border-left-width") "2px")
  (check-true
   (ormap (lambda (result)
            (and (string=? (css-compute-property-result-name result)
                           "border-top-width")
                 (string=? (css-compute-candidate-source-name
                            (css-compute-property-result-winner result))
                           "border")))
          (css-compute-style-trace-property-results shorthand-trace))))
