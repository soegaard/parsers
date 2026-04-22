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

(struct css-compute-candidate (name value important? specificity source-order declaration matched-rule)
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
  (values style-hash
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
  (for/first ([group (in-list (css-style-rule-selector-groups rule))]
              [selector (in-list (css-style-rule-selectors rule))]
              #:when (string=? group selector-group))
    (selector-group-specificity selector)))

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
       (values (add1 next-order)
               (cons (css-compute-candidate (normalized-property-name
                                             (css-declaration-name item))
                                            (declaration-computed-value item)
                                            (css-declaration-important? item)
                                            (css-compute-matched-rule-specificity matched-rule)
                                            next-order
                                            item
                                            matched-rule)
                     next-candidates))]
      [else
       (values next-order next-candidates)])))

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

;; selector-group-specificity : css-selector? -> list?
;;   Compute specificity as (list ids classes types).
(define (selector-group-specificity selector)
  (selector-parts-specificity (css-selector-parts selector)))

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
          (css-compute-style-trace-var-resolutions trace))))
