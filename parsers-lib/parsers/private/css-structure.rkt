#lang racket/base

;;;
;;; CSS Structured Views
;;;
;;
;; Derive selector and component-value structure from preserved raw text.

(provide css-style-rule-selectors
         css-selector-compounds
         css-selector-attribute-derived-details
         css-at-rule-prelude-values
         css-at-rule-prelude-derived-details
         css-declaration-component-values)

(require racket/list
         racket/string
         "css-ast.rkt")

;; css-style-rule-selectors : css-style-rule? -> list?
;;   Derive selector nodes from a style rule's selector groups.
(define (css-style-rule-selectors rule)
  (unless (css-style-rule? rule)
    (raise-argument-error 'css-style-rule-selectors "css-style-rule?" rule))
  (map (lambda (text)
         (css-selector text #f (parse-selector-parts text)))
       (css-style-rule-selector-groups rule)))

;; css-selector-compounds : css-selector? -> list?
;;   Group flat selector parts into compound selectors.
(define (css-selector-compounds selector)
  (unless (css-selector? selector)
    (raise-argument-error 'css-selector-compounds "css-selector?" selector))
  (selector-parts->compounds (css-selector-parts selector)))

;; css-selector-attribute-derived-details : css-selector-attribute? -> css-selector-attribute-details?
;;   Derive a structured view of an attribute selector.
(define (css-selector-attribute-derived-details attribute)
  (unless (css-selector-attribute? attribute)
    (raise-argument-error 'css-selector-attribute-derived-details "css-selector-attribute?" attribute))
  (define-values (namespace name)
    (split-attribute-name (css-selector-attribute-name attribute)))
  (css-selector-attribute-details namespace
                                  name
                                  (css-selector-attribute-matcher attribute)
                                  (parse-attribute-value (css-selector-attribute-value attribute))
                                  (normalize-attribute-modifier
                                   (css-selector-attribute-modifier attribute))
                                  (css-selector-attribute-text attribute)
                                  (css-selector-attribute-span attribute)))

;; css-at-rule-prelude-values : css-at-rule? -> list?
;;   Derive component values from an at-rule prelude.
(define (css-at-rule-prelude-values rule)
  (unless (css-at-rule? rule)
    (raise-argument-error 'css-at-rule-prelude-values "css-at-rule?" rule))
  (parse-component-values (css-at-rule-prelude rule)))

;; css-at-rule-prelude-derived-details : css-at-rule? -> any/c
;;   Derive a structured view of an at-rule prelude when recognized.
(define (css-at-rule-prelude-derived-details rule)
  (unless (css-at-rule? rule)
    (raise-argument-error 'css-at-rule-prelude-derived-details "css-at-rule?" rule))
  (case (string->symbol (string-downcase (css-at-rule-name rule)))
    [(@media)
     (parse-media-prelude (css-at-rule-prelude rule))]
    [(@supports)
     (parse-supports-prelude (css-at-rule-prelude rule))]
    [else
     (css-at-rule-prelude-values rule)]))

;; css-declaration-component-values : css-declaration? -> list?
;;   Derive component values from a declaration value.
(define (css-declaration-component-values declaration)
  (unless (css-declaration? declaration)
    (raise-argument-error 'css-declaration-component-values "css-declaration?" declaration))
  (parse-component-values (css-declaration-value declaration)))

;; parse-media-prelude : string? -> css-media-prelude-details?
;;   Parse a media prelude into query records.
(define (parse-media-prelude text)
  (css-media-prelude-details
   (map parse-media-query (split-top-level-commas text))
   text
   #f))

;; parse-media-query : string? -> css-media-query?
;;   Parse one media query into modifier, media type, and features.
(define (parse-media-query text)
  (define query-text
    (string-trim text))
  (define segments
    (split-top-level-keyword query-text "and"))
  (define head
    (if (pair? segments) (car segments) query-text))
  (define features
    (map (lambda (segment)
           (parse-media-feature segment))
         (filter non-empty-string?
                 (if (pair? segments) (cdr segments) '()))))
  (define head-parts
    (filter non-empty-string? (string-split head)))
  (define modifier
    (and (pair? head-parts)
         (member (string-downcase (car head-parts)) '("not" "only"))
         (string-downcase (car head-parts))))
  (define media-type
    (cond
      [(and modifier (pair? (cdr head-parts)))
       (cadr head-parts)]
      [(and (pair? head-parts)
            (not (string-prefix? (car head-parts) "(")))
       (car head-parts)]
      [else
       #f]))
  (define head-feature?
    (and (pair? head-parts)
         (string-prefix? (car head-parts) "(")))
  (css-media-query modifier
                   media-type
                   (if head-feature?
                       (cons (parse-media-feature head) features)
                       features)
                   query-text
                   #f))

;; parse-media-feature : string? -> any/c
;;   Parse a media feature into a typed expression when recognized.
(define (parse-media-feature text)
  (define trimmed
    (string-trim text))
  (define inner
    (strip-outer-parens-if-balanced trimmed))
  (define range-m
    (regexp-match #px"^(.+?)\\s*(<=|<)\\s*([A-Za-z_-][A-Za-z0-9_-]*)\\s*(<=|<)\\s*(.+)$" inner))
  (define reverse-m
    (regexp-match #px"^(.+?)\\s*(<=|>=|<|>)\\s*([A-Za-z_-][A-Za-z0-9_-]*)$" inner))
  (define direct-m
    (regexp-match #px"^([A-Za-z_-][A-Za-z0-9_-]*)\\s*(<=|>=|=|<|>|:)\\s*(.+)$" inner))
  (cond
    [range-m
     (css-media-feature-range (string-trim (list-ref range-m 1))
                              (list-ref range-m 2)
                              (list-ref range-m 3)
                              (list-ref range-m 4)
                              (string-trim (list-ref range-m 5))
                              trimmed
                              #f)]
    [direct-m
     (define operator
       (list-ref direct-m 2))
     (css-media-feature-expression (string-trim (list-ref direct-m 1))
                                   operator
                                   (string-trim (list-ref direct-m 3))
                                   trimmed
                                   #f)]
    [reverse-m
     (define raw-operator
       (list-ref reverse-m 2))
     (css-media-feature-expression (list-ref reverse-m 3)
                                   (flip-range-operator raw-operator)
                                   (string-trim (list-ref reverse-m 1))
                                   trimmed
                                   #f)]
    [else
     (css-media-feature trimmed #f)]))

;; flip-range-operator : string? -> string?
;;   Reverse a one-sided range operator when the value appears on the left.
(define (flip-range-operator operator)
  (cond
    [(string=? operator "<") ">"]
    [(string=? operator "<=") ">="]
    [(string=? operator ">") "<"]
    [(string=? operator ">=") "<="]
    [else operator]))

;; parse-supports-prelude : string? -> css-supports-prelude-details?
;;   Parse a supports prelude into derived conditions.
(define (parse-supports-prelude text)
  (css-supports-prelude-details
   (list (parse-supports-condition text))
   text
   #f))

;; parse-supports-condition : string? -> css-supports-condition?
;;   Parse one supports condition recursively.
(define (parse-supports-condition text)
  (define condition-text
    (string-trim text))
  (define or-parts
    (split-top-level-keyword condition-text "or"))
  (cond
    [(> (length or-parts) 1)
     (css-supports-condition 'or
                             condition-text
                             (map parse-supports-condition or-parts)
                             #f)]
    [else
     (define and-parts
       (split-top-level-keyword condition-text "and"))
     (cond
       [(> (length and-parts) 1)
        (css-supports-condition 'and
                                condition-text
                                (map parse-supports-condition and-parts)
                                #f)]
       [(regexp-match? #px"^(?i:not)\\s+" condition-text)
        (css-supports-condition 'not
                                condition-text
                                (list (parse-supports-condition
                                       (string-trim
                                        (regexp-replace #px"^(?i:not)\\s+" condition-text ""))))
                                #f)]
       [else
        (define without-parens
          (strip-outer-parens-if-balanced condition-text))
        (cond
          [(and (not (equal? without-parens condition-text))
                (regexp-match? #px":" condition-text))
           (css-supports-condition 'feature
                                   condition-text
                                   (parse-supports-feature-tests condition-text)
                                   #f)]
          [(not (equal? without-parens condition-text))
           (parse-supports-condition without-parens)]
          [(and (string-prefix? condition-text "(")
                (regexp-match? #px":" condition-text))
           (css-supports-condition 'feature
                                   condition-text
                                   (parse-supports-feature-tests condition-text)
                                   #f)]
          [else
           (css-supports-condition 'unknown condition-text '() #f)])])]))

;; parse-supports-feature-tests : string? -> list?
;;   Parse one parenthesized supports feature test into typed nodes when possible.
(define (parse-supports-feature-tests text)
  (define trimmed
    (string-trim text))
  (define inner
    (strip-outer-parens-if-balanced trimmed))
  (define colon-pos
    (find-char-index inner #\:))
  (cond
    [colon-pos
     (list (css-supports-feature (string-trim (substring inner 0 colon-pos))
                                 (string-trim (substring inner (add1 colon-pos) (string-length inner)))
                                 trimmed
                                 #f))]
    [else
     '()]))

;; parse-component-values : string? -> list?
;;   Parse a raw CSS value/prelude into component-value AST nodes.
(define (parse-component-values text)
  (define len
    (string-length text))
  (let loop ([i 0]
             [values '()])
    (cond
      [(>= i len)
       (reverse values)]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(char-whitespace? ch)
          (loop (add1 i) values)]
         [(delimiter-open? ch)
          (define-values (next-i block)
            (parse-simple-block text i))
          (loop next-i (cons block values))]
         [(string-delimiter? ch)
          (define next-i
            (scan-string text i))
          (loop next-i
                (cons (css-component-string (substring text i next-i)
                                            (unquote-css-string (substring text i next-i))
                                            #f)
                      values))]
         [(char=? ch #\#)
          (define next-i
            (scan-hash text i))
          (loop next-i
                (cons (css-component-hash (substring text i next-i)
                                          (substring text (add1 i) next-i)
                                          #f)
                      values))]
         [(number-start-char? text i)
          (define-values (next-i node)
            (parse-numeric text i))
          (loop next-i (cons node values))]
         [(ident-start-char? ch)
          (define ident-end
            (scan-ident text i))
          (cond
            [(and (< ident-end len)
                  (char=? (string-ref text ident-end) #\())
             (define-values (next-i node)
               (parse-function-or-url text i ident-end))
             (loop next-i (cons node values))]
            [else
             (define token-text
               (substring text i ident-end))
             (loop ident-end
                   (cons (css-component-token token-text #f) values))])]
         [else
          (define next-i
            (scan-token text i))
          (loop next-i
                (cons (css-component-token (substring text i next-i) #f)
                      values))])])))

;; parse-function-or-url : string? exact-nonnegative-integer? exact-nonnegative-integer? -> values
;;   Parse a function token or a url() value.
(define (parse-function-or-url text start name-end)
  (define name
    (substring text start name-end))
  (define-values (end-index raw-text)
    (scan-balanced text name-end #\( #\)))
  (define inner-text
    (substring text (add1 name-end) (max (add1 name-end) (sub1 end-index))))
  (cond
    [(string-ci=? name "url")
     (values end-index
             (css-component-url raw-text
                                (string-trim inner-text)
                                #f))]
    [else
     (values end-index
             (css-component-function name
                                     (parse-component-values inner-text)
                                     raw-text
                                     #f))]))

;; parse-simple-block : string? exact-nonnegative-integer? -> values
;;   Parse a simple block and its nested component values.
(define (parse-simple-block text start)
  (define ch
    (string-ref text start))
  (define close-ch
    (matching-delimiter ch))
  (define-values (end-index raw-text)
    (scan-balanced text start ch close-ch))
  (values end-index
          (css-component-block ch
                               (parse-component-values
                                (substring text (add1 start) (max (add1 start) (sub1 end-index))))
                               raw-text
                               #f)))

;; parse-numeric : string? exact-nonnegative-integer? -> values
;;   Parse a number, percentage, or dimension.
(define (parse-numeric text start)
  (define number-end
    (scan-number text start))
  (define number-text
    (substring text start number-end))
  (define value
    (string->number number-text))
  (define len
    (string-length text))
  (cond
    [(and (< number-end len)
          (char=? (string-ref text number-end) #\%))
     (values (add1 number-end)
             (css-component-percentage (substring text start (add1 number-end))
                                       value
                                       #f))]
    [(and (< number-end len)
          (ident-start-char? (string-ref text number-end)))
     (define unit-end
       (scan-ident text number-end))
     (values unit-end
             (css-component-dimension (substring text start unit-end)
                                      value
                                      (substring text number-end unit-end)
                                      #f))]
    [else
     (values number-end
             (css-component-number number-text value #f))]))

;; scan-balanced : string? exact-nonnegative-integer? char? char? -> values
;;   Scan a balanced delimited span, tolerating EOF by consuming to end.
(define (scan-balanced text start open-ch close-ch)
  (define len
    (string-length text))
  (let loop ([i start]
             [depth 0])
    (cond
      [(>= i len)
       (values len (substring text start len))]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(string-delimiter? ch)
          (loop (scan-string text i) depth)]
         [(char=? ch open-ch)
          (loop (add1 i) (add1 depth))]
         [(char=? ch close-ch)
          (define next-depth
            (sub1 depth))
          (cond
            [(zero? next-depth)
             (values (add1 i) (substring text start (add1 i)))]
            [else
             (loop (add1 i) next-depth)])]
         [else
          (loop (add1 i) depth)])])))

;; scan-ident : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Scan an identifier-like sequence.
(define (scan-ident text start)
  (define len
    (string-length text))
  (let loop ([i start])
    (cond
      [(>= i len) i]
      [(ident-continue-char? (string-ref text i))
       (loop (add1 i))]
      [else i])))

;; scan-string : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Scan a quoted string token.
(define (scan-string text start)
  (define len
    (string-length text))
  (define delim
    (string-ref text start))
  (let loop ([i (add1 start)])
    (cond
      [(>= i len) len]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(char=? ch #\\)
          (loop (min len (+ i 2)))]
         [(char=? ch delim)
          (add1 i)]
         [else
          (loop (add1 i))])])))

;; scan-number : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Scan a CSS numeric prefix.
(define (scan-number text start)
  (define len
    (string-length text))
  (define i0
    (cond
      [(and (< start len)
            (or (char=? (string-ref text start) #\+)
                (char=? (string-ref text start) #\-)))
       (add1 start)]
      [else
       start]))
  (define i1
    (scan-digits text i0))
  (cond
    [(and (< i1 len)
          (char=? (string-ref text i1) #\.))
     (scan-digits text (add1 i1))]
    [(and (= i1 i0)
          (< i1 len)
          (char=? (string-ref text i1) #\.))
     (scan-digits text (add1 i1))]
    [else
     i1]))

;; scan-digits : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Scan consecutive digits.
(define (scan-digits text start)
  (define len
    (string-length text))
  (let loop ([i start])
    (cond
      [(>= i len) i]
      [(char-numeric? (string-ref text i))
       (loop (add1 i))]
      [else i])))

;; scan-hash : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Scan a hash token.
(define (scan-hash text start)
  (scan-ident text (add1 start)))

;; scan-token : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Scan a non-whitespace token chunk until a structural boundary.
(define (scan-token text start)
  (define len
    (string-length text))
  (let loop ([i start])
    (cond
      [(>= i len) i]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(or (char-whitespace? ch)
              (delimiter-open? ch)
              (delimiter-close? ch))
          (max (add1 start) i)]
         [else
          (loop (add1 i))])])))

;; unquote-css-string : string? -> string?
;;   Remove the outer quotes from a scanned string token.
(define (unquote-css-string text)
  (cond
    [(>= (string-length text) 2)
     (substring text 1 (sub1 (string-length text)))]
    [else
     text]))

;; number-start-char? : string? exact-nonnegative-integer? -> boolean?
;;   Determine whether a position can start a numeric token.
(define (number-start-char? text i)
  (define len
    (string-length text))
  (and (< i len)
       (let ([ch (string-ref text i)])
         (or (char-numeric? ch)
             (and (or (char=? ch #\+) (char=? ch #\-))
                  (< (add1 i) len)
                  (or (char-numeric? (string-ref text (add1 i)))
                      (char=? (string-ref text (add1 i)) #\.)))
             (and (char=? ch #\.)
                  (< (add1 i) len)
                  (char-numeric? (string-ref text (add1 i))))))))

;; ident-start-char? : char? -> boolean?
;;   Determine whether a char starts an identifier-like token.
(define (ident-start-char? ch)
  (or (char-alphabetic? ch)
      (char=? ch #\-)
      (char=? ch #\_)
      (char=? ch #\@)))

;; ident-continue-char? : char? -> boolean?
;;   Determine whether a char continues an identifier-like token.
(define (ident-continue-char? ch)
  (or (ident-start-char? ch)
      (char-numeric? ch)
      (char=? ch #\%)
      (char=? ch #\.)
      (char=? ch #\:)
      (char=? ch #\|)
      (char=? ch #\#)
      (char=? ch #\*)
      (char=? ch #\/)))

;; string-delimiter? : char? -> boolean?
;;   Determine whether a char begins a quoted string.
(define (string-delimiter? ch)
  (or (char=? ch #\")
      (char=? ch #\')))

;; delimiter-open? : char? -> boolean?
;;   Determine whether a char opens a simple block.
(define (delimiter-open? ch)
  (or (char=? ch #\()
      (char=? ch #\[)
      (char=? ch #\{)))

;; delimiter-close? : char? -> boolean?
;;   Determine whether a char closes a simple block.
(define (delimiter-close? ch)
  (or (char=? ch #\))
      (char=? ch #\])
      (char=? ch #\})))

;; matching-delimiter : char? -> char?
;;   Return the matching closing delimiter.
(define (matching-delimiter ch)
  (case ch
    [(#\() #\)]
    [(#\[) #\]]
    [(#\{) #\}]
    [else  ch]))

(module+ test
  (require rackunit)

  (define declaration
    (css-declaration "background" "rgb(1 2 3) / cover" #f #f))
  (define numeric-declaration
    (css-declaration "padding" "10px 50% 2" #f #f))
  (define url-declaration
    (css-declaration "background-image" "url(\"x.png\") #fff" #f #f))
  (define string-declaration
    (css-declaration "content" "\"hi\"" #f #f))
  (define media-at-rule
    (css-at-rule "@media" "not screen and (width >= 40rem) and (20rem <= width <= 60rem) and (color), print" '() #f))
  (define at-rule
    (css-at-rule "@supports" "(display: grid) and (color: red)" '() #f))
  (define style-rule
    (css-style-rule '("div#main > a[href^=\"https\"]:hover" ".b")
                    '()
                    "div#main > a[href^=\"https\"]:hover, .b"
                    #f))
  (define namespaced-style-rule
    (css-style-rule '("svg|rect.foo *|a" "foo|*")
                    '()
                    "svg|rect.foo *|a, foo|*"
                    #f))
  (define pseudo-style-rule
    (css-style-rule '("a:not(.x, #y) > span:nth-child(2n+1)")
                    '()
                    "a:not(.x, #y) > span:nth-child(2n+1)"
                    #f))
  (define odd-pseudo-style-rule
    (css-style-rule '("li:nth-child(odd)")
                    '()
                    "li:nth-child(odd)"
                    #f))
  (define nth-of-pseudo-style-rule
    (css-style-rule '("li:nth-child(2n+1 of .item, #main)")
                    '()
                    "li:nth-child(2n+1 of .item, #main)"
                    #f))
  (define identifier-pseudo-style-rule
    (css-style-rule '("html:lang(en-US, da):dir(rtl)")
                    '()
                    "html:lang(en-US, da):dir(rtl)"
                    #f))

  (define selectors
    (css-style-rule-selectors style-rule))
  (check-equal? (map css-selector-text selectors)
                '("div#main > a[href^=\"https\"]:hover" ".b"))
  (define first-selector-parts
    (css-selector-parts (car selectors)))
  (check-true (css-selector-type? (car first-selector-parts)))
  (check-true (css-selector-id? (cadr first-selector-parts)))
  (check-true (css-selector-combinator? (caddr first-selector-parts)))
  (check-true (css-selector-type? (cadddr first-selector-parts)))
  (check-true (css-selector-attribute? (list-ref first-selector-parts 4)))
  (check-true (css-selector-pseudo? (list-ref first-selector-parts 5)))
  (define first-attribute-details
    (css-selector-attribute-derived-details (list-ref first-selector-parts 4)))
  (check-equal? (css-selector-attribute-details-name first-attribute-details) "href")
  (check-equal? (css-selector-attribute-details-matcher first-attribute-details) "^=")
  (check-true
   (css-selector-attribute-string-value?
    (css-selector-attribute-details-value first-attribute-details)))
  (check-equal? (length (css-selector-compounds (car selectors))) 3)
  (define namespaced-selectors
    (css-style-rule-selectors namespaced-style-rule))
  (define first-namespaced-parts
    (css-selector-parts (car namespaced-selectors)))
  (check-true (css-selector-namespaced-type? (car first-namespaced-parts)))
  (check-true (css-selector-class? (cadr first-namespaced-parts)))
  (check-true (css-selector-combinator? (caddr first-namespaced-parts)))
  (check-true (css-selector-namespaced-type? (cadddr first-namespaced-parts)))
  (check-true (css-selector-namespaced-universal?
               (car (css-selector-parts (cadr namespaced-selectors)))))
  (define pseudo-selector
    (car (css-style-rule-selectors pseudo-style-rule)))
  (define odd-pseudo-selector
    (car (css-style-rule-selectors odd-pseudo-style-rule)))
  (define nth-of-pseudo-selector
    (car (css-style-rule-selectors nth-of-pseudo-style-rule)))
  (define identifier-pseudo-selector
    (car (css-style-rule-selectors identifier-pseudo-style-rule)))
  (define pseudo-parts
    (css-selector-parts pseudo-selector))
  (define odd-pseudo-parts
    (css-selector-parts odd-pseudo-selector))
  (define nth-of-pseudo-parts
    (css-selector-parts nth-of-pseudo-selector))
  (define identifier-pseudo-parts
    (css-selector-parts identifier-pseudo-selector))
  (define not-pseudo
    (cadr pseudo-parts))
  (define nth-pseudo
    (list-ref pseudo-parts 4))
  (define odd-nth-pseudo
    (cadr odd-pseudo-parts))
  (define nth-of-pseudo
    (cadr nth-of-pseudo-parts))
  (define lang-pseudo
    (cadr identifier-pseudo-parts))
  (define dir-pseudo
    (caddr identifier-pseudo-parts))
  (check-true (css-selector-pseudo? not-pseudo))
  (check-true
   (css-selector-pseudo-selector-list?
    (css-selector-pseudo-argument-structure not-pseudo)))
  (check-true (css-selector? (car (css-selector-pseudo-arguments not-pseudo))))
  (check-equal? (map css-selector-text (css-selector-pseudo-arguments not-pseudo))
                '(".x" "#y"))
  (check-true (css-selector-pseudo? nth-pseudo))
  (check-true
   (css-selector-pseudo-nth-arguments?
    (css-selector-pseudo-argument-structure nth-pseudo)))
  (check-true (css-component-an-plus-b? (car (css-selector-pseudo-arguments nth-pseudo))))
  (check-equal? (css-component-an-plus-b-a (car (css-selector-pseudo-arguments nth-pseudo))) 2)
  (check-equal? (css-component-an-plus-b-b (car (css-selector-pseudo-arguments nth-pseudo))) 1)
  (check-equal?
   (css-selector-pseudo-nth-arguments-selectors
    (css-selector-pseudo-argument-structure nth-pseudo))
   '())
  (check-equal? (css-component-an-plus-b-a (car (css-selector-pseudo-arguments odd-nth-pseudo))) 2)
  (check-equal? (css-component-an-plus-b-b (car (css-selector-pseudo-arguments odd-nth-pseudo))) 1)
  (check-true
   (css-selector-pseudo-nth-arguments?
    (css-selector-pseudo-argument-structure nth-of-pseudo)))
  (check-equal?
   (map css-selector-text
        (css-selector-pseudo-nth-arguments-selectors
         (css-selector-pseudo-argument-structure nth-of-pseudo)))
   '(".item" "#main"))
  (check-equal?
   (css-component-an-plus-b-a
    (car (css-selector-pseudo-nth-arguments-formula
          (css-selector-pseudo-argument-structure nth-of-pseudo))))
   2)
  (check-equal?
   (css-component-an-plus-b-b
    (car (css-selector-pseudo-nth-arguments-formula
          (css-selector-pseudo-argument-structure nth-of-pseudo))))
   1)
  (check-true
   (css-selector-pseudo-identifier-list?
    (css-selector-pseudo-argument-structure lang-pseudo)))
  (check-equal?
   (map css-selector-pseudo-identifier-value
        (css-selector-pseudo-arguments lang-pseudo))
   '("en-US" "da"))
  (check-true
   (css-selector-pseudo-identifier-list?
    (css-selector-pseudo-argument-structure dir-pseudo)))
  (check-equal?
   (map css-selector-pseudo-identifier-value
        (css-selector-pseudo-arguments dir-pseudo))
   '("rtl"))
  (define namespaced-attribute-selector
    (car (css-style-rule-selectors
          (css-style-rule '("svg|a[foo|href=button i]")
                          '()
                          "svg|a[foo|href=button i]"
                          #f))))
  (define namespaced-attribute-details
    (css-selector-attribute-derived-details
     (cadr (css-selector-parts namespaced-attribute-selector))))
  (check-equal? (css-selector-attribute-details-namespace namespaced-attribute-details) "foo")
  (check-equal? (css-selector-attribute-details-name namespaced-attribute-details) "href")
  (check-equal? (css-selector-attribute-details-modifier namespaced-attribute-details) "i")
  (check-true
   (css-selector-attribute-identifier-value?
    (css-selector-attribute-details-value namespaced-attribute-details)))
  (check-true (css-component-function?
               (car (css-declaration-component-values declaration))))
  (define media-details
    (css-at-rule-prelude-derived-details media-at-rule))
  (check-true (css-media-prelude-details? media-details))
  (check-equal? (length (css-media-prelude-details-queries media-details)) 2)
  (check-equal? (css-media-query-modifier (car (css-media-prelude-details-queries media-details))) "not")
  (check-equal? (css-media-query-media-type (car (css-media-prelude-details-queries media-details))) "screen")
  (check-true
   (css-media-feature-expression?
    (car (css-media-query-features (car (css-media-prelude-details-queries media-details))))))
  (check-equal? (css-media-feature-expression-name
                 (car (css-media-query-features (car (css-media-prelude-details-queries media-details)))))
                "width")
  (check-equal? (css-media-feature-expression-operator
                 (car (css-media-query-features (car (css-media-prelude-details-queries media-details)))))
                ">=")
  (check-equal? (css-media-feature-expression-value
                 (car (css-media-query-features (car (css-media-prelude-details-queries media-details)))))
                "40rem")
  (check-equal? (css-media-feature-expression-text
                 (car (css-media-query-features (car (css-media-prelude-details-queries media-details)))))
                "(width >= 40rem)")
  (check-true
   (css-media-feature-range?
    (cadr (css-media-query-features (car (css-media-prelude-details-queries media-details))))))
  (check-equal? (css-media-feature-range-name
                 (cadr (css-media-query-features (car (css-media-prelude-details-queries media-details)))))
                "width")
  (check-equal? (css-media-feature-range-lower
                 (cadr (css-media-query-features (car (css-media-prelude-details-queries media-details)))))
                "20rem")
  (check-equal? (css-media-feature-range-upper
                 (cadr (css-media-query-features (car (css-media-prelude-details-queries media-details)))))
                "60rem")
  (check-true
   (css-media-feature?
    (caddr (css-media-query-features (car (css-media-prelude-details-queries media-details))))))
  (define supports-details
    (css-at-rule-prelude-derived-details at-rule))
  (check-true (css-supports-prelude-details? supports-details))
  (check-equal? (css-supports-condition-kind (car (css-supports-prelude-details-conditions supports-details))) 'and)
  (define first-supports-feature-condition
    (car (css-supports-condition-arguments
          (car (css-supports-prelude-details-conditions supports-details)))))
  (check-equal? (css-supports-condition-kind first-supports-feature-condition) 'feature)
  (check-true
   (css-supports-feature?
    (car (css-supports-condition-arguments first-supports-feature-condition))))
  (check-equal?
   (css-supports-feature-name
    (car (css-supports-condition-arguments first-supports-feature-condition)))
   "display")
  (check-equal?
   (css-supports-feature-value
    (car (css-supports-condition-arguments first-supports-feature-condition)))
   "grid")
  (check-true (pair? (css-at-rule-prelude-values at-rule)))
  (check-true (css-component-dimension?
               (car (css-declaration-component-values numeric-declaration))))
  (check-true (css-component-percentage?
               (cadr (css-declaration-component-values numeric-declaration))))
  (check-true (css-component-number?
               (caddr (css-declaration-component-values numeric-declaration))))
  (check-true (css-component-url?
               (car (css-declaration-component-values url-declaration))))
  (check-true (css-component-hash?
               (cadr (css-declaration-component-values url-declaration))))
  (check-true (css-component-string?
               (car (css-declaration-component-values string-declaration)))))

;; parse-selector-parts : string? -> list?
;;   Parse one selector group into typed selector parts.
(define (parse-selector-parts text)
  (define len
    (string-length text))
  (let loop ([i 0]
             [parts '()]
             [pending-descendant? #f])
    (cond
      [(>= i len)
       (reverse parts)]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(char-whitespace? ch)
          (loop (skip-whitespace text i) parts (pair? parts))]
         [(member ch '(#\> #\+ #\~))
          (loop (add1 i)
                (cons (css-selector-combinator (string ch) #f) parts)
                #f)]
         [else
          (define-values (next-i node)
            (parse-selector-node text i))
          (define new-parts
            (cond
              [(and pending-descendant?
                    node
                    (not (css-selector-combinator? (car-safe parts))))
               (cons node
                     (cons (css-selector-combinator " " #f) parts))]
              [node
               (cons node parts)]
              [else
               parts]))
          (loop next-i new-parts #f)])])))

;; parse-selector-node : string? exact-nonnegative-integer? -> values
;;   Parse one selector node.
(define (parse-selector-node text start)
  (define len
    (string-length text))
  (define ch
    (string-ref text start))
  (cond
    [(char=? ch #\.)
     (define end (scan-ident text (add1 start)))
     (values end (css-selector-class (substring text (add1 start) end) #f))]
    [(char=? ch #\#)
     (define end (scan-ident text (add1 start)))
     (values end (css-selector-id (substring text (add1 start) end) #f))]
    [(char=? ch #\[)
     (parse-attribute-selector text start)]
    [(char=? ch #\:)
     (parse-pseudo-selector text start)]
    [(char=? ch #\*)
     (cond
       [(and (< (+ start 2) len)
             (char=? (string-ref text (add1 start)) #\|))
        (define next-ch
          (string-ref text (+ start 2)))
        (cond
          [(char=? next-ch #\*)
           (values (+ start 3)
                   (css-selector-namespaced-universal "*" "*|*" #f))]
          [(or (char-alphabetic? next-ch)
               (char=? next-ch #\_)
               (char=? next-ch #\-))
           (define end
             (scan-selector-name text (+ start 2)))
           (values end
                   (css-selector-namespaced-type "*"
                                                (substring text (+ start 2) end)
                                                #f))]
          [else
           (values (add1 start) (css-selector-universal "*" #f))])]
       [else
        (values (add1 start) (css-selector-universal "*" #f))])]
    [else
     (define pipe-pos
       (scan-selector-namespace-pipe text start))
     (cond
       [pipe-pos
        (define namespace
          (substring text start pipe-pos))
        (define name-start
          (add1 pipe-pos))
        (cond
          [(and (< name-start len)
                (char=? (string-ref text name-start) #\*))
           (values (add1 name-start)
                   (css-selector-namespaced-universal namespace
                                                     (substring text start (add1 name-start))
                                                     #f))]
          [else
           (define end
             (scan-selector-name text name-start))
           (values end
                   (css-selector-namespaced-type namespace
                                                (substring text name-start end)
                                                #f))])]
       [else
        (define end (scan-selector-name text start))
        (values end (css-selector-type (substring text start end) #f))])]))

;; parse-attribute-selector : string? exact-nonnegative-integer? -> values
;;   Parse one attribute selector.
(define (parse-attribute-selector text start)
  (define-values (end raw-text)
    (scan-balanced text start #\[ #\]))
  (define inner
    (string-trim (substring text (add1 start) (max (add1 start) (sub1 end)))))
  (define matcher-rx
    #px"^([^~^$*!=\\s\\]]+)\\s*(?:([~|^$*]?=)\\s*(.+?))?(?:\\s+([A-Za-z]))?$")
  (define m
    (regexp-match matcher-rx inner))
  (values end
          (if m
              (css-selector-attribute (list-ref m 1)
                                      (list-ref m 2)
                                      (list-ref m 3)
                                      (list-ref m 4)
                                      raw-text
                                      #f)
              (css-selector-attribute inner #f #f #f raw-text #f))))

;; split-attribute-name : string? -> values
;;   Split an attribute name into optional namespace and local name.
(define (split-attribute-name name)
  (let loop ([i 0]
             [len (string-length name)])
    (cond
      [(>= i len)
       (values #f name)]
      [(char=? (string-ref name i) #\|)
       (values (substring name 0 i)
               (substring name (add1 i) len))]
      [else
       (loop (add1 i) len)])))

;; parse-attribute-value : any/c -> any/c
;;   Derive a typed attribute value when present.
(define (parse-attribute-value value)
  (cond
    [(not value)
     #f]
    [(>= (string-length value) 2)
     (define first-ch
       (string-ref value 0))
     (define last-ch
       (string-ref value (sub1 (string-length value))))
     (cond
       [(and (or (char=? first-ch #\") (char=? first-ch #\'))
             (char=? first-ch last-ch))
        (css-selector-attribute-string-value value
                                             (substring value 1 (sub1 (string-length value)))
                                             #f)]
       [else
        (css-selector-attribute-identifier-value value value #f)])]
    [else
     (css-selector-attribute-identifier-value value value #f)]))

;; normalize-attribute-modifier : any/c -> any/c
;;   Normalize an attribute selector modifier when present.
(define (normalize-attribute-modifier modifier)
  (and modifier (string-downcase modifier)))

;; non-empty-string? : any/c -> boolean?
;;   Determine whether a string is non-empty.
(define (non-empty-string? s)
  (and (string? s)
       (not (string=? s ""))))

;; find-char-index : string? char? -> (or/c exact-nonnegative-integer? #f)
;;   Find the first occurrence of a character in a string.
(define (find-char-index text target)
  (define len
    (string-length text))
  (let loop ([i 0])
    (cond
      [(>= i len) #f]
      [(char=? (string-ref text i) target) i]
      [else
       (loop (add1 i))])))

;; parse-pseudo-selector : string? exact-nonnegative-integer? -> values
;;   Parse one pseudo-class or pseudo-element selector.
(define (parse-pseudo-selector text start)
  (define len
    (string-length text))
  (define element?
    (and (< (add1 start) len)
         (char=? (string-ref text (add1 start)) #\:)))
  (define name-start
    (+ start (if element? 2 1)))
  (define name-end
    (scan-ident text name-start))
  (cond
    [(and (< name-end len)
          (char=? (string-ref text name-end) #\())
     (define-values (end raw-text)
       (scan-balanced text name-end #\( #\)))
     (define args-text
       (substring text (add1 name-end) (max (add1 name-end) (sub1 end))))
     (define pseudo-name
       (substring text name-start name-end))
     (define argument-structure
       (cond
         [(selector-argument-pseudo-name? pseudo-name)
          (css-selector-pseudo-selector-list
           (parse-selector-arguments args-text)
           args-text
           #f)]
         [(nth-argument-pseudo-name? pseudo-name)
          (parse-nth-pseudo-arguments args-text)]
         [(identifier-argument-pseudo-name? pseudo-name)
          (parse-identifier-pseudo-arguments args-text)]
         [else
          (css-selector-pseudo-value-list
           (parse-pseudo-value-arguments pseudo-name args-text)
           args-text
           #f)]))
     (values end
             (css-selector-pseudo pseudo-name
                                  (pseudo-argument-structure->arguments argument-structure)
                                  argument-structure
                                  element?
                                  (substring text start end)
                                  #f))]
    [else
     (values name-end
             (css-selector-pseudo (substring text name-start name-end)
                                  '()
                                  #f
                                  element?
                                  (substring text start name-end)
                                  #f))]))

;; skip-whitespace : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Skip consecutive whitespace.
(define (skip-whitespace text start)
  (define len
    (string-length text))
  (let loop ([i start])
    (cond
      [(>= i len) i]
      [(char-whitespace? (string-ref text i))
       (loop (add1 i))]
      [else i])))

;; car-safe : list? -> any/c
;;   Return the first item when present.
(define (car-safe xs)
  (and (pair? xs) (car xs)))

;; parse-selector-arguments : string? -> list?
;;   Parse selector-like pseudo arguments as selector groups.
(define (parse-selector-arguments text)
  (map (lambda (selector-text)
         (css-selector selector-text #f (parse-selector-parts selector-text)))
       (split-top-level-selector-groups text)))

;; parse-pseudo-value-arguments : string? string? -> list?
;;   Parse value-oriented pseudo arguments, recognizing an+b where useful.
(define (parse-pseudo-value-arguments pseudo-name text)
  (cond
    [(nth-argument-pseudo-name? pseudo-name)
     (or (parse-an-plus-b text)
         (parse-component-values text))]
    [else
     (parse-component-values text)]))

;; parse-nth-pseudo-arguments : string? -> css-selector-pseudo-nth-arguments?
;;   Parse nth-* pseudo arguments with optional `of` selector clause.
(define (parse-nth-pseudo-arguments text)
  (define-values (formula-text selectors-text)
    (split-nth-of-clause text))
  (css-selector-pseudo-nth-arguments
   (or (parse-an-plus-b formula-text)
       (parse-component-values formula-text))
   (if selectors-text
       (parse-selector-arguments selectors-text)
       '())
   text
   #f))

;; parse-identifier-pseudo-arguments : string? -> css-selector-pseudo-identifier-list?
;;   Parse identifier-like pseudo arguments such as :lang(...) and :dir(...).
(define (parse-identifier-pseudo-arguments text)
  (css-selector-pseudo-identifier-list
   (map (lambda (part)
          (css-selector-pseudo-identifier part part #f))
        (split-top-level-commas text))
   text
   #f))

;; pseudo-argument-structure->arguments : any/c -> list?
;;   Preserve a backward-compatible flat argument view for pseudo nodes.
(define (pseudo-argument-structure->arguments argument-structure)
  (cond
    [(css-selector-pseudo-selector-list? argument-structure)
     (css-selector-pseudo-selector-list-selectors argument-structure)]
    [(css-selector-pseudo-value-list? argument-structure)
     (css-selector-pseudo-value-list-values argument-structure)]
    [(css-selector-pseudo-nth-arguments? argument-structure)
     (append (css-selector-pseudo-nth-arguments-formula argument-structure)
             (css-selector-pseudo-nth-arguments-selectors argument-structure))]
    [(css-selector-pseudo-identifier-list? argument-structure)
     (css-selector-pseudo-identifier-list-values argument-structure)]
    [else
     '()]))

;; parse-an-plus-b : string? -> (or/c list? #f)
;;   Parse an an+b microsyntax into one typed component node.
(define (parse-an-plus-b text)
  (define normalized
    (regexp-replace* #px"\\s+" (string-downcase (string-trim text)) ""))
  (define (->node raw a b)
    (list (css-component-an-plus-b raw a b #f)))
  (cond
    [(string=? normalized "odd")
     (->node text 2 1)]
    [(string=? normalized "even")
     (->node text 2 0)]
    [else
     (define m-linear
       (regexp-match #px"^([+-]?)(?:(\\d+))?n(?:(([+-])(\\d+)))?$" normalized))
     (cond
       [m-linear
        (define sign-text
          (list-ref m-linear 1))
        (define coeff-text
          (list-ref m-linear 2))
        (define b-sign
          (list-ref m-linear 4))
        (define b-text
          (list-ref m-linear 5))
        (define a
          (* (if (equal? sign-text "-") -1 1)
             (if coeff-text
                 (string->number coeff-text)
                 1)))
        (define b
          (cond
            [b-text
             (* (if (equal? b-sign "-") -1 1)
                (string->number b-text))]
            [else
             0]))
        (->node text a b)]
       [else
        (define m-integer
          (regexp-match #px"^([+-]?\\d+)$" normalized))
        (and m-integer
             (->node text 0 (string->number normalized)))])]))

;; split-nth-of-clause : string? -> values
;;   Split `an+b of selector-list` at a top-level `of` keyword when present.
(define (split-nth-of-clause text)
  (define len
    (string-length text))
  (let loop ([i 0]
             [paren-depth 0]
             [bracket-depth 0])
    (cond
      [(>= i len)
       (values (string-trim text) #f)]
      [else
       (define ch
         (string-ref text i))
       (case ch
         [(#\()
          (loop (add1 i) (add1 paren-depth) bracket-depth)]
         [(#\))
          (loop (add1 i) (max 0 (sub1 paren-depth)) bracket-depth)]
         [(#\[)
          (loop (add1 i) paren-depth (add1 bracket-depth))]
         [(#\])
          (loop (add1 i) paren-depth (max 0 (sub1 bracket-depth)))]
         [else
          (cond
            [(and (= paren-depth 0)
                  (= bracket-depth 0)
                  (top-level-of-keyword-at? text i))
             (values (string-trim (substring text 0 i))
                     (string-trim (substring text (+ i 2) len)))]
            [else
             (loop (add1 i) paren-depth bracket-depth)])])])))

;; top-level-of-keyword-at? : string? exact-nonnegative-integer? -> boolean?
;;   Determine whether `of` occurs as a standalone top-level keyword.
(define (top-level-of-keyword-at? text i)
  (define len
    (string-length text))
  (and (<= (+ i 2) len)
       (string-ci=? (substring text i (+ i 2)) "of")
       (or (= i 0)
           (char-whitespace? (string-ref text (sub1 i))))
       (or (= (+ i 2) len)
           (char-whitespace? (string-ref text (+ i 2))))))

;; selector-parts->compounds : list? -> list?
;;   Group selector parts into compounds separated by combinators.
(define (selector-parts->compounds parts)
  (let loop ([remaining parts]
             [current '()]
             [groups '()])
    (cond
      [(null? remaining)
       (reverse
        (if (pair? current)
            (cons (css-selector-compound (reverse current) #f) groups)
            groups))]
      [else
       (define part
         (car remaining))
       (cond
         [(css-selector-combinator? part)
          (loop (cdr remaining)
                '()
                (append (if (pair? current)
                            (list (css-selector-compound (reverse current) #f) part)
                            (list part))
                        groups))]
         [else
          (loop (cdr remaining)
                (cons part current)
                groups)])])))

;; scan-selector-name : string? exact-nonnegative-integer? -> exact-nonnegative-integer?
;;   Scan a selector name without consuming the next simple-selector marker.
(define (scan-selector-name text start)
  (define len
    (string-length text))
  (let loop ([i start])
    (cond
      [(>= i len) i]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(or (char-whitespace? ch)
              (member ch '(#\. #\# #\[ #\: #\> #\+ #\~ #\, #\) #\] #\})))
          i]
         [else
          (loop (add1 i))])])))

;; scan-selector-namespace-pipe : string? exact-nonnegative-integer? -> (or/c exact-nonnegative-integer? #f)
;;   Find a namespace separator in a type/universal selector position.
(define (scan-selector-namespace-pipe text start)
  (define len
    (string-length text))
  (let loop ([i start])
    (cond
      [(>= i len) #f]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(char=? ch #\|) i]
         [(or (char-whitespace? ch)
              (member ch '(#\. #\# #\[ #\: #\> #\+ #\~ #\, #\) #\] #\})))
          #f]
         [else
          (loop (add1 i))])])))

;; split-top-level-commas : string? -> list?
;;   Split a prelude on top-level commas.
(define (split-top-level-commas text)
  (split-top-level-delimited text #\,))

;; split-top-level-keyword : string? string? -> list?
;;   Split on a top-level standalone keyword such as `and` or `or`.
(define (split-top-level-keyword text keyword)
  (define len
    (string-length text))
  (define key-len
    (string-length keyword))
  (let loop ([i 0]
             [paren-depth 0]
             [bracket-depth 0]
             [chunk-start 0]
             [chunks '()])
    (cond
      [(>= i len)
       (define chunk
         (string-trim (substring text chunk-start len)))
       (reverse (if (string=? chunk "") chunks (cons chunk chunks)))]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(string-delimiter? ch)
          (loop (scan-string text i) paren-depth bracket-depth chunk-start chunks)]
         [(char=? ch #\()
          (loop (add1 i) (add1 paren-depth) bracket-depth chunk-start chunks)]
         [(char=? ch #\))
          (loop (add1 i) (max 0 (sub1 paren-depth)) bracket-depth chunk-start chunks)]
         [(char=? ch #\[)
          (loop (add1 i) paren-depth (add1 bracket-depth) chunk-start chunks)]
         [(char=? ch #\])
          (loop (add1 i) paren-depth (max 0 (sub1 bracket-depth)) chunk-start chunks)]
         [(and (= paren-depth 0)
               (= bracket-depth 0)
               (<= (+ i key-len) len)
               (string-ci=? (substring text i (+ i key-len)) keyword)
               (or (= i 0)
                   (char-whitespace? (string-ref text (sub1 i))))
               (or (= (+ i key-len) len)
                   (char-whitespace? (string-ref text (+ i key-len)))))
          (define chunk
            (string-trim (substring text chunk-start i)))
          (loop (+ i key-len)
                paren-depth
                bracket-depth
                (+ i key-len)
                (if (string=? chunk "") chunks (cons chunk chunks)))]
         [else
          (loop (add1 i) paren-depth bracket-depth chunk-start chunks)])])))

;; split-top-level-delimited : string? char? -> list?
;;   Split a string on a top-level delimiter.
(define (split-top-level-delimited text delimiter)
  (define len
    (string-length text))
  (let loop ([i 0]
             [paren-depth 0]
             [bracket-depth 0]
             [chunk-start 0]
             [chunks '()])
    (cond
      [(>= i len)
       (define chunk
         (string-trim (substring text chunk-start len)))
       (reverse (if (string=? chunk "") chunks (cons chunk chunks)))]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(string-delimiter? ch)
          (loop (scan-string text i) paren-depth bracket-depth chunk-start chunks)]
         [(char=? ch #\()
          (loop (add1 i) (add1 paren-depth) bracket-depth chunk-start chunks)]
         [(char=? ch #\))
          (loop (add1 i) (max 0 (sub1 paren-depth)) bracket-depth chunk-start chunks)]
         [(char=? ch #\[)
          (loop (add1 i) paren-depth (add1 bracket-depth) chunk-start chunks)]
         [(char=? ch #\])
          (loop (add1 i) paren-depth (max 0 (sub1 bracket-depth)) chunk-start chunks)]
         [(and (= paren-depth 0)
               (= bracket-depth 0)
               (char=? ch delimiter))
          (define chunk
            (string-trim (substring text chunk-start i)))
          (loop (add1 i)
                paren-depth
                bracket-depth
                (add1 i)
                (if (string=? chunk "") chunks (cons chunk chunks)))]
         [else
          (loop (add1 i) paren-depth bracket-depth chunk-start chunks)])])))

;; strip-outer-parens-if-balanced : string? -> string?
;;   Remove one balanced outer parenthesis layer when it covers the whole text.
(define (strip-outer-parens-if-balanced text)
  (define trimmed
    (string-trim text))
  (define len
    (string-length trimmed))
  (cond
    [(and (>= len 2)
          (char=? (string-ref trimmed 0) #\()
          (char=? (string-ref trimmed (sub1 len)) #\))
          (balanced-outer-parens? trimmed))
     (string-trim (substring trimmed 1 (sub1 len)))]
    [else
     trimmed]))

;; balanced-outer-parens? : string? -> boolean?
;;   Determine whether one outer pair of parentheses spans the whole string.
(define (balanced-outer-parens? text)
  (define len
    (string-length text))
  (let loop ([i 0]
             [depth 0])
    (cond
      [(>= i len)
       (zero? depth)]
      [else
       (define ch
         (string-ref text i))
       (cond
         [(string-delimiter? ch)
          (loop (scan-string text i) depth)]
         [(char=? ch #\()
          (loop (add1 i) (add1 depth))]
         [(char=? ch #\))
          (define next-depth
            (sub1 depth))
          (and (>= next-depth 0)
               (or (< i (sub1 len))
                   (zero? next-depth))
               (loop (add1 i) next-depth))]
         [else
          (loop (add1 i) depth)])])))

;; split-top-level-selector-groups : string? -> list?
;;   Split a selector list on top-level commas.
(define (split-top-level-selector-groups text)
  (define len
    (string-length text))
  (let loop ([i 0]
             [paren-depth 0]
             [bracket-depth 0]
             [chunk-start 0]
             [chunks '()])
    (cond
      [(= i len)
       (define chunk
         (string-trim (substring text chunk-start len)))
       (reverse (if (string=? chunk "") chunks (cons chunk chunks)))]
      [else
       (define ch
         (string-ref text i))
       (case ch
         [(#\()
          (loop (add1 i) (add1 paren-depth) bracket-depth chunk-start chunks)]
         [(#\))
          (loop (add1 i) (max 0 (sub1 paren-depth)) bracket-depth chunk-start chunks)]
         [(#\[)
          (loop (add1 i) paren-depth (add1 bracket-depth) chunk-start chunks)]
         [(#\])
          (loop (add1 i) paren-depth (max 0 (sub1 bracket-depth)) chunk-start chunks)]
         [(#\,)
          (cond
            [(and (= paren-depth 0) (= bracket-depth 0))
             (define chunk
               (string-trim (substring text chunk-start i)))
             (loop (add1 i)
                   paren-depth
                   bracket-depth
                   (add1 i)
                   (if (string=? chunk "") chunks (cons chunk chunks)))]
            [else
             (loop (add1 i) paren-depth bracket-depth chunk-start chunks)])]
         [else
          (loop (add1 i) paren-depth bracket-depth chunk-start chunks)])])))

;; selector-argument-pseudo-name? : string? -> boolean?
;;   Determine whether a functional pseudo expects selector arguments.
(define (selector-argument-pseudo-name? name)
  (member (string-downcase name)
          '("not" "is" "where" "has" "matches" "-moz-any" "-webkit-any")))

;; nth-argument-pseudo-name? : string? -> boolean?
;;   Determine whether a functional pseudo expects an+b-style arguments.
(define (nth-argument-pseudo-name? name)
  (member (string-downcase name)
          '("nth-child" "nth-last-child" "nth-of-type" "nth-last-of-type"
            "nth-col" "nth-last-col")))

;; identifier-argument-pseudo-name? : string? -> boolean?
;;   Determine whether a functional pseudo expects identifier-like arguments.
(define (identifier-argument-pseudo-name? name)
  (member (string-downcase name)
          '("lang" "dir")))
