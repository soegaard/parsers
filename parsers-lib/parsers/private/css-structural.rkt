#lang racket/base

;;;
;;; CSS Structural Reader
;;;
;;
;; Handwritten structural parser for the current CSS scaffold. This layer
;; consumes raw CSS tokens and builds the outer AST while preserving raw source
;; slices for selectors, at-rule preludes, and declaration values.

;; parse-css-structurally : string? -> css-stylesheet?
;;   Parse a stylesheet using handwritten structural token consumption.

(provide parse-css-structurally)

(require lexers/css
         parser-tools/lex
         racket/list
         racket/string
         "css-ast.rkt"
         "css-errors.rkt")

(define source-offset-map-cache
  (make-weak-hasheq))

(struct token-stream (lexer in buffer)
  #:mutable
  #:transparent)

;; parse-css-structurally : string? -> css-stylesheet?
;;   Parse a stylesheet using handwritten structural token consumption.
(define (parse-css-structurally source)
  (define in
    (open-input-string source))
  (port-count-lines! in)
  (define stream
    (token-stream (make-css-raw-lexer) in '()))
  (define rules
    (parse-stylesheet-items stream source))
  (css-stylesheet rules
                  source
                  (and (positive? (string-length source))
                       (css-source-span (offset->position 1)
                                        (offset->position (add1 (string-length source)))))))

;; parse-stylesheet-items : token-stream? string? -> (listof any/c)
;;   Parse the top-level stylesheet items until eof.
(define (parse-stylesheet-items stream source)
  (let loop ([items '()])
    (define token
      (peek-significant-token stream))
    (cond
      [(eq? token 'eof)
       (reverse items)]
      [(eq? (css-raw-token-kind token) 'CDO-token)
       (consume-significant-token! stream)
       (loop items)]
      [(eq? (css-raw-token-kind token) 'CDC-token)
       (consume-significant-token! stream)
       (loop items)]
      [(eq? (css-raw-token-kind token) 'comment-token)
       (loop (cons (consume-comment stream) items))]
      [(eq? (css-raw-token-kind token) 'at-keyword-token)
       (with-handlers ([exn:fail:css?
                        (lambda (e)
                          (define recovery
                            (recover-invalid-statement! stream source e))
                          (loop (if recovery
                                    (cons recovery items)
                                    items)))])
         (loop (cons (parse-at-rule stream source) items)))]
      [else
       (with-handlers ([exn:fail:css?
                        (lambda (e)
                          (define recovery
                            (recover-invalid-statement! stream source e))
                          (loop (if recovery
                                    (cons recovery items)
                                    items)))])
         (loop (cons (parse-style-rule stream source) items)))])))

;; parse-at-rule : token-stream? string? -> css-at-rule?
;;   Parse one at-rule, preserving its raw prelude.
(define (parse-at-rule stream source)
  (define at-token
    (consume-significant-token! stream))
  (define name
    (css-raw-token-text at-token))
  (define start-pos
    (css-raw-token-start at-token))
  (define-values (prelude-start prelude-end terminator-kind)
    (consume-until-top-level stream '(semicolon-token open-brace-token)))
  (define prelude
    (cond
      [(and prelude-start prelude-end)
       (string-trim (source-slice source prelude-start prelude-end))]
      [else
       ""]))
  (case terminator-kind
    [(semicolon-token)
     (define semi
       (consume-significant-token! stream))
     (css-at-rule name
                  prelude
                  #f
                  (css-source-span start-pos (css-raw-token-end semi)))]
    [(open-brace-token)
     (define-values (body end-pos)
       (parse-brace-block stream source #:at-rule-name name))
     (css-at-rule name
                  prelude
                  body
                  (css-source-span start-pos end-pos))]
    [else
     (raise-css-parse-error
      'parse-css
      'at-rule
      name
      start-pos
      start-pos
      "unterminated at-rule")]))

;; parse-style-rule : token-stream? string? -> css-style-rule?
;;   Parse one style rule, preserving raw selector text.
(define (parse-style-rule stream source)
  (define selector-start
    (css-raw-token-start (peek-significant-token stream)))
  (define-values (_ selector-end terminator-kind)
    (consume-until-top-level stream '(open-brace-token)))
  (unless (eq? terminator-kind 'open-brace-token)
    (raise-css-parse-error
     'parse-css
     'style-rule
     #f
     selector-start
     selector-start
     "expected style rule block"))
  (define raw-selector
    (string-trim (source-slice source selector-start selector-end)))
  (define-values (block end-pos)
    (parse-brace-block stream source))
  (css-style-rule (top-level-selector-groups raw-selector)
                  block
                  raw-selector
                  (css-source-span selector-start end-pos)))

;; parse-brace-block : token-stream? string? keyword-arguments -> any/c
;;   Parse a brace-delimited block.
(define (parse-brace-block stream source #:at-rule-name [at-rule-name #f])
  (define open-token
    (consume-significant-token! stream))
  (unless (eq? (css-raw-token-kind open-token) 'open-brace-token)
    (raise-css-parse-error
     'parse-css
     'open-brace-token
     (css-raw-token-text open-token)
     (css-raw-token-start open-token)
     (css-raw-token-end open-token)
     "expected open brace"))
  (define-values (items close-token)
    (cond
      [(stylesheet-body-at-rule-name? at-rule-name)
       (parse-stylesheet-items-until-brace stream source)]
      [else
       (parse-declarations-until-brace stream source)]))
  (when (eq? close-token 'eof)
    (raise-css-parse-error
     'parse-css
     'close-brace-token
     at-rule-name
     (css-raw-token-start open-token)
     (css-raw-token-end open-token)
     "unterminated block"))
  (values items
          (css-raw-token-end close-token)))

;; stylesheet-body-at-rule-name? : any/c -> boolean?
;;   Determine whether an at-rule block contains nested rule items.
(define (stylesheet-body-at-rule-name? at-rule-name)
  (cond
    [(not at-rule-name)
     #f]
    [(member at-rule-name '("@media" "@supports"))
     #t]
    [else
     (regexp-match? #px"^@(?:-[A-Za-z0-9]+-)?keyframes$" at-rule-name)]))

;; parse-stylesheet-items-until-brace : token-stream? string? -> values
;;   Parse nested rule items until the matching close brace.
(define (parse-stylesheet-items-until-brace stream source)
  (let loop ([items '()])
    (define token
      (peek-significant-token stream))
    (cond
      [(eq? token 'eof)
       (values (reverse items) 'eof)]
      [(eq? (css-raw-token-kind token) 'close-brace-token)
       (define close-token
         (consume-significant-token! stream))
       (values (reverse items) close-token)]
      [(eq? (css-raw-token-kind token) 'comment-token)
       (loop (cons (consume-comment stream) items))]
      [(eq? (css-raw-token-kind token) 'at-keyword-token)
       (with-handlers ([exn:fail:css?
                        (lambda (e)
                          (define recovery
                            (recover-invalid-statement! stream source e))
                          (loop (if recovery
                                    (cons recovery items)
                                    items)))])
         (loop (cons (parse-at-rule stream source) items)))]
      [else
       (with-handlers ([exn:fail:css?
                        (lambda (e)
                          (define recovery
                            (recover-invalid-statement! stream source e))
                          (loop (if recovery
                                    (cons recovery items)
                                    items)))])
         (loop (cons (parse-style-rule stream source) items)))])))

;; parse-declarations-until-brace : token-stream? string? -> values
;;   Parse declaration items until the matching close brace.
(define (parse-declarations-until-brace stream source)
  (let loop ([items '()])
    (define token
      (peek-significant-token stream))
    (cond
      [(eq? token 'eof)
       (values (reverse items) 'eof)]
      [(eq? (css-raw-token-kind token) 'close-brace-token)
       (define close-token
         (consume-significant-token! stream))
       (values (reverse items) close-token)]
      [(eq? (css-raw-token-kind token) 'comment-token)
       (loop (cons (consume-comment stream) items))]
      [(eq? (css-raw-token-kind token) 'at-keyword-token)
       (loop (cons (parse-at-rule stream source) items))]
      [(eq? (css-raw-token-kind token) 'semicolon-token)
       (consume-significant-token! stream)
       (loop items)]
      [else
       (with-handlers ([exn:fail:css?
                        (lambda (e)
                          (define recovery
                            (recover-invalid-declaration! stream source e))
                          (loop (if recovery
                                    (cons recovery items)
                                    items)))])
         (loop (cons (parse-declaration stream source) items)))])))

;; parse-declaration : token-stream? string? -> css-declaration?
;;   Parse one declaration with raw value preservation.
(define (parse-declaration stream source)
  (define-values (name-text name-start-pos _name-end-pos)
    (consume-declaration-name! stream))
  (define colon-token
    (consume-significant-token! stream))
  (unless (eq? (css-raw-token-kind colon-token) 'colon-token)
    (raise-css-parse-error
     'parse-css
     'colon-token
     (css-raw-token-text colon-token)
     (css-raw-token-start colon-token)
     (css-raw-token-end colon-token)
     "expected declaration colon"))
  (define-values (value-start value-end terminator-kind)
    (consume-until-top-level stream '(semicolon-token close-brace-token)))
  (define raw-value
    (cond
      [(and value-start value-end)
       (string-trim (source-slice source value-start value-end))]
      [else
       ""]))
  (define important?
    (regexp-match? #px"!important\\s*$" raw-value))
  (case terminator-kind
    [(semicolon-token)
     (define semi
       (consume-significant-token! stream))
     (css-declaration name-text
                      raw-value
                      important?
                      (css-source-span name-start-pos
                                       (css-raw-token-end semi)))]
    [(close-brace-token)
     (css-declaration name-text
                      raw-value
                      important?
                      (css-source-span name-start-pos
                                       (or value-end
                                           (css-raw-token-end colon-token))))]
    [else
     (raise-css-parse-error
      'parse-css
      'declaration
      name-text
      name-start-pos
      name-start-pos
      "unterminated declaration")]))

;; consume-declaration-name! : token-stream? -> values
;;   Consume a declaration name, allowing common prefixed property hacks.
(define (consume-declaration-name! stream)
  (define first-token
    (consume-significant-token! stream))
  (cond
    [(eq? (css-raw-token-kind first-token) 'ident-token)
     (values (css-raw-token-text first-token)
             (css-raw-token-start first-token)
             (css-raw-token-end first-token))]
    [(and (eq? (css-raw-token-kind first-token) 'delim-token)
          (member (css-raw-token-text first-token) '("*" "_")))
     (define second-token
       (consume-significant-token! stream))
     (unless (eq? (css-raw-token-kind second-token) 'ident-token)
       (raise-css-parse-error
        'parse-css
        'ident-token
        (css-raw-token-text second-token)
        (css-raw-token-start second-token)
        (css-raw-token-end second-token)
        "expected declaration name"))
     (values (string-append (css-raw-token-text first-token)
                            (css-raw-token-text second-token))
             (css-raw-token-start first-token)
             (css-raw-token-end second-token))]
    [else
     (raise-css-parse-error
      'parse-css
      'ident-token
      (css-raw-token-text first-token)
      (css-raw-token-start first-token)
      (css-raw-token-end first-token)
      "expected declaration name")]))

;; consume-comment : token-stream? -> css-comment?
;;   Consume a comment token as a first-class AST node.
(define (consume-comment stream)
  (define token
    (peek-significant-token stream))
  (unless (and (not (eq? token 'eof))
               (eq? (css-raw-token-kind token) 'comment-token))
    (raise-css-parse-error
     'parse-css
     'comment-token
     (and (not (eq? token 'eof))
          (css-raw-token-text token))
     (and (not (eq? token 'eof))
          (css-raw-token-start token))
     (and (not (eq? token 'eof))
          (css-raw-token-end token))
     "expected comment"))
  (consume-raw-token! stream)
  (css-comment (css-raw-token-text token)
               (css-source-span (css-raw-token-start token)
                                (css-raw-token-end token))))

;; skip-invalid-declaration! : token-stream? -> void?
;;   Recover from a malformed declaration by skipping to the next boundary.
(define (skip-invalid-declaration! stream)
  (let loop ([paren-depth 0]
             [bracket-depth 0]
             [brace-depth 0]
             [last-end-pos #f])
    (define token
      (peek-significant-token stream))
    (cond
      [(eq? token 'eof)
       last-end-pos]
      [else
       (define kind
         (css-raw-token-kind token))
       (cond
         [(and (= paren-depth 0)
               (= bracket-depth 0)
               (= brace-depth 0)
               (eq? kind 'semicolon-token))
          (define semi
            (consume-significant-token! stream))
          (css-raw-token-end semi)]
         [(and (= paren-depth 0)
               (= bracket-depth 0)
               (= brace-depth 0)
               (eq? kind 'close-brace-token))
          last-end-pos]
         [else
          (define consumed-token
            (consume-significant-token! stream))
          (loop (case kind
                  [(open-paren-token) (add1 paren-depth)]
                  [(close-paren-token) (max 0 (sub1 paren-depth))]
                  [else paren-depth])
                (case kind
                  [(open-bracket-token) (add1 bracket-depth)]
                  [(close-bracket-token) (max 0 (sub1 bracket-depth))]
                  [else bracket-depth])
                (case kind
                  [(open-brace-token) (add1 brace-depth)]
                  [(close-brace-token) (max 0 (sub1 brace-depth))]
                  [else brace-depth])
                (css-raw-token-end consumed-token))])])))

;; skip-invalid-statement! : token-stream? -> void?
;;   Recover from a malformed stylesheet item by skipping one statement.
(define (skip-invalid-statement! stream)
  (let loop ([paren-depth 0]
             [bracket-depth 0]
             [brace-depth 0]
             [saw-open-brace? #f]
             [last-end-pos #f])
    (define token
      (peek-significant-token stream))
    (cond
      [(eq? token 'eof)
       last-end-pos]
      [else
       (define kind
         (css-raw-token-kind token))
       (cond
         [(and (= paren-depth 0)
               (= bracket-depth 0)
               (= brace-depth 0)
               (eq? kind 'semicolon-token))
          (define semi
            (consume-significant-token! stream))
          (css-raw-token-end semi)]
         [(and saw-open-brace?
               (= paren-depth 0)
               (= bracket-depth 0)
               (= brace-depth 0)
               (eq? kind 'close-brace-token))
          (define close-token
            (consume-significant-token! stream))
          (css-raw-token-end close-token)]
         [(and (not saw-open-brace?)
               (= paren-depth 0)
               (= bracket-depth 0)
               (= brace-depth 0)
               (eq? kind 'close-brace-token))
          last-end-pos]
         [else
          (define consumed-token
            (consume-significant-token! stream))
          (loop (case kind
                  [(open-paren-token) (add1 paren-depth)]
                  [(close-paren-token) (max 0 (sub1 paren-depth))]
                  [else paren-depth])
                (case kind
                  [(open-bracket-token) (add1 bracket-depth)]
                  [(close-bracket-token) (max 0 (sub1 bracket-depth))]
                  [else bracket-depth])
                (case kind
                  [(open-brace-token) (add1 brace-depth)]
                  [(close-brace-token) (max 0 (sub1 brace-depth))]
                  [else brace-depth])
                (or saw-open-brace?
                    (eq? kind 'open-brace-token))
                (css-raw-token-end consumed-token))])])))

;; recover-invalid-declaration! : token-stream? string? exn:fail:css? -> (or/c css-recovery? #f)
;;   Recover from a malformed declaration and return a recovery node.
(define (recover-invalid-declaration! stream source e)
  (define start-pos
    (detail-ref (exn:fail:css-detail e) 'start-pos))
  (define end-pos
    (or (skip-invalid-declaration! stream)
        (detail-ref (exn:fail:css-detail e) 'end-pos)
        start-pos))
  (make-recovery-node 'declaration (exn-message e) source start-pos end-pos (exn:fail:css-detail e)))

;; recover-invalid-statement! : token-stream? string? exn:fail:css? -> (or/c css-recovery? #f)
;;   Recover from a malformed stylesheet statement and return a recovery node.
(define (recover-invalid-statement! stream source e)
  (define start-pos
    (detail-ref (exn:fail:css-detail e) 'start-pos))
  (define end-pos
    (or (skip-invalid-statement! stream)
        (detail-ref (exn:fail:css-detail e) 'end-pos)
        start-pos))
  (make-recovery-node 'statement (exn-message e) source start-pos end-pos (exn:fail:css-detail e)))

;; make-recovery-node : symbol? string? string? any/c any/c any/c -> (or/c css-recovery? #f)
;;   Construct a recovery node when enough source information exists.
(define (make-recovery-node kind reason source start-pos end-pos detail)
  (cond
    [(and start-pos end-pos)
     (css-recovery kind
                   reason
                   (source-slice source start-pos end-pos)
                   (css-source-span start-pos end-pos)
                   detail)]
    [else
     #f]))

;; detail-ref : any/c symbol? -> any/c
;;   Extract one key from a parse-error detail alist.
(define (detail-ref detail key)
  (cond
    [(pair? detail)
     (define a
       (assq key detail))
     (and a (cdr a))]
    [else
     #f]))

;; consume-until-top-level : token-stream? list? -> values
;;   Consume tokens until one of the stop kinds appears at top level.
(define (consume-until-top-level stream stop-kinds)
  (let loop ([start-pos #f]
             [end-pos #f]
             [paren-depth 0]
             [bracket-depth 0]
             [brace-depth 0])
    (define token
      (peek-significant-token stream))
    (cond
      [(eq? token 'eof)
       (values start-pos end-pos 'eof)]
      [else
       (define kind
         (css-raw-token-kind token))
       (cond
         [(and (= paren-depth 0)
               (= bracket-depth 0)
               (= brace-depth 0)
               (member kind stop-kinds))
          (values start-pos end-pos kind)]
         [else
          (consume-significant-token! stream)
          (loop (or start-pos (css-raw-token-start token))
                (css-raw-token-end token)
                (case kind
                  [(open-paren-token) (add1 paren-depth)]
                  [(close-paren-token) (max 0 (sub1 paren-depth))]
                  [else paren-depth])
                (case kind
                  [(open-bracket-token) (add1 bracket-depth)]
                  [(close-bracket-token) (max 0 (sub1 bracket-depth))]
                  [else bracket-depth])
                (case kind
                  [(open-brace-token) (add1 brace-depth)]
                  [(close-brace-token) (max 0 (sub1 brace-depth))]
                  [else brace-depth]))])])))

;; top-level-selector-groups : string? -> (listof string?)
;;   Split a selector prelude on top-level commas.
(define (top-level-selector-groups text)
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

;; source-slice : string? position? position? -> string?
;;   Extract a source slice using parser-tools positions.
(define (source-slice source start-pos end-pos)
  (cond
    [(or (not start-pos) (not end-pos))
     ""]
    [else
     (define start-offset
       (position-offset start-pos))
     (define end-offset
       (position-offset end-pos))
     (substring source
                (logical-offset->string-index source start-offset)
                (logical-offset->string-index source end-offset))]))

;; logical-offset->string-index : string? exact-positive-integer? -> exact-nonnegative-integer?
;;   Convert a parser position offset into a concrete string index.
;;
;; The lexer reports offsets where a CRLF newline counts as one logical step.
;; Racket strings count CR and LF as separate characters, so Windows-style
;; inputs need an offset-to-index conversion before substring slicing.
(define (logical-offset->string-index source offset)
  (define mapping
    (hash-ref! source-offset-map-cache
               source
               (lambda ()
                 (build-source-offset-map source))))
  (define max-offset
    (sub1 (vector-length mapping)))
  (vector-ref mapping
              (min (max 1 offset)
                   max-offset)))

;; build-source-offset-map : string? -> vector?
;;   Build a mapping from logical parser offsets to concrete string indices.
(define (build-source-offset-map source)
  (define len
    (string-length source))
  (define indices
    (let loop ([index 0]
               [acc '(0)])
      (cond
        [(>= index len)
         (reverse acc)]
        [(and (char=? (string-ref source index) #\return)
              (< (add1 index) len)
              (char=? (string-ref source (add1 index)) #\newline))
         (define next-index
           (+ index 2))
         (loop next-index
               (cons next-index acc))]
        [else
         (define next-index
           (add1 index))
         (loop next-index
               (cons next-index acc))])))
  (list->vector (cons 0 indices)))

;; peek-significant-token : token-stream? -> (or/c css-raw-token? 'eof)
;;   Peek the next non-trivia token.
(define (peek-significant-token stream)
  (let loop ()
    (define token
      (peek-raw-token stream))
    (cond
      [(eq? token 'eof) 'eof]
      [(trivia-token? token)
       (consume-raw-token! stream)
       (loop)]
      [else
       token])))

;; consume-significant-token! : token-stream? -> css-raw-token?
;;   Consume and return the next non-trivia token.
(define (consume-significant-token! stream)
  (define token
    (peek-significant-token stream))
  (cond
    [(eq? token 'eof)
     (raise-css-parse-error 'parse-css 'eof #f #f #f "unexpected eof")]
    [else
     (consume-raw-token! stream)]))

;; trivia-token? : css-raw-token? -> boolean?
;;   Determine whether a token is parser trivia.
(define (trivia-token? token)
  (case (css-raw-token-kind token)
    [(whitespace-token) #t]
    [else               #f]))

;; peek-raw-token : token-stream? -> (or/c css-raw-token? 'eof)
;;   Peek the next raw token from the stream.
(define (peek-raw-token stream)
  (cond
    [(pair? (token-stream-buffer stream))
     (car (token-stream-buffer stream))]
    [else
     (define token
       ((token-stream-lexer stream) (token-stream-in stream)))
     (set-token-stream-buffer! stream (list token))
     token]))

;; consume-raw-token! : token-stream? -> (or/c css-raw-token? 'eof)
;;   Consume the next raw token from the stream.
(define (consume-raw-token! stream)
  (define token
    (peek-raw-token stream))
  (set-token-stream-buffer! stream '())
  token)

;; offset->position : exact-nonnegative-integer? -> position?
;;   Construct a coarse position from an offset for whole-source spans.
(define (offset->position offset)
  (make-position offset 1 offset))

(module+ test
  (require rackunit)

  (define stylesheet
    (parse-css-structurally "body { color: red; }"))
  (define rule
    (car (css-stylesheet-rules stylesheet)))
  (define declaration
    (car (css-style-rule-block rule)))
  (define grouped-stylesheet
    (parse-css-structurally ".a, .b { background: rgb(1 2 3); }"))
  (define grouped-rule
    (car (css-stylesheet-rules grouped-stylesheet)))
  (define comment-stylesheet
    (parse-css-structurally "/* top */ body { /* inner */ color: red; }"))
  (define hacked-declaration-stylesheet
    (parse-css-structurally "body { *zoom: 1; _width: 10px; color: red; }"))
  (define recovered-stylesheet
    (parse-css-structurally "body { content: \"Hint.\";s font-style: italic; color: red; }"))
  (define supports-stylesheet
    (parse-css-structurally "@supports (display: grid) { .a { color: red; } }"))
  (define import-stylesheet
    (parse-css-structurally "@import url(\"x.css\") screen;"))
  (define font-face-stylesheet
    (parse-css-structurally "@font-face { font-family: \"X\"; src: url(\"x.woff2\"); }"))
  (define page-stylesheet
    (parse-css-structurally "@page { @bottom-right { content: counter(page); } }"))
  (define keyframes-stylesheet
    (parse-css-structurally "@keyframes fade { from { opacity: 0; } to { opacity: 1; } }"))

  (check-true (css-stylesheet? stylesheet))
  (check-true (css-style-rule? rule))
  (check-equal? (css-style-rule-raw-selector rule) "body")
  (check-equal? (css-declaration-name declaration) "color")
  (check-equal? (css-declaration-value declaration) "red")
  (check-equal? (css-style-rule-selector-groups grouped-rule)
                '(".a" ".b"))
  (define top-comment
    (car (css-stylesheet-rules comment-stylesheet)))
  (check-true (css-comment? top-comment))
  (check-equal? (css-comment-text top-comment) "/* top */")
  (define comment-rule
    (cadr (css-stylesheet-rules comment-stylesheet)))
  (check-true (css-style-rule? comment-rule))
  (define inner-comment
    (car (css-style-rule-block comment-rule)))
  (check-true (css-comment? inner-comment))
  (check-equal? (css-comment-text inner-comment) "/* inner */")
  (define hacked-rule
    (car (css-stylesheet-rules hacked-declaration-stylesheet)))
  (check-equal?
   (map css-declaration-name
        (filter css-declaration?
                (css-style-rule-block hacked-rule)))
   '("*zoom" "_width" "color"))
  (define recovered-rule
    (car (css-stylesheet-rules recovered-stylesheet)))
  (check-equal?
   (map css-declaration-name
        (filter css-declaration?
                (css-style-rule-block recovered-rule)))
   '("content" "color"))
  (check-true
   (ormap css-recovery?
          (css-style-rule-block recovered-rule)))

  (define supports-rule
    (car (css-stylesheet-rules supports-stylesheet)))
  (check-true (css-at-rule? supports-rule))
  (check-equal? (css-at-rule-name supports-rule) "@supports")
  (check-equal? (css-at-rule-prelude supports-rule) "(display: grid)")
  (check-equal? (length (css-at-rule-block supports-rule)) 1)

  (define import-rule
    (car (css-stylesheet-rules import-stylesheet)))
  (check-true (css-at-rule? import-rule))
  (check-equal? (css-at-rule-name import-rule) "@import")
  (check-equal? (css-at-rule-prelude import-rule) "url(\"x.css\") screen")
  (check-false (css-at-rule-block import-rule))

  (define font-face-rule
    (car (css-stylesheet-rules font-face-stylesheet)))
  (check-true (css-at-rule? font-face-rule))
  (check-equal? (css-at-rule-name font-face-rule) "@font-face")
  (check-equal? (length (css-at-rule-block font-face-rule)) 2)

  (define page-rule
    (car (css-stylesheet-rules page-stylesheet)))
  (check-true (css-at-rule? page-rule))
  (define page-margin-rule
    (car (css-at-rule-block page-rule)))
  (check-true (css-at-rule? page-margin-rule))
  (check-equal? (css-at-rule-name page-margin-rule) "@bottom-right")

  (define keyframes-rule
    (car (css-stylesheet-rules keyframes-stylesheet)))
  (check-true (css-at-rule? keyframes-rule))
  (check-equal? (css-at-rule-name keyframes-rule) "@keyframes")
  (check-equal? (css-at-rule-prelude keyframes-rule) "fade")
  (check-equal? (length (css-at-rule-block keyframes-rule)) 2)
  (define first-keyframe
    (car (css-at-rule-block keyframes-rule)))
  (check-true (css-style-rule? first-keyframe))
  (check-equal? (css-style-rule-raw-selector first-keyframe) "from")

  (define recovered-error-stylesheet
    (parse-css-structurally "body { color red; width: 10px; }"))
  (define recovered-statement-stylesheet
    (parse-css-structurally "body { color: red; }\n/"))
  (define recovered-block-stylesheet
    (parse-css-structurally "body { color: red;"))
  (define crlf-stylesheet
    (parse-css-structurally "* {\r\n  margin: 0;\r\n  padding: 0;\r\n}"))
  (check-equal?
   (map css-declaration-name
        (filter css-declaration?
                (css-style-rule-block (car (css-stylesheet-rules recovered-error-stylesheet)))))
   '("width"))
  (check-true
   (ormap css-recovery?
          (css-style-rule-block (car (css-stylesheet-rules recovered-error-stylesheet)))))
  (check-equal?
   (map css-style-rule-raw-selector
        (filter css-style-rule?
                (css-stylesheet-rules recovered-statement-stylesheet)))
   '("body"))
  (define crlf-rule
    (car (css-stylesheet-rules crlf-stylesheet)))
  (check-equal?
   (map css-declaration-name
        (filter css-declaration?
                (css-style-rule-block crlf-rule)))
   '("margin" "padding"))
  (check-equal?
   (map css-declaration-value
        (filter css-declaration?
                (css-style-rule-block crlf-rule)))
   '("0" "0"))
  (check-equal?
   (map css-style-rule-raw-selector
        (filter css-style-rule?
                (css-stylesheet-rules recovered-block-stylesheet)))
   '()))
