#lang racket/base

;;;
;;; CSS Corpus Computed-Style Checker
;;;
;;
;; Parse a local CSS corpus, sample exact selector groups from real files, and
;; verify that the computed-style helpers do not crash and satisfy basic
;; internal invariants.

(require parsers/css
         racket/cmdline
         racket/file
         racket/format
         racket/hash
         racket/list
         racket/match
         racket/path
         racket/string)

;; default-corpus-dir : path-string?
;;   Default local corpus directory.
(define default-corpus-dir
  "/private/tmp/lexers-css-corpus")

;; default-max-selector-groups-per-file : exact-positive-integer?
;;   Default number of exact selector groups to sample per file.
(define default-max-selector-groups-per-file
  8)

;; default-max-files : exact-positive-integer?
;;   Default number of corpus files to check in one run.
(define default-max-files
  10)

;; main : -> void?
;;   Run the computed-style corpus checker.
(define (main)
  (define max-selector-groups-per-file
    default-max-selector-groups-per-file)
  (define max-files
    default-max-files)
  (define memory-limit-mb
    #f)
  (define corpus-dir
    (command-line
     #:program "check-css-corpus-compute"
     #:once-each
     [("--max-files")
      count
      "Check at most this many CSS files in one run."
      (set! max-files
            (parse-positive-integer
             'check-css-corpus-compute
             "--max-files"
             count))]
     [("--max-selector-groups-per-file")
      count
      "Sample at most this many selector groups per file."
      (set! max-selector-groups-per-file
            (parse-positive-integer
             'check-css-corpus-compute
             "--max-selector-groups-per-file"
             count))]
     [("--memory-limit-mb")
      count
      "Limit this Racket process to approximately this many megabytes."
      (set! memory-limit-mb
            (parse-positive-integer
             'check-css-corpus-compute
             "--memory-limit-mb"
             count))]
     #:args ([dir default-corpus-dir])
     dir))
  (when memory-limit-mb
    (custodian-limit-memory (current-custodian)
                            (* memory-limit-mb 1024 1024)))
  (cond
    [(not (directory-exists? corpus-dir))
     (printf "CSS corpus not found at ~a; skipping.~n" corpus-dir)]
    [else
     (check-corpus-compute corpus-dir
                           max-files
                           max-selector-groups-per-file)]))

;; check-corpus-compute : path-string? exact-positive-integer? exact-positive-integer? -> void?
;;   Check computed-style invariants for sampled selector groups in the corpus.
(define (check-corpus-compute corpus-dir max-files max-selector-groups-per-file)
  (define all-paths
    (sort (find-css-files corpus-dir)
          string<?
          #:key path->string))
  (define paths
    (take all-paths
          (min max-files
               (length all-paths))))
  (printf "Checking computed styles for ~a CSS files in ~a~n"
          (length paths)
          corpus-dir)
  (flush-output)
  (define parse-failures '())
  (define compute-failures '())
  (define checked-selector-groups 0)
  (for ([path (in-list paths)])
    (define result
      (check-file-compute path max-selector-groups-per-file))
    (match result
      [(list 'parse-failure message)
       (set! parse-failures
             (cons (list path message) parse-failures))]
      [(list 'compute-failure selector-group message)
       (set! compute-failures
             (cons (list path selector-group message) compute-failures))]
      [(list 'ok count)
       (set! checked-selector-groups
             (+ checked-selector-groups count))]))
  (printf "Computed-style checks: ~a selector groups ok, ~a parse failures, ~a compute failures~n"
          checked-selector-groups
          (length parse-failures)
          (length compute-failures))
  (flush-output)
  (for ([failure (in-list (reverse parse-failures))])
    (match-define (list path message) failure)
    (printf "PARSE ~a: ~a~n" path message))
  (for ([failure (in-list (reverse compute-failures))])
    (match-define (list path selector-group message) failure)
    (printf "COMPUTE ~a ~s: ~a~n" path selector-group message))
  (flush-output)
  (when (or (pair? parse-failures)
            (pair? compute-failures))
    (exit 1)))

;; check-file-compute : path? exact-positive-integer? -> list?
;;   Check computed-style invariants for one file.
(define (check-file-compute path max-selector-groups-per-file)
  (with-handlers ([exn:fail:css?
                   (lambda (e)
                     (list 'parse-failure (exn-message e)))]
                  [exn:fail?
                   (lambda (e)
                     (list 'parse-failure
                           (format "unexpected failure: ~a"
                                   (exn-message e))))])
    (define stylesheet
      (call-with-input-file path parse-css))
    (define all-selector-groups
      (unique-selector-groups stylesheet))
    (define selector-groups
      (take all-selector-groups
            (min max-selector-groups-per-file
                 (length all-selector-groups))))
    (let loop ([remaining selector-groups]
               [count 0])
      (cond
        [(null? remaining)
         (list 'ok count)]
        [else
         (define result
           (check-selector-group-compute stylesheet (car remaining)))
         (match result
           [(list 'ok)
            (loop (cdr remaining) (add1 count))]
           [(list 'failure message)
            (list 'compute-failure (car remaining) message)])]))))

;; check-selector-group-compute : css-stylesheet? string? -> list?
;;   Check computed-style invariants for one exact selector-group target.
(define (check-selector-group-compute stylesheet selector-group)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (list 'failure (exn-message e)))])
    (define style
      (css-compute-style-for-selector-group stylesheet selector-group))
    (define custom-properties
      (css-compute-custom-properties-for-selector-group stylesheet selector-group))
    (define resolved-style
      (css-compute-style-for-selector-group stylesheet
                                            selector-group
                                            #:resolve-vars? #t))
    (define resolved-custom-properties
      (css-compute-custom-properties-for-selector-group stylesheet
                                                        selector-group
                                                        #:resolve-vars? #t))
    (define-values (traced-style trace)
      (css-compute-style-for-selector-group stylesheet
                                            selector-group
                                            #:resolve-vars? #t
                                            #:trace? #t))
    (define-values (traced-custom-properties custom-trace)
      (css-compute-custom-properties-for-selector-group stylesheet
                                                        selector-group
                                                        #:resolve-vars? #t
                                                        #:trace? #t))
    (check-hash-result 'style style)
    (check-hash-result 'custom-properties custom-properties)
    (check-hash-result 'resolved-style resolved-style)
    (check-hash-result 'resolved-custom-properties resolved-custom-properties)
    (check-hash-result 'traced-style traced-style)
    (check-hash-result 'traced-custom-properties traced-custom-properties)
    (check-trace-result trace selector-group traced-style traced-custom-properties)
    (check-trace-result custom-trace selector-group traced-style traced-custom-properties)
    (list 'ok)))

;; check-hash-result : symbol? any/c -> void?
;;   Ensure a computed-style result is a hash.
(define (check-hash-result label result)
  (unless (hash? result)
    (raise-arguments-error 'check-css-corpus-compute
                           "expected hash result"
                           "label" label
                           "result" result)))

;; check-trace-result : any/c string? hash? hash? -> void?
;;   Verify basic internal invariants for one trace payload.
(define (check-trace-result trace selector-group style-hash custom-properties-hash)
  (unless (css-compute-style-trace? trace)
    (raise-arguments-error 'check-css-corpus-compute
                           "expected css-compute-style-trace?"
                           "trace" trace))
  (unless (string=? (css-compute-style-trace-selector-group trace)
                    selector-group)
    (raise-arguments-error 'check-css-corpus-compute
                           "trace selector-group mismatch"
                           "expected" selector-group
                           "actual" (css-compute-style-trace-selector-group trace)))
  (for ([result (in-list (css-compute-style-trace-property-results trace))])
    (check-property-result-in-hash result style-hash))
  (for ([result (in-list (css-compute-style-trace-custom-property-results trace))])
    (check-property-result-in-hash result custom-properties-hash))
  (for ([matched-rule (in-list (css-compute-style-trace-matched-rules trace))])
    (unless (css-compute-matched-rule? matched-rule)
      (raise-arguments-error 'check-css-corpus-compute
                             "expected css-compute-matched-rule?"
                             "matched-rule" matched-rule)))
  (for ([resolution (in-list (css-compute-style-trace-var-resolutions trace))])
    (unless (css-compute-var-resolution? resolution)
      (raise-arguments-error 'check-css-corpus-compute
                             "expected css-compute-var-resolution?"
                             "resolution" resolution))))

;; check-property-result-in-hash : css-compute-property-result? hash? -> void?
;;   Ensure one property result is reflected in the computed hash.
(define (check-property-result-in-hash result computed-hash)
  (unless (css-compute-property-result? result)
    (raise-arguments-error 'check-css-corpus-compute
                           "expected css-compute-property-result?"
                           "result" result))
  (define name
    (css-compute-property-result-name result))
  (define winner
    (css-compute-property-result-winner result))
  (unless (css-compute-candidate? winner)
    (raise-arguments-error 'check-css-corpus-compute
                           "expected css-compute-candidate? winner"
                           "winner" winner))
  (unless (member winner (css-compute-property-result-candidates result))
    (raise-arguments-error 'check-css-corpus-compute
                           "winner is not among listed candidates"
                           "property" name))
  (unless (hash-has-key? computed-hash name)
    (raise-arguments-error 'check-css-corpus-compute
                           "missing property in computed hash"
                           "property" name))
  (unless (string=? (hash-ref computed-hash name)
                    (css-compute-candidate-value winner))
    (raise-arguments-error 'check-css-corpus-compute
                           "computed hash disagrees with winning candidate"
                           "property" name
                           "hash-value" (hash-ref computed-hash name)
                           "winner-value" (css-compute-candidate-value winner))))

;; unique-selector-groups : css-stylesheet? -> list?
;;   Collect unique exact selector groups from flattened style rules in source order.
(define (unique-selector-groups stylesheet)
  (reverse
   (for/fold ([seen (hash)]
              [groups '()]
              #:result groups)
             ([node (in-list (css-flatten-rules stylesheet))]
              #:when (css-style-rule? node))
     (for/fold ([next-seen seen]
                [next-groups groups])
               ([selector-group (in-list (css-style-rule-selector-groups node))])
       (cond
         [(hash-has-key? next-seen selector-group)
          (values next-seen next-groups)]
         [else
          (values (hash-set next-seen selector-group #t)
                  (cons selector-group next-groups))])))))

;; find-css-files : path-string? -> list?
;;   Find CSS files below a directory.
(define (find-css-files corpus-dir)
  (cond
    [(not (directory-exists? corpus-dir))
     '()]
    [else
     (for/list ([path (in-directory corpus-dir)]
                #:when (and (file-exists? path)
                            (string-suffix? (path->string path) ".css")))
       path)]))

;; parse-positive-integer : symbol? string? string? -> exact-positive-integer?
;;   Parse a positive integer command-line argument.
(define (parse-positive-integer who option-name text)
  (define n
    (string->number text))
  (cond
    [(and (exact-integer? n)
          (positive? n))
     n]
    [else
     (raise-arguments-error who
                            "expected a positive integer"
                            "option" option-name
                            "value" text)]))

(module+ main
  (main))

(module+ test
  (require rackunit)

  (define stylesheet
    (parse-css
     ".a, .b { color: red; }\n@media screen { .b { --gap: 1rem; } }\n.c { color: blue; }"))

  (check-equal?
   (find-css-files "/definitely/missing")
   '())
  (check-equal?
   (unique-selector-groups stylesheet)
   '(".a" ".b" ".c"))
  (check-equal?
   (parse-positive-integer 'check-css-corpus-compute "--max-selector-groups-per-file" "3")
   3)
  (check-equal?
   (check-selector-group-compute stylesheet ".b")
   '(ok)))
