#lang racket/base

;;;
;;; Scheme Corpus Checker
;;;

;; Check Scheme parser source preservation against an optional local corpus.

(require racket/file
         racket/path
         racket/list
         racket/string
         racket/cmdline
         rackunit
         "../parsers-lib/parsers/scheme.rkt")

;; corpus-root : path-string?
;;   Root directory for the optional local Scheme corpus.
(define corpus-root "/private/tmp/lexers-scheme-corpus")

;; memory-limit-mb : exact-positive-integer?
;;   Maximum memory available to one Scheme corpus-checking process.
(define memory-limit-mb 256)

;; selected-dialect : (or/c symbol? #f)
;;   Optional corpus dialect filter.
(define selected-dialect #f)

;; max-files : (or/c exact-positive-integer? #f)
;;   Optional deterministic cap for a bounded corpus chunk.
(define max-files #f)

;; start-index : exact-nonnegative-integer?
;;   Zero-based offset after dialect filtering for chunked corpus checks.
(define start-index 0)

(command-line
 #:once-each
 ["--memory-limit-mb" megabytes
                      "Maximum process memory in megabytes (default: 256)."
                      (set! memory-limit-mb (string->number megabytes))]
 ["--dialect" dialect
              "Check only one dialect directory."
              (set! selected-dialect (string->symbol dialect))]
 ["--max-files" count
                "Check at most this many files after dialect filtering."
                (set! max-files (string->number count))]
 ["--start-index" index
                  "Skip this many files after dialect filtering (default: 0)."
                  (set! start-index (string->number index))])

;; path-dialect : path? -> symbol?
;;   Determine the selected lexer dialect from a corpus subdirectory name.
(define (path-dialect path)
  (define pieces (string-split (path->string path) "/"))
  (string->symbol (list-ref (reverse pieces) 1)))

;; main : -> void?
;;   Parse and source-round-trip every corpus file, or skip when unavailable.
(define (main)
  (custodian-limit-memory (current-custodian) (* memory-limit-mb 1024 1024))
  (cond
    [(not (directory-exists? corpus-root))
     (displayln "Skipping Scheme corpus check: local corpus is unavailable.")]
    [else
     (define files
       (sort (for/list ([path (in-directory corpus-root)]
                        #:when (and (file-exists? path)
                                    (or (not selected-dialect)
                                        (eq? (path-dialect path) selected-dialect))))
               path)
             path<?))
     (define remaining-files (drop files (min start-index (length files))))
     (define checked-files
       (if max-files
           (take remaining-files (min max-files (length remaining-files)))
           remaining-files))
     (for ([path (in-list checked-files)])
       (define source (file->string path))
       (check-equal? (serialize-scheme (parse-scheme source #:dialect (path-dialect path)))
                     source
                     (path->string path)))
     (displayln (format "Scheme corpus check passed for ~a files." (length checked-files)))]))

(module+ main (main))
(module+ test (main))
