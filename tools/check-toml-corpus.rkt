#lang racket/base

;;;
;;; TOML Corpus Checker
;;;

;; Check TOML parser source preservation against an optional local corpus.

(require racket/file
         racket/path
         rackunit
         "../parsers-lib/parsers/toml.rkt")

;; corpus-root : path-string?
;;   Root directory for the optional local TOML corpus.
(define corpus-root "/private/tmp/lexers-toml-corpus")

;; main : -> void?
;;   Parse and source-round-trip each corpus file, or skip when unavailable.
(define (main)
  (cond
    [(not (directory-exists? corpus-root))
     (displayln "Skipping TOML corpus check: local corpus is unavailable.")]
    [else
     (define files
       (sort (for/list ([path (in-directory corpus-root)]
                        #:when (and (file-exists? path)
                                    (equal? (path-get-extension path) #".toml")))
               path)
             path<?))
     (for ([path (in-list files)])
       (define source (file->string path))
       (check-equal? (serialize-toml (parse-toml source)) source (path->string path)))
     (displayln (format "TOML corpus check passed for ~a files." (length files)))]))

(module+ main (main))
(module+ test (main))
