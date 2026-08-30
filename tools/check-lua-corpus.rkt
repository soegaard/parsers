#lang racket/base

;;;
;;; Lua Corpus Checker
;;;

;; Check Lua parser source preservation against an optional local corpus.

(require racket/file
         racket/path
         rackunit
         "../parsers-lib/parsers/lua.rkt")

;; corpus-root : path-string?
;;   Root directory for the optional local Lua corpus.
(define corpus-root "/private/tmp/lexers-lua-corpus")

;; main : -> void?
;;   Parse and source-round-trip each corpus file, or skip when unavailable.
(define (main)
  (cond
    [(not (directory-exists? corpus-root))
     (displayln "Skipping Lua corpus check: local corpus is unavailable.")]
    [else
     (define files
       (sort (for/list ([path (in-directory corpus-root)]
                        #:when (and (file-exists? path)
                                    (equal? (path-get-extension path) #".lua")))
               path)
             path<?))
     (for ([path (in-list files)])
       (define source (file->string path))
       (check-equal? (serialize-lua (parse-lua source)) source (path->string path)))
     (displayln (format "Lua corpus check passed for ~a files." (length files)))]))

(module+ main (main))
(module+ test (main))
