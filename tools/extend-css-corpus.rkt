#lang racket/base

;;;
;;; CSS Corpus Expander
;;;
;;
;; Build a larger CSS corpus from a seed directory and local source roots.

(require openssl/sha1
         racket/file
         racket/format
         racket/list
         racket/path
         racket/string)

;; default-seed-dir : path-string?
;;   Default seed corpus directory.
(define default-seed-dir
  "/private/tmp/lexers-css-corpus")

;; default-output-dir : path-string?
;;   Default expanded corpus directory.
(define default-output-dir
  "/private/tmp/lexers-css-corpus-1000")

;; default-source-roots : (listof path-string?)
;;   Default roots used to harvest CSS files.
(define default-source-roots
  (list ".."))

;; main : -> void?
;;   Expand the CSS corpus according to the command-line arguments.
(define (main)
  (define-values (seed-dir output-dir target-size source-roots)
    (parse-arguments))
  (expand-css-corpus seed-dir
                     output-dir
                     target-size
                     source-roots))

;; expand-css-corpus : path-string? path-string? exact-positive-integer? list? -> void?
;;   Build an expanded corpus from a seed directory and local source roots.
(define (expand-css-corpus seed-dir output-dir target-size source-roots)
  (unless (directory-exists? seed-dir)
    (raise-arguments-error 'expand-css-corpus
                           "seed corpus directory does not exist"
                           "seed-dir"
                           seed-dir))
  (make-directory* output-dir)
  (define seed-files
    (sort (find-css-files seed-dir)
          string<?
          #:key path->string))
  (define width
    (string-length (number->string target-size)))
  (define seen-hashes
    (make-hash))
  (define copied-count
    (copy-seed-files! seed-files output-dir width seen-hashes target-size))
  (define added-count
    (fill-from-sources! output-dir
                        width
                        copied-count
                        target-size
                        source-roots
                        seen-hashes))
  (printf "Seeded ~a files and added ~a files into ~a~n"
          copied-count
          added-count
          output-dir)
  (printf "Final corpus size: ~a~n" (+ copied-count added-count)))

;; parse-arguments : -> values
;;   Parse simple command-line arguments.
(define (parse-arguments)
  (define args
    (vector->list (current-command-line-arguments)))
  (define target-size
    (cond
      [(null? args) 1000]
      [else
       (or (string->number (car args))
           (raise-arguments-error 'extend-css-corpus
                                  "expected numeric target size as first argument"
                                  "argument"
                                  (car args)))]))
  (define seed-dir
    (if (>= (length args) 2)
        (cadr args)
        default-seed-dir))
  (define output-dir
    (if (>= (length args) 3)
        (caddr args)
        default-output-dir))
  (define source-roots
    (if (>= (length args) 4)
        (cdddr args)
        default-source-roots))
  (values seed-dir output-dir target-size source-roots))

;; copy-seed-files! : list? path-string? exact-positive-integer? hash? exact-positive-integer? -> exact-nonnegative-integer?
;;   Copy seed files into the output corpus while deduplicating by content.
(define (copy-seed-files! seed-files output-dir width seen-hashes target-size)
  (let loop ([remaining seed-files]
             [index 1]
             [copied 0])
    (cond
      [(or (null? remaining)
           (> index target-size))
       copied]
      [else
       (define path
         (car remaining))
       (define hash-value
         (file-sha1 path))
       (cond
         [(hash-has-key? seen-hashes hash-value)
          (loop (cdr remaining) index copied)]
         [else
          (hash-set! seen-hashes hash-value #t)
          (copy-file path
                     (build-path output-dir
                                 (corpus-file-name index width (file-name-from-path path)))
                     #t)
          (loop (cdr remaining)
                (add1 index)
                (add1 copied))])])))

;; fill-from-sources! : path-string? exact-positive-integer? exact-nonnegative-integer? exact-positive-integer? list? hash? -> exact-nonnegative-integer?
;;   Fill the output corpus from candidate source roots.
(define (fill-from-sources! output-dir width start-count target-size source-roots seen-hashes)
  (let loop-roots ([roots source-roots]
                   [index (add1 start-count)]
                   [added 0])
    (cond
      [(or (null? roots)
           (> index target-size))
       added]
      [else
       (define-values (next-index next-added)
         (fill-from-root! (car roots)
                          output-dir
                          width
                          index
                          added
                          target-size
                          seen-hashes))
       (loop-roots (cdr roots) next-index next-added)])))

;; fill-from-root! : path-string? path-string? exact-positive-integer? exact-positive-integer? exact-nonnegative-integer? exact-positive-integer? hash? -> values
;;   Fill the output corpus from one root.
(define (fill-from-root! root output-dir width start-index added target-size seen-hashes)
  (define paths
    (sort (find-css-files root)
          string<?
          #:key path->string))
  (let loop ([remaining paths]
             [index start-index]
             [count added])
    (cond
      [(or (null? remaining)
           (> index target-size))
       (values index count)]
      [else
       (define path
         (car remaining))
       (define hash-value
         (file-sha1 path))
       (cond
         [(hash-has-key? seen-hashes hash-value)
          (loop (cdr remaining) index count)]
         [else
          (hash-set! seen-hashes hash-value #t)
          (copy-file path
                     (build-path output-dir
                                 (corpus-file-name index width (file-name-from-path path)))
                     #t)
          (loop (cdr remaining)
                (add1 index)
                (add1 count))])])))

;; find-css-files : path-string? -> list?
;;   Find CSS files under a root.
(define (find-css-files root)
  (cond
    [(not (directory-exists? root))
     '()]
    [else
     (for/list ([path (in-directory root)]
                #:when (and (file-exists? path)
                            (string-suffix? (string-downcase (path->string path)) ".css")))
       path)]))

;; file-sha1 : path-string? -> string?
;;   Compute a SHA-1 digest for a file's contents.
(define (file-sha1 path)
  (call-with-input-file path
    (lambda (in)
      (sha1 in))))

;; corpus-file-name : exact-positive-integer? exact-positive-integer? path? -> string?
;;   Build one numbered corpus file name.
(define (corpus-file-name index width original-name)
  (define suffix
    (path->string original-name))
  (format "~a-~a"
          (~a index #:min-width width #:align 'right #:pad-string "0")
          suffix))

(module+ main
  (main))

(module+ test
  (require rackunit)

  (define seed-dir
    (make-temporary-file "seed-corpus~a" 'directory))
  (define root-dir
    (make-temporary-file "source-corpus~a" 'directory))
  (define output-dir
    (make-temporary-file "output-corpus~a" 'directory))

  (call-with-output-file (build-path seed-dir "001-a.css")
    (lambda (out)
      (display "body { color: red; }" out)))
  (call-with-output-file (build-path root-dir "b.css")
    (lambda (out)
      (display "body { color: red; }" out)))
  (call-with-output-file (build-path root-dir "c.css")
    (lambda (out)
      (display "body { color: blue; }" out)))

  (expand-css-corpus seed-dir output-dir 2 (list root-dir))
  (define generated-files
    (sort (find-css-files output-dir)
          string<?
          #:key path->string))
  (check-equal? (length generated-files) 2)
  (check-equal? (map (lambda (p) (path->string (file-name-from-path p))) generated-files)
                '("1-001-a.css" "2-c.css")))
