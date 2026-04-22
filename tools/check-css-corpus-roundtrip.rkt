#lang racket/base

;;;
;;; CSS Corpus Roundtrip Checker
;;;
;;
;; Parse a local CSS corpus, roundtrip it through the serializer, and report
;; whether normalized and source-preserving roundtrips succeed.

(require parsers/css
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/string)

;; default-corpus-dir : path-string?
;;   Default local corpus directory.
(define default-corpus-dir
  "/private/tmp/lexers-css-corpus")

;; main : -> void?
;;   Run the corpus roundtrip checker.
(define (main)
  (define mode 'both)
  (define corpus-dir
    (command-line
     #:program "check-css-corpus-roundtrip"
     #:once-each
     [("--normalized")
      "Check only normalized parse/serialize/parse roundtrips."
      (set! mode 'normalized)]
     [("--preserve-source")
      "Check only source-preserving exact roundtrips."
      (set! mode 'preserve-source)]
     #:args ([dir default-corpus-dir])
     dir))
  (cond
    [(not (directory-exists? corpus-dir))
     (printf "CSS corpus not found at ~a; skipping.~n" corpus-dir)]
    [else
     (check-corpus-roundtrip corpus-dir mode)]))

;; check-corpus-roundtrip : path-string? symbol? -> void?
;;   Roundtrip every CSS file in the corpus directory and print a summary.
(define (check-corpus-roundtrip corpus-dir mode)
  (define paths
    (sort (find-css-files corpus-dir)
          string<?
          #:key path->string))
  (printf "Checking ~a CSS files in ~a~n" (length paths) corpus-dir)
  (define normalized-failures '())
  (define preserve-failures '())
  (for ([path (in-list paths)])
    (define-values (normalized-failure preserve-failure)
      (check-file-roundtrip path mode))
    (when normalized-failure
      (set! normalized-failures (cons normalized-failure normalized-failures)))
    (when preserve-failure
      (set! preserve-failures (cons preserve-failure preserve-failures))))
  (define normalized-failure-count
    (length normalized-failures))
  (define preserve-failure-count
    (length preserve-failures))
  (when (memq mode '(both normalized))
    (printf "Normalized roundtrip: ~a ok, ~a failed~n"
            (- (length paths) normalized-failure-count)
            normalized-failure-count))
  (when (memq mode '(both preserve-source))
    (printf "Preserve-source roundtrip: ~a ok, ~a failed~n"
            (- (length paths) preserve-failure-count)
            preserve-failure-count))
  (report-failures "NORMALIZED" (reverse normalized-failures))
  (report-failures "PRESERVE"   (reverse preserve-failures))
  (when (or (positive? normalized-failure-count)
            (positive? preserve-failure-count))
    (exit 1)))

;; report-failures : string? list? -> void?
;;   Print failure details for one roundtrip mode.
(define (report-failures label failures)
  (for ([failure (in-list failures)])
    (match-define (list path message) failure)
    (printf "~a ~a: ~a~n" label path message)))

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

;; check-file-roundtrip : path? symbol? -> values
;;   Check one CSS file in one or both roundtrip modes.
(define (check-file-roundtrip path mode)
  (with-handlers ([exn:fail:css?
                   (lambda (e)
                     (define failure
                       (list path (exn-message e)))
                     (values (and (memq mode '(both normalized)) failure)
                             (and (memq mode '(both preserve-source)) failure)))]
                  [exn:fail?
                   (lambda (e)
                     (define failure
                       (list path (format "unexpected failure: ~a"
                                          (exn-message e))))
                     (values (and (memq mode '(both normalized)) failure)
                             (and (memq mode '(both preserve-source)) failure)))])
    (define source
      (file->string path))
    (define stylesheet
      (parse-css source))
    (define normalized-failure
      (and (memq mode '(both normalized))
           (check-normalized-roundtrip stylesheet)))
    (define preserve-failure
      (and (memq mode '(both preserve-source))
           (check-preserve-source-roundtrip source stylesheet)))
    (values (and normalized-failure
                 (list path normalized-failure))
            (and preserve-failure
                 (list path preserve-failure)))))

;; check-normalized-roundtrip : css-stylesheet? -> (or/c #f string?)
;;   Check parse -> normalized serialize -> parse -> normalized serialize.
(define (check-normalized-roundtrip stylesheet)
  (define normalized-1
    (serialize-stylesheet/normalized stylesheet))
  (define normalized-2
    (serialize-stylesheet/normalized
     (parse-css normalized-1)))
  (and (not (string=? normalized-1 normalized-2))
       (string-append "normalized roundtrip changed output from "
                      (~s normalized-1)
                      " to "
                      (~s normalized-2))))

;; check-preserve-source-roundtrip : string? css-stylesheet? -> (or/c #f string?)
;;   Check parse -> preserve-source serialize is byte-for-byte identical.
(define (check-preserve-source-roundtrip source stylesheet)
  (define preserved
    (serialize-stylesheet stylesheet #:preserve-source? #t))
  (and (not (string=? source preserved))
       (string-append "preserve-source changed output from "
                      (~s source)
                      " to "
                      (~s preserved))))

(module+ main
  (main))

(module+ test
  (require rackunit)

  (check-equal? (find-css-files "/definitely/missing") '())

  (define simple
    (parse-css "body { color: red; }"))

  (check-false (check-normalized-roundtrip simple))
  (check-false
   (check-preserve-source-roundtrip
    "body { color: red; }"
    simple)))
