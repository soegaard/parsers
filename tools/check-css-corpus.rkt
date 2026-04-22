#lang racket/base

;;;
;;; CSS Corpus Checker
;;;
;;
;; Parse a local CSS corpus and report successes and failures.

(require parsers/css
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/path
         racket/string)

;; default-corpus-dir : path-string?
;;   Default local corpus directory.
(define default-corpus-dir
  "/private/tmp/lexers-css-corpus")

;; main : -> void?
;;   Run the corpus checker.
(define (main)
  (define dir
    (let ([args (vector->list (current-command-line-arguments))])
      (cond
        [(null? args) default-corpus-dir]
        [(null? (cdr args)) (car args)]
        [else
         (raise-arguments-error 'check-css-corpus
                                "expected zero or one directory argument"
                                "arguments"
                                args)])))
  (define corpus-dir
    (if (string=? dir "")
        default-corpus-dir
        dir))
  (cond
    [(not (directory-exists? corpus-dir))
     (printf "CSS corpus not found at ~a; skipping.~n" corpus-dir)]
    [else
     (check-corpus corpus-dir)]))

;; check-corpus : path-string? -> void?
;;   Parse every CSS file in the corpus directory and print a summary.
(define (check-corpus corpus-dir)
  (define paths
    (sort (find-css-files corpus-dir)
          string<?
          #:key path->string))
  (printf "Checking ~a CSS files in ~a~n" (length paths) corpus-dir)
  (define failures
    (for/list ([path (in-list paths)]
               #:when #t)
      (and (not (parse-file-ok? path))
           path)))
  (define actual-failures
    (filter values failures))
  (printf "Parsed: ~a ok, ~a failed~n"
          (- (length paths) (length actual-failures))
          (length actual-failures))
  (for ([path (in-list actual-failures)])
    (printf "FAIL ~a~n" path))
  (when (pair? actual-failures)
    (exit 1)))

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

;; parse-file-ok? : path? -> boolean?
;;   Parse one CSS file and report failures.
(define (parse-file-ok? path)
  (with-handlers ([exn:fail:css?
                   (lambda (e)
                     (printf "~a: ~a~n" path (exn-message e))
                     #f)]
                  [exn:fail?
                   (lambda (e)
                     (printf "~a: unexpected failure: ~a~n" path (exn-message e))
                     #f)])
    (call-with-input-file path
      (lambda (in)
        (parse-css in)
        #t))))

(module+ main
  (main))

(module+ test
  (require rackunit)

  (check-equal? (find-css-files "/definitely/missing") '()))
