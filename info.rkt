#lang info

(define pkg-desc "Meta-package for the parsers library and documentation.")
(define pkg-authors '(soegaard))
(define license 'MIT)

(define deps '("base"
               "parsers-lib"
               "parsers-doc"
               "parser-tools-lib"))
(define build-deps '("parser-tools-doc"
                     "racket-doc"
                     "scribble-lib"))
