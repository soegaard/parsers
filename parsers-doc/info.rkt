#lang info

(define collection 'multi)
(define pkg-desc "Scribble documentation for the parsers library.")
(define pkg-authors '(soegaard))
(define license 'MIT)
(define deps '("base"
               "parsers-lib"
               "parser-tools-lib"
               "scribble-lib"))
(define build-deps '("parser-tools-doc"
                     "racket-doc"))
(define scribblings '(("parsers.scrbl" () (library))))
