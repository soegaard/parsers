#lang info

(define collection 'multi)
(define pkg-desc
  "Reusable parsers. The initial public parser module is parsers/css.")
(define pkg-authors '(soegaard))
(define license 'MIT)
(define deps '("base"
               "lexers-lib"
               "parser-tools-lib"))
(define build-deps '("rackunit-lib"))
