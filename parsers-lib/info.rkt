#lang info

(define collection 'multi)
(define pkg-desc
  "Reusable parsers for CSS, TOML, Lua, and Scheme.")
(define pkg-authors '(soegaard))
(define license 'MIT)
(define deps '("base"
               "lexers-lib"
               "parser-tools-lib"))
(define build-deps '("rackunit-lib"))
