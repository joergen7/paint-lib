#lang info
(define collection "paint-lib")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/paint-lib.scrbl" ())))
(define pkg-desc "A very simple paint library.")
(define version "0.0")
(define pkg-authors '(joergen))
(define license 'Apache-2.0)
