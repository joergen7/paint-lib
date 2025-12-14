#lang racket/base

(module+ test

  (require
   racket/class
   racket/draw
   racket/math
   rackunit
   "../syntax-ext.rkt")

  (define n
    20)

  (define a
    (* 0.8 pi))

  (define s
    .95)

  (define t
    (with-turtle ()
      (repeat (n)
              (forward 1)
              (turn a)
              (resize s))))

  (define image
    (send (make-image t) fit-width 200))


  (get-bitmap image 200))

