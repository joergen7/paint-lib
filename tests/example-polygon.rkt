#lang racket/base

(module+ test

  (require
   racket/class
   racket/draw
   racket/math
   rackunit
   "../syntax-ext.rkt")

  (define n
    10)

  (define t
    (with-turtle ()
      (repeat (n)
              (forward 1)
              (turn (/ (* 2 pi) n)))))

  (define image
    (make-image t))

  (get-bitmap image 200))

       
