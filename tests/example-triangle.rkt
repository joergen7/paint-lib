#lang racket/base

(module+ test
  (require
   racket/class
   racket/math
   rackunit
   "../syntax-ext.rkt")

  (define (polygon n)
    (repeat (n)
      (forward 1)
      (turn (* -2/3 pi))))

  (define/turtle triangle
    [base
     (polygon 3)]
    [inductive
     (repeat (3)
       (resize 1/2)
       (triangle)
       (resize 2)
       (move 1)
       (turn (* -2/3 pi)))])

  (define t
    (triangle (make-turtle) 3))

  (define image
    (make-image t))

  (send image get-bitmap 200))

 
