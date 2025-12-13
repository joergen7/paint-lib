#lang racket/base

(module+ test

  (require
   racket/class
   racket/draw
   racket/math
   rackunit
   "../syntax-ext.rkt")

  (define n
    5)

  (define t
    (for/turtle (n)
      (forward 1)
      (turn (/ (* 4 pi) n))))

  (define image
    (make-image t))

  ;; (get-bitmap image 200)
  
  (check-true
   (is-a? (get-bitmap image 200)
          bitmap%)))
