#lang racket/base

(module+ test

  (require
   racket/class
   racket/math
   rackunit
   "../abstract-paint.rkt")

  (define p0
    (new origin-paint%))

  (check-= (send p0 get-x) 0.0 0.001)
  (check-= (send p0 get-y) 0.0 0.001)
  (check-= (send p0 get-face) 0.0 0.001)

  (define p1
    (send p0 walk 2.0))

  (check-= (send p1 get-x) 2.0 0.001)
  (check-= (send p1 get-y) 0.0 0.001)
  (check-= (send p1 get-face) 0.0 0.001)

  (define p2
    (send p1 turn (- (/ pi 2))))

  (check-= (send p2 get-x) 2.0 0.001)
  (check-= (send p2 get-y) 0.0 0.001)
  (check-= (send p2 get-face) (* 3/2 pi) 0.001)

  (define p3
    (send p2 walk 3.0))

  (check-= (send p3 get-x) 2.0 0.001)
  (check-= (send p3 get-y) -3.0 0.001)
  (check-= (send p3 get-face) (* 3/2 pi) 0.001)

  (define p4
    (send p3 turn (/ pi 4)))

  (check-= (send p4 get-x) 2.0 0.001)
  (check-= (send p4 get-y) -3.0 0.001)
  (check-= (send p4 get-face) (* 7/4 pi) 0.001)

  (define p5
    (send p4 walk (sqrt 2)))

  (check-= (send p5 get-x) 3.0 0.001)
  (check-= (send p5 get-y) -4.0 0.001)
  (check-= (send p5 get-face) (* 7/4 pi) 0.001)

  )

  
