#lang racket/base

(module+ test

  (require
   racket/class
   racket/math
   rackunit
   "../abstract-turtle.rkt")

  (define p0
    (new origin-turtle%))

  (check-= (send p0 get-x) 0.0 0.001)
  (check-= (send p0 get-y) 0.0 0.001)
  (check-= (send p0 get-face) 0.0 0.001)

  (define p1
    (send p0 forward 2.0))

  (check-= (send p1 get-x) 2.0 0.001)
  (check-= (send p1 get-y) 0.0 0.001)
  (check-= (send p1 get-face) 0.0 0.001)

  (define p2
    (send p1 turn (- (/ pi 2))))

  (check-= (send p2 get-x) 2.0 0.001)
  (check-= (send p2 get-y) 0.0 0.001)
  (check-= (send p2 get-face) (* 3/2 pi) 0.001)

  (define p3
    (send p2 forward 3.0))

  (check-= (send p3 get-x) 2.0 0.001)
  (check-= (send p3 get-y) -3.0 0.001)
  (check-= (send p3 get-face) (* 3/2 pi) 0.001)

  (define p4
    (send p3 turn (/ pi 4)))

  (check-= (send p4 get-x) 2.0 0.001)
  (check-= (send p4 get-y) -3.0 0.001)
  (check-= (send p4 get-face) (* 7/4 pi) 0.001)

  (define p5
    (send p4 forward (sqrt 2)))

  (check-= (send p5 get-x) 3.0 0.001)
  (check-= (send p5 get-y) -4.0 0.001)
  (check-= (send p5 get-face) (* 7/4 pi) 0.001)

  (define t1
    (new origin-turtle%))

  (check-= (send t1 get-x) 0 0.001)
  (check-= (send t1 get-y) 0 0.001)
  (check-= (send t1 get-step-size) 1 0.001)

  (define t2
    (send t1 resize 9/10))

  (check-= (send t2 get-x) 0 0.001)
  (check-= (send t2 get-y) 0 0.001)
  (check-= (send t2 get-step-size) 9/10 0.001)

  (define t3
    (send t2 forward 1))

  (check-= (send t3 get-x) 9/10 0.001)
  (check-= (send t3 get-y) 0 0.001)
  (check-= (send t3 get-step-size) 9/10 0.001)

  )

  
