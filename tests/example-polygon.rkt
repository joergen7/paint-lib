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
    (let recur ([turtle (make-turtle)]
                [i      n])
      (cond
        [(zero? i)
         turtle]
        [else
         (recur
          (with-turtle (turtle)
            (forward 1)
            (turn (/ (* 2 pi) n)))
          (sub1 i))])))

  (define image
    (make-image t))

  ;; (get-bitmap image 200)

  (check-true
   (is-a? (get-bitmap image 200)
          bitmap%)))
       
