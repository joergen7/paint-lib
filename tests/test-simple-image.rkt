#lang racket/base


(module+ test
  
  (require
   racket/class
   racket/stream
   racket/math
   racket/draw
   rackunit
   "../syntax-ext.rkt")

  (define p
    (with-turtle ()
      (forward 5)
      (turn (* -1/2 pi))
      (forward 5)
      (turn (* -1/4 pi))
      (forward (* 5/2 (sqrt 2)))
      (turn (* -1/2 pi))
      (forward (* 5/2 (sqrt 2)))
      (turn (* -1/4 pi))
      (forward 5)
      (turn (* -3/4 pi))
      (forward (* 5 (sqrt 2)))
      (turn (* -3/4 pi))
      (forward 5)
      (turn (* -3/4 pi))
      (forward (* 5 (sqrt 2)))))

  (define image
    (make-image p))

  (check-true
   (is-a? (get-bitmap image 400) bitmap%)))



  



