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
    (with-paint ()
      (walk 5)
      (turn (* -1/2 pi))
      (walk 5)
      (turn (* -1/4 pi))
      (walk (* 5/2 (sqrt 2)))
      (turn (* -1/2 pi))
      (walk (* 5/2 (sqrt 2)))
      (turn (* -1/4 pi))
      (walk 5)
      (turn (* -3/4 pi))
      (walk (* 5 (sqrt 2)))
      (turn (* -3/4 pi))
      (walk 5)
      (turn (* -3/4 pi))
      (walk (* 5 (sqrt 2)))))

  
  (define image
    (make-image p))

  (check-true
   (is-a? (get-bitmap image 400) bitmap%)))



  



