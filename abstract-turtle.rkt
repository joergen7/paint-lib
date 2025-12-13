;; Copyright 2025 Jörgen Brandt
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

#lang racket/base

(require
 racket/class
 racket/contract
 racket/math
 "turtle.rkt")

(provide
 abstract-turtle%
 origin-turtle%
 face-turtle%
 turn-turtle%
 forward-turtle%
 move-turtle%
 write-turtle%
 normalize-angle)

(define abstract-turtle%
  (class* object% (turtle<%>)
    (super-new)

    (abstract
     dump
     get-x
     get-y
     get-min-x
     get-max-x
     get-min-y
     get-max-y
     get-face
     transpose
     scale)

    (define/public (hatch)
      (new origin-turtle%
           [x    (get-x)]
           [y    (get-y)]
           [face (get-face)]))

    (define/public (face angle)
      (new face-turtle%
           [parent this]
           [angle  angle]))

    (define/public (turn angle)
      (new turn-turtle%
           [parent this]
           [angle  angle]))

    (define/public (forward distance)
      (new forward-turtle%
           [parent   this]
           [distance distance]))

    (define/public (move distance)
      (new move-turtle%
           [parent   this]
           [distance distance]))

    (define/public (write text)
      (new write-turtle%
           [parent this]
           [text   text]))))

(define origin-turtle%
  (class abstract-turtle%
    (super-new)

    (init-field
     [x    0.0]
     [y    0.0]
     [face 0.0])

    (define/override (dump dc)
      (void))
      
    (define/override (get-x)
      x)
    
    (define/override (get-y)
      y)

    (define/override (get-min-x)
      x)

    (define/override (get-max-x)
      x)

    (define/override (get-min-y)
      y)

    (define/override (get-max-y)
      y)

    (define/override (get-face)
      face)

    (define/override (transpose x y)
      (new origin-turtle%
           [x    (+ x (get-x))]
           [y    (+ y (get-y))]
           [face (get-face)]))

    (define/override (scale factor)
      (new origin-turtle%
           [x    (* factor (get-x))]
           [y    (* factor (get-y))]
           [face (get-face)]))))


(define face-turtle%
  (class abstract-turtle%
    (super-new)

    (init-field
     parent
     face)

    (define/override (dump dc)
      (send parent dump dc))

    (define/override (get-x)
      (send parent get-x))

    (define/override (get-y)
      (send parent get-y))

    (define/override (get-min-x)
      (send parent get-min-x))

    (define/override (get-max-x)
      (send parent get-max-x))

    (define/override (get-min-y)
      (send parent get-min-y))

    (define/override (get-max-y)
      (send parent get-max-y))

    (define/override (get-face)
      face)

    (define/override (transpose x y)
      (new face-turtle%
           [parent (send parent transpose x y)]
           [face   face]))

    (define/override (scale factor)
      (new face-turtle%
           [parent (send parent scale factor)]
           [face   face]))))

(define turn-turtle%
  (class abstract-turtle%
    (super-new)

    (init-field
     parent
     angle)

    (define/override (dump dc)
      (send parent dump dc))

    (define/override (get-x)
      (send parent get-x))

    (define/override (get-y)
      (send parent get-y))

    (define/override (get-min-x)
      (send parent get-min-x))

    (define/override (get-max-x)
      (send parent get-max-x))

    (define/override (get-min-y)
      (send parent get-min-y))

    (define/override (get-max-y)
      (send parent get-max-y))

    (define/override (get-face)
      (normalize-angle
       (+ angle (send parent get-face))))

    (define/override (transpose x y)
      (new turn-turtle%
           [parent (send parent transpose x y)]
           [angle   angle]))

    (define/override (scale factor)
      (new turn-turtle%
           [parent (send parent scale factor)]
           [angle  angle]))))
    

(define forward-turtle%
  (class abstract-turtle%
    (super-new)

    (init-field
     parent
     distance)

    (define/override (dump dc)
      (send dc
            draw-line
            (inexact->exact (round (send parent get-x)))
            (inexact->exact (round (send parent get-y)))
            (inexact->exact (round (get-x)))
            (inexact->exact (round (get-y))))
      (send parent dump dc))


    (define/override (get-x)
      (+ (send parent get-x)
         (* distance (cos (get-face)))))

    (define/override (get-y)
      (+ (send parent get-y)
         (* distance (sin (get-face)))))

    (define/override (get-min-x)
      (min (send parent get-min-x)
           (get-x)))

    (define/override (get-max-x)
      (max (send parent get-max-x)
           (get-x)))

    (define/override (get-min-y)
      (min (send parent get-min-y)
           (get-y)))

    (define/override (get-max-y)
      (max (send parent get-max-y)
           (get-y)))

    (define/override (get-face)
      (send parent get-face))

    (define/override (transpose x y)
      (new forward-turtle%
           [parent   (send parent transpose x y)]
           [distance distance]))

    (define/override (scale factor)
      (new forward-turtle%
           [parent   (send parent scale factor)]
           [distance (* factor distance)]))))

(define move-turtle%
  (class abstract-turtle%
    (super-new)

    (init-field
     parent
     distance)

    (define/override (dump dc)
      (send parent dump dc))


    (define/override (get-x)
      (+ (send parent get-x)
         (* distance (cos (get-face)))))

    (define/override (get-y)
      (+ (send parent get-y)
         (* distance (sin (get-face)))))

    (define/override (get-min-x)
      (min (send parent get-min-x)
           (get-x)))

    (define/override (get-max-x)
      (max (send parent get-max-x)
           (get-x)))

    (define/override (get-min-y)
      (min (send parent get-min-y)
           (get-y)))

    (define/override (get-max-y)
      (max (send parent get-max-y)
           (get-y)))

    (define/override (get-face)
      (send parent get-face))

    (define/override (transpose x y)
      (new move-turtle%
           [parent   (send parent transpose x y)]
           [distance distance]))

    (define/override (scale factor)
      (new move-turtle%
           [parent   (send parent scale factor)]
           [distance (* factor distance)]))))

(define write-turtle%
  (class abstract-turtle%
    (super-new)

    (init-field
     parent
     text)

    (define/override (dump dc)
      (send dc
            draw-text
            (inexact->exact (round (get-x)))
            (inexact->exact (round (get-y)))))

    (define/override (get-x)
      (send parent get-x))
      
    (define/override (get-y)
      (send parent get-y))

    (define/override (get-min-x)
      (send parent get-min-x))

    (define/override (get-max-x)
      (send parent get-max-x))

    (define/override (get-min-y)
      (send parent get-min-y))

    (define/override (get-max-y)
      (send parent get-max-y))

    (define/override (get-face)
      (send parent get-face))

    (define/override (transpose x y)
      (new write-turtle%
           [parent (send parent transpose x y)]
           [text   text]))

    (define/override (scale factor)
      (new write-turtle%
           [parent (send parent scale factor)]
           [text   text]))))
                   
(define/contract (normalize-angle angle)
  (-> rational? rational?)
  (cond
    [(negative? angle)
     (normalize-angle (+ angle (* 2 pi)))]
    [(> angle (* 2 pi))
     (normalize-angle (- angle (* 2 pi)))]
    [else
     angle]))
      
