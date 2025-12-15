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
 "path.rkt"
 "null-turtle.rkt"
 "normalize-angle.rkt")

(provide
 abstract-path%
 origin-path%
 forward-path%
 move-path%
 turn-path%
 resize-path%)

(define abstract-path%
  (class* object% (path<%>)
    (super-new)

    (abstract
     guide
     transpose
     scale)

    (define/public (forward distance)
      (new forward-path%
           [parent   this]
           [distance distance]))

    (define/public (move distance)
      (new move-path%
           [parent   this]
           [distance distance]))

    (define/public (resize factor)
      (new resize-path%
           [parent this]
           [factor factor]))

    (define/public (turn angle)
      (new turn-path%
           [parent this]
           [angle  (normalize-angle angle)]))

    (define/public (hatch)
      (define turtle
        (new null-turtle%))
      (guide turtle)
      (new origin-path%
           [x         (send turtle get-x)]
           [y         (send turtle get-y)]
           [face      (send turtle get-face)]
           [step-size (send turtle get-step-size)]))))


(define origin-path%
  (class abstract-path%
    (super-new)

    (init-field
     [x         0]
     [y         0]
     [face      0]
     [step-size 1])

    (define/override (guide turtle)
      (send turtle set-pos x y)
      (send turtle set-face face)
      (send turtle set-step-size step-size)
      (send turtle set-min x y)
      (send turtle set-max x y))


    (define/override (transpose dx dy)
      (new origin-path%
           [x         (+ x dx)]
           [y         (+ y dy)]
           [face      face]
           [step-size step-size]))

    (define/override (scale factor)
      (new origin-path%
           [x         (* factor x)]
           [y         (* factor y)]
           [face      face]
           [step-size (* factor step-size)]))))

(define forward-path%
  (class abstract-path%
    (super-new)

    (init-field
     parent
     distance)

    (define/override (guide turtle)
      (send parent guide turtle)
      (send turtle forward distance))

    (define/override (transpose dx dy)
      (new forward-path%
           [parent   (send parent transpose dx dy)]
           [distance distance]))

    (define/override (scale factor)
      (new forward-path%
           [parent   (send parent scale factor)]
           [distance distance]))))

(define move-path%
  (class abstract-path%
    (super-new)

    (init-field
     parent
     distance)

    (define/override (guide turtle)
      (send parent guide turtle)
      (send turtle move distance))

    (define/override (transpose dx dy)
      (new move-path%
           [parent   (send parent transpose dx dy)]
           [distance distance]))

    (define/override (scale factor)
      (new move-path%
           [parent   (send parent scale factor)]
           [distance distance]))))

(define resize-path%
  (class abstract-path%
    (super-new)

    (init-field
     parent
     factor)

    (define/override (guide turtle)
      (send parent guide turtle)
      (send turtle resize factor))

    (define/override (transpose dx dy)
      (new resize-path%
           [parent (send parent transpose dx dy)]
           [factor factor]))

    (define/override (scale factor1)
      (new resize-path%
           [parent (send parent scale factor1)]
           [factor factor]))))

(define turn-path%
  (class abstract-path%
    (super-new)

    (init-field
     parent
     angle)

    (define/override (guide turtle)
      (send parent guide turtle)
      (send turtle turn angle))

    (define/override (transpose dx dy)
      (new turn-path%
           [parent (send parent transpose dx dy)]
           [angle  angle]))

    (define/override (scale factor)
      (new turn-path%
           [parent (send parent scale factor)]
           [angle  angle]))))

