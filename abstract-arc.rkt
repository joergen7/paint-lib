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
 "arc.rkt"
 "null-turtle.rkt"
 "normalize-angle.rkt")

(provide
 abstract-arc%
 origin-arc%
 forward-arc%
 move-arc%
 turn-arc%
 resize-arc%)

(define abstract-arc%
  (class* object% (arc<%>)
    (super-new)

    (abstract
     guide
     transpose
     scale)

    (define/public (forward distance)
      (new forward-arc%
           [parent   this]
           [distance distance]))

    (define/public (move distance)
      (new move-arc%
           [parent   this]
           [distance distance]))

    (define/public (resize factor)
      (new resize-arc%
           [parent this]
           [factor factor]))

    (define/public (turn angle)
      (new turn-arc%
           [parent this]
           [angle  (normalize-angle angle)]))

    (define/public (hatch)
      (define turtle
        (new null-turtle%))
      (guide turtle)
      (new origin-arc%
           [x         (send turtle get-x)]
           [y         (send turtle get-y)]
           [face      (send turtle get-face)]
           [step-size (send turtle get-step-size)]))))


(define origin-arc%
  (class abstract-arc%
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
      (new origin-arc%
           [x         (+ x dx)]
           [y         (+ y dy)]
           [face      face]
           [step-size step-size]))

    (define/override (scale factor)
      (new origin-arc%
           [x         (* factor x)]
           [y         (* factor y)]
           [face      face]
           [step-size (* factor step-size)]))))

(define forward-arc%
  (class abstract-arc%
    (super-new)

    (init-field
     parent
     distance)

    (define/override (guide turtle)
      (send parent guide turtle)
      (send turtle forward distance))

    (define/override (transpose dx dy)
      (new forward-arc%
           [parent   (send parent transpose dx dy)]
           [distance distance]))

    (define/override (scale factor)
      (new forward-arc%
           [parent   (send parent scale factor)]
           [distance distance]))))

(define move-arc%
  (class abstract-arc%
    (super-new)

    (init-field
     parent
     distance)

    (define/override (guide turtle)
      (send parent guide turtle)
      (send turtle move distance))

    (define/override (transpose dx dy)
      (new move-arc%
           [parent   (send parent transpose dx dy)]
           [distance distance]))

    (define/override (scale factor)
      (new move-arc%
           [parent   (send parent scale factor)]
           [distance distance]))))

(define resize-arc%
  (class abstract-arc%
    (super-new)

    (init-field
     parent
     factor)

    (define/override (guide turtle)
      (send parent guide turtle)
      (send turtle resize factor))

    (define/override (transpose dx dy)
      (new resize-arc%
           [parent (send parent transpose dx dy)]
           [factor factor]))

    (define/override (scale factor1)
      (new resize-arc%
           [parent (send parent scale factor1)]
           [factor factor]))))

(define turn-arc%
  (class abstract-arc%
    (super-new)

    (init-field
     parent
     angle)

    (define/override (guide turtle)
      (send parent guide turtle)
      (send turtle turn angle))

    (define/override (transpose dx dy)
      (new turn-arc%
           [parent (send parent transpose dx dy)]
           [angle  angle]))

    (define/override (scale factor)
      (new turn-arc%
           [parent (send parent scale factor)]
           [angle  angle]))))

