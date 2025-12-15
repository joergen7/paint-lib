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
 racket/math
 "turtle.rkt")

(provide
 abstract-turtle%)

(define abstract-turtle%
  (class* object% (turtle<%>)
    (super-new)

    (abstract
     get-x
     get-y
     get-face
     get-step-size
     get-min-x
     get-max-x
     get-min-y
     get-max-y
     set-face
     set-pos
     set-step-size
     set-min-x
     set-max-x
     set-min-y
     set-max-y)

    (define/public (get-width)
      (- (get-max-x) (get-min-x)))

    (define/public (get-height)
      (- (get-max-y) (get-min-y)))

    (define/public (forward distance)
      (move distance))

    (define/public (move distance)
      (define factor
        (* distance (get-step-size)))
      (define x1
        (+ (get-x)
           (* factor
              (cos (get-face)))))
      (define y1
        (+ (get-y)
           (* factor
              (sin (get-face)))))
      (set-pos x1 y1)
      (set-min-x (min (get-min-x) x1))
      (set-max-x (max (get-max-x) x1))
      (set-min-y (min (get-min-y) y1))
      (set-max-y (max (get-max-y) y1)))

    (define/public (resize factor)
      (set-step-size (* factor (get-step-size))))

    (define/public (turn angle)
      (set-face (+ (get-face) angle)))

    (define/public (label text)
      (define face0
        (get-face))
      (set-face (* 1/2 pi))
      (move 1/2)
      (turn (* -1/2 pi))
      (move (* 0.6 (string-length text)))
      (turn (* -1/2 pi))
      (move 1)
      (turn (* -1/2 pi))
      (move (* 0.6 (string-length text)))
      (turn (* -1/2 pi))
      (move 1/2)
      (set-face face0))))
      
