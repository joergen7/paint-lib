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
 racket/draw
 "null-turtle.rkt")

(provide
 dc-turtle%)

(define dc-turtle%
  (class null-turtle%
    (super-new)

    (init-field
     dc)

    (inherit
     get-x
     get-y
     get-step-size
     move)

    (define/override (forward distance)
      (define x0
        (get-x))
      (define y0
        (- (get-y)))
      (move distance)
      (define x1
        (get-x))
      (define y1
        (- (get-y)))
      (send dc
            draw-line
            (inexact->exact (round x0))
            (inexact->exact (round y0))
            (inexact->exact (round x1))
            (inexact->exact (round y1))))

    (define/override (label text size)

      ;; (super label text size)

      (define step-size
        (get-step-size))

      (define x-offset
        (* 0.25 size step-size))

      (define y-offset
        (* 0.5 size step-size))

      (define font
        (make-object font% (/ (* size step-size) 2) 'default))

      (send dc
            set-font
            font)

      (send dc
            draw-text
            text
            (+ (get-x) x-offset)
            (- (+ (get-y) y-offset))))))

