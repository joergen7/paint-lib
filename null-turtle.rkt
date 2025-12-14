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
 "abstract-turtle.rkt"
 "normalize-angle.rkt")

(provide
 null-turtle%)

(define null-turtle%
  (class abstract-turtle%
    (super-new)

    (init-field
     [x         0]
     [y         0]
     [face      0]
     [step-size 1]
     [min-x     0]
     [max-x     0]
     [min-y     0]
     [max-y     0])

    (define/override (get-x)
      x)

    (define/override (get-y)
      y)

    (define/override (get-face)
      face)

    (define/override (get-step-size)
      step-size)

    (define/override (get-min-x)
      min-x)

    (define/override (get-max-x)
      max-x)

    (define/override (get-min-y)
      min-y)

    (define/override (get-max-y)
      max-y)

    (define/override (set-face angle)
      (set! face (normalize-angle angle)))

    (define/override (set-pos x1 y1)
      (set! x x1)
      (set! y y1))

    (define/override (set-step-size step-size1)
      (set! step-size step-size1))

    (define/override (set-min min-x1 min-y1)
      (set! min-x min-x1)
      (set! min-y min-y1))

    (define/override (set-max max-x1 max-y1)
      (set! max-x max-x1)
      (set! max-y max-y1))))

