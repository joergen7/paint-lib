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
 racket/stream
 "image.rkt"
 "null-turtle.rkt"
 "dc-turtle.rkt")

(provide
 abstract-image%)

(define abstract-image%
  (class* object% (image<%>)
    (super-new)

    (abstract
     get-path-stream
     transpose
     scale)

    (define turtle-stream
      (for/stream ([path (in-stream (get-path-stream))])
        (define turtle
          (new null-turtle%))
        (send path guide turtle)
        turtle))

    (define/public (get-min-x)
      (define candidate-list
        (for/list ([t (in-stream turtle-stream)])
          (send t get-min-x)))
      (apply min candidate-list))

    (define/public (get-max-x)
      (define candidate-list
        (for/list ([t (in-stream turtle-stream)])
          (send t get-max-x)))
      (apply max candidate-list))

    (define/public (get-min-y)
      (define candidate-list
        (for/list ([t (in-stream turtle-stream)])
          (send t get-min-y)))
      (apply min candidate-list))

    (define/public (get-max-y)
      (define candidate-list
        (for/list ([t (in-stream turtle-stream)])
          (send t get-max-y)))
      (apply max candidate-list))

    (define/public (get-width)
      (- (get-max-x) (get-min-x)))

    (define/public (get-height)
      (- (get-max-y) (get-min-y)))

    (define/public (fit-width width)
      (define image1
        (transpose (- (get-min-x)) (- (get-max-y))))
      (define width0
        (get-width))
      (send image1 scale (/ (sub1 width) width0)))

    (define/public (get-bitmap width)
      (define image1
        (fit-width width))
      (define height
        (add1
         (inexact->exact
          (round
           (send image1 get-height)))))
      (define bitmap
        (make-bitmap width height))
      (define dc
        (new bitmap-dc%
             [bitmap bitmap]))
      (define turtle
        (new dc-turtle%
             [dc dc]))
      (stream-for-each
       (lambda (path)
         (send path guide turtle))
       (send image1 get-path-stream))
      bitmap)))



