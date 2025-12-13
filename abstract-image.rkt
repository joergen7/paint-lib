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
 "image.rkt")

(provide
 abstract-image%)

(define abstract-image%
  (class* object% (image<%>)
    (super-new)

    (abstract
     get-turtle-stream
     add
     scale
     transpose)

    (define/public (get-min-x)
      (apply
       min
       (for/list ([p (get-turtle-stream)])
         (send p get-min-x))))

    (define/public (get-max-x)
      (apply
       max
       (for/list ([p (get-turtle-stream)])
         (send p get-max-x))))

    (define/public (get-min-y)
      (apply
       min
       (for/list ([p (get-turtle-stream)])
         (send p get-min-y))))

    (define/public (get-max-y)
      (apply
       max
       (for/list ([p (get-turtle-stream)])
         (send p get-max-y))))

    (define/public (get-width)
      (- (get-max-x) (get-min-x)))

    (define/public (get-height)
      (- (get-max-y) (get-min-y)))

    (define/public (fit-width width)
      (define c1
        (transpose (- (get-min-x)) (- (get-min-y))))
      (define width0
        (get-width))
      (if (zero? width0)
          c1
          (send c1 scale (/ width width0))))

    (define/public (get-bitmap width)
      (define c1
        (fit-width (sub1 width)))
      (define height
        (add1
         (inexact->exact
          (round
           (send c1 get-height)))))
      (define bitmap
        (make-bitmap width height))
      (define dc
        (new bitmap-dc%
             [bitmap bitmap]))
      (for-each
       (lambda (p)
         (send p dump dc))
       (stream->list (send c1 get-turtle-stream)))
      bitmap)))
