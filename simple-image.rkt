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
 racket/stream
 "abstract-image.rkt")

(provide
 simple-image%)

(define simple-image%
  (class abstract-image%
    (super-new)

    (init-field
     [paint-stream (stream)])

    (define/override (get-paint-stream)
      paint-stream)

    (define/override (add paint)
      (new simple-image%
           [paint-stream (stream-cons paint (get-paint-stream))]))

    (define/override (scale factor)
      (define paint-stream1
        (for/stream ([p paint-stream])
          (send p scale factor)))
      (new simple-image%
           [paint-stream paint-stream1]))

    (define/override (transpose x y)
      (define paint-stream1
        (for/stream ([p paint-stream])
          (send p transpose x y)))
      (new simple-image%
           [paint-stream paint-stream1]))))

