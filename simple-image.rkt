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
     [arc-stream (stream)])

    (define/override (get-arc-stream)
      arc-stream)

    (define/override (transpose x y)
      (define arc-stream1
        (for/stream ([a (in-stream arc-stream)])
          (send a transpose x y)))
      (new simple-image%
           [arc-stream arc-stream1]))

    (define/override (scale factor)
      (define arc-stream1
        (for/stream ([a (in-stream arc-stream)])
          (send a scale factor)))
      (new simple-image%
           [arc-stream arc-stream1]))))


