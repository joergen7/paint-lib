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
     [turtle-stream (stream)])

    (define/override (get-turtle-stream)
      turtle-stream)

    (define/override (add turtle)
      (new simple-image%
           [turtle-stream (stream-cons turtle (get-turtle-stream))]))

    (define/override (scale factor)
      (define turtle-stream1
        (for/stream ([p turtle-stream])
          (send p scale factor)))
      (new simple-image%
           [turtle-stream turtle-stream1]))

    (define/override (transpose x y)
      (define turtle-stream1
        (for/stream ([p turtle-stream])
          (send p transpose x y)))
      (new simple-image%
           [turtle-stream turtle-stream1]))))

