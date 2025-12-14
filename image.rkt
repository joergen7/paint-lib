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
 racket/contract
 racket/stream
 racket/draw
 "arc.rkt")

(provide
 image<%>)

(define image<%>
  (interface ()
    [get-min-x      (->m rational?)]
    [get-max-x      (->m rational?)]
    [get-min-y      (->m rational?)]
    [get-max-y      (->m rational?)]
    [get-width      (->m rational?)]
    [get-height     (->m rational?)]
    [fit-width      (recursive-contract (->m exact-positive-integer? (is-a?/c image<%>)))]
    [scale          (recursive-contract (->m rational? (is-a?/c image<%>)))]
    [transpose      (recursive-contract (->m rational? rational? (is-a?/c image<%>)))]
    [get-arc-stream (->m (stream/c (is-a?/c arc<%>)))]
    [get-bitmap     (->m rational? (is-a?/c bitmap%))]))

