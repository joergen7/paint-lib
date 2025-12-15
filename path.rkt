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
 "turtle.rkt")

(provide
 path<%>)

(define path<%>
  (interface ()
    [forward   (recursive-contract (->m rational? (is-a?/c path<%>)))]
    [turn      (recursive-contract (->m rational? (is-a?/c path<%>)))]
    [resize    (recursive-contract (->m rational? (is-a?/c path<%>)))]
    [move      (recursive-contract (->m rational? (is-a?/c path<%>)))]
    [label     (recursive-contract (->m string? rational? (is-a?/c path<%>)))]
    [hatch     (recursive-contract (->m (is-a?/c path<%>)))]
    [guide     (->m (is-a?/c turtle<%>) void?)]
    [transpose (recursive-contract (->m rational? rational? (is-a?/c path<%>)))]
    [scale     (recursive-contract (->m rational? (is-a?/c path<%>)))]))

