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
 racket/draw)

(provide
 turtle<%>)

(define turtle<%>
  (interface ()
    [dump          (->m (is-a?/c bitmap-dc%) void?)]
    [get-face      (->m rational?)]
    [get-x         (->m rational?)]
    [get-y         (->m rational?)]
    [get-min-x     (->m rational?)]
    [get-max-x     (->m rational?)]
    [get-min-y     (->m rational?)]
    [get-max-y     (->m rational?)]
    [get-step-size (->m rational?)]
    [face          (recursive-contract (->m rational? (is-a?/c turtle<%>)))]
    [forward       (recursive-contract (->m rational? (is-a?/c turtle<%>)))]
    [hatch         (recursive-contract (->m (is-a?/c turtle<%>)))]
    [move          (recursive-contract (->m rational? (is-a?/c turtle<%>)))]
    [resize        (recursive-contract (->m rational? (is-a?/c turtle<%>)))]
    [scale         (recursive-contract (->m rational? (is-a?/c turtle<%>)))]
    [transpose     (recursive-contract (->m rational? rational? (is-a?/c turtle<%>)))]
    [turn          (recursive-contract (->m rational? (is-a?/c turtle<%>)))]
    [write         (recursive-contract (->m string?   (is-a?/c turtle<%>)))]))


