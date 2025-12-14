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
 racket/class)

(provide
 turtle<%>)

(define turtle<%>
  (interface ()
    [get-x         (->m rational?)]
    [get-y         (->m rational?)]
    [get-face      (->m rational?)]
    [get-step-size (->m rational?)]
    [get-min-x     (->m rational?)]
    [get-max-x     (->m rational?)]
    [get-min-y     (->m rational?)]
    [get-max-y     (->m rational?)]
    [set-face      (->m rational? void?)]
    [set-pos       (->m rational? rational? void?)]
    [set-step-size (->m rational? void?)]
    [set-min       (->m rational? rational? void?)]
    [set-max       (->m rational? rational? void?)]
    [forward       (->m rational? void?)]
    [move          (->m rational? void?)]
    [resize        (->m rational? void?)]
    [turn          (->m rational? void?)]))

