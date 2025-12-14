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

(module+ test
  (require
   racket/class
   racket/math
   rackunit
   "../syntax-ext.rkt")

  (define (polygon n)
    (repeat (n)
      (forward 1)
      (turn (/ (* 2 pi) n))))

  (define/inductive sierp
    [base
     (polygon 3)]
    [inductive
     (repeat (3)
       (resize 1/2)
       (sierp)
       (resize 2)
       (move 1)
       (turn (* 2/3 pi)))])

  (define t
    (sierp (make-arc) 6))

  (define image
    (make-image t))

  (send image get-bitmap 400))

 
