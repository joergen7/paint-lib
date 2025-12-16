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
   racket/math
   racket/contract
   rackunit
   "../syntax-ext.rkt")

  (define angle1  0.4)
  (define x1      0.9)
  (define angle2 -0.2)
  (define x2      0.8)

  (define/contract (perturb x)
    (-> rational? rational?)
    (* x
       (add1 (* 0.4 (- (random) 0.5)))))
  
  (define/inductive tree
    [base
     (forward 1)
     (move -1)]
    [inductive
     (define a1
       (perturb angle1))
     (define a2
       (perturb angle2))
     (forward 1)
     (turn a1)
     (tree (perturb x1))
     (turn (- a1))
     (turn a2)
     (tree (perturb x2))
     (turn (- a2))
     (move -1)])


  (define path
    (tree (with-path () (turn (* 1/2 pi))) 11))

  (define image
    (make-image path))

  (get-bitmap image 600))
