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
   rackunit
   "../syntax-ext.rkt")

  (define factor  0.7)
  (define angle1  0.5)
  (define x1      1)
  (define angle2 -0.1)
  (define x2      1.3)
  
  (define/inductive tree
    [base
     (forward 1)
     (turn pi)
     (move 1)
     (turn pi)]
    [inductive
     (forward 1)
     (resize factor)
     (turn angle1)
     (resize x1)
     (tree)
     (resize (/ 1 x1))
     (turn (- angle1))
     (turn angle2)
     (resize x2)
     (tree)
     (resize (/ 1 x2))
     (turn (- angle2))
     (resize (/ 1 factor))
     (turn pi)
     (move 1)
     (turn pi)])


  (define path
    (tree (make-path) 12))

  (define image
    (make-image path))

  (get-bitmap image 600))
