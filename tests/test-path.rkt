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

  (define path0
    (with-path ()
      (forward 5)
      (turn (* 1/2 pi))
      (forward 4)))

  (define origin0
    (get-field parent (get-field parent (get-field parent path0))))

  (check-equal? (get-field x origin0) 0)
  (check-equal? (get-field y origin0) 0)
  (check-equal? (get-field face origin0) 0)
  (check-equal? (get-field step-size origin0) 1)

  (define path1
    (send path0 scale 3))
  
  (define origin1
    (get-field parent (get-field parent (get-field parent path1))))

  (check-equal? (get-field x origin1) 0)
  (check-equal? (get-field y origin1) 0)
  (check-equal? (get-field face origin1) 0)
  (check-equal? (get-field step-size origin1) 3)

  (define path2
    (send path0 transpose -2 -1))

  (define origin2
    (get-field parent (get-field parent (get-field parent path2))))
  
  (check-equal? (get-field x origin2) -2)
  (check-equal? (get-field y origin2) -1)
  (check-equal? (get-field face origin2) 0)
  (check-equal? (get-field step-size origin2) 1)

  )
