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
   "../syntax-ext.rkt"
   "../abstract-path.rkt"
   "../null-turtle.rkt")

  (let ([turtle (new null-turtle%)])
    (check-equal? (send turtle get-x) 0)
    (check-equal? (send turtle get-y) 0)
    (check-equal? (send turtle get-face) 0)
    (check-equal? (send turtle get-step-size) 1)
    (check-equal? (send turtle get-min-x) 0)
    (check-equal? (send turtle get-max-x) 0)
    (check-equal? (send turtle get-min-y) 0)
    (check-equal? (send turtle get-max-y) 0))

  (let ([path    (with-path ())]
        [turtle (new null-turtle%)])
    (send path guide turtle)
    (check-equal? (send turtle get-x) 0)
    (check-equal? (send turtle get-y) 0)
    (check-equal? (send turtle get-face) 0)
    (check-equal? (send turtle get-step-size) 1)
    (check-equal? (send turtle get-min-x) 0)
    (check-equal? (send turtle get-max-x) 0)
    (check-equal? (send turtle get-min-y) 0)
    (check-equal? (send turtle get-max-y) 0))

  (let ([path    (with-path ()
                  (forward 4))]
        [turtle (new null-turtle%)])
    (send path guide turtle)
    (check-equal? (send turtle get-x) 4)
    (check-equal? (send turtle get-y) 0)
    (check-equal? (send turtle get-face) 0)
    (check-equal? (send turtle get-step-size) 1)
    (check-equal? (send turtle get-min-x) 0)
    (check-equal? (send turtle get-max-x) 4)
    (check-equal? (send turtle get-min-y) 0)
    (check-equal? (send turtle get-max-y) 0))

  (let ([path    (with-path ()
                  (move 5))]
        [turtle (new null-turtle%)])
    (send path guide turtle)
    (check-equal? (send turtle get-x) 5)
    (check-equal? (send turtle get-y) 0)
    (check-equal? (send turtle get-face) 0)
    (check-equal? (send turtle get-step-size) 1)
    (check-equal? (send turtle get-min-x) 0)
    (check-equal? (send turtle get-max-x) 5)
    (check-equal? (send turtle get-min-y) 0)
    (check-equal? (send turtle get-max-y) 0))

  (let ([path    (with-path ()
                  (turn 2))]
        [turtle (new null-turtle%)])
    (send path guide turtle)
    (check-equal? (send turtle get-x) 0)
    (check-equal? (send turtle get-y) 0)
    (check-equal? (send turtle get-face) 2)
    (check-equal? (send turtle get-step-size) 1)
    (check-equal? (send turtle get-min-x) 0)
    (check-equal? (send turtle get-max-x) 0)
    (check-equal? (send turtle get-min-y) 0)
    (check-equal? (send turtle get-max-y) 0))

  (let ([path    (with-path ()
                  (resize 3))]
        [turtle (new null-turtle%)])
    (send path guide turtle)
    (check-equal? (send turtle get-x) 0)
    (check-equal? (send turtle get-y) 0)
    (check-equal? (send turtle get-face) 0)
    (check-equal? (send turtle get-step-size) 3)
    (check-equal? (send turtle get-min-x) 0)
    (check-equal? (send turtle get-max-x) 0)
    (check-equal? (send turtle get-min-y) 0)
    (check-equal? (send turtle get-max-y) 0))

  (let ([path (with-path ()
               (forward 5)
               (turn (* 1/2 pi))
               (forward 4))]
        [turtle (new null-turtle%)])
    (send path guide turtle)
    (check-equal? (send turtle get-x) 5.0)
    (check-equal? (send turtle get-y) 4.0)
    (check-equal? (send turtle get-face) (* 1/2 pi))
    (check-equal? (send turtle get-step-size) 1)
    (check-equal? (send turtle get-min-x) 0.0)
    (check-equal? (send turtle get-max-x) 5.0)
    (check-equal? (send turtle get-min-y) 0.0)
    (check-equal? (send turtle get-max-y) 4.0))

  (let ([path (with-path ()
               (forward 5)
               (turn (* 1/2 pi))
               (forward 4))]
        [turtle (new null-turtle%)])
    (send (send path scale 3) guide turtle)
    (check-equal? (send turtle get-x) 15.0)
    (check-equal? (send turtle get-y) 12.0)
    (check-equal? (send turtle get-face) (* 1/2 pi))
    (check-equal? (send turtle get-step-size) 3)
    (check-equal? (send turtle get-min-x) 0.0)
    (check-equal? (send turtle get-max-x) 15.0)
    (check-equal? (send turtle get-min-y) 0.0)
    (check-equal? (send turtle get-max-y) 12.0))
  
  (let ([path (with-path ()
               (forward 5)
               (turn (* 1/2 pi))
               (forward 4))]
        [turtle (new null-turtle%)])
    (send (send path transpose -2 -1) guide turtle)
    (check-= (send turtle get-x) 3.0 0.001)
    (check-= (send turtle get-y) 3.0 0.001)
    (check-= (send turtle get-face) (* 1/2 pi) 0.001)
    (check-= (send turtle get-step-size) 1 0.001)
    (check-= (send turtle get-min-x) -2.0 0.001)
    (check-= (send turtle get-max-x) 3.0 0.001)
    (check-= (send turtle get-min-y) -1.0 0.001)
    (check-= (send turtle get-max-y) 3.0 0.001))
  
  )
