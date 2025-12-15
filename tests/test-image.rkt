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

  (define path
    (with-path ()
      (forward 5)
      (turn (* 1/2 pi))
      (forward 4)))

  (define image0
    (make-image path))

  (check-= (send image0 get-min-x) 0.0 0.001)
  (check-= (send image0 get-max-x) 5.0 0.001)
  (check-= (send image0 get-min-y) 0.0 0.001)
  (check-= (send image0 get-max-y) 4.0 0.001)
  (check-= (send image0 get-width) 5.0 0.001)
  (check-= (send image0 get-height) 4.0 0.001)

  (define image1
    (send image0 scale 3))

  (check-= (send image1 get-min-x) 0.0 0.001)
  (check-= (send image1 get-max-x) 15.0 0.001)
  (check-= (send image1 get-min-y) 0.0 0.001)
  (check-= (send image1 get-max-y) 12.0 0.001)
  (check-= (send image1 get-width) 15.0 0.001)
  (check-= (send image1 get-height) 12.0 0.001)

  (define image2
    (send image0 transpose -2 -1))

  (check-= (send image2 get-min-x) -2.0 0.001)
  (check-= (send image2 get-max-x) 3.0 0.001)
  (check-= (send image2 get-min-y) -1.0 0.001)
  (check-= (send image2 get-max-y) 3.0 0.001)
  (check-= (send image2 get-width) 5.0 0.001)
  (check-= (send image2 get-height) 4.0 0.001)

  (define image3
    (send image2 fit-width 200))

  (check-= (send image3 get-min-x) 0.0 0.001)
  (check-= (send image3 get-min-y) (- (send image3 get-height)) 0.001)
  (check-= (send image3 get-width) 199 0.001)


  )
