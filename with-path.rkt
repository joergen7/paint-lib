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
 (for-syntax
  racket/base
  syntax/parse)
 racket/class
 racket/contract)

(provide
 with-path
 forward
 turn
 resize
 move
 label)

(define a-path
  (make-parameter #f))

(define-syntax (with-path stx)
  (syntax-parse stx
    [(_ ()
        e_i ...)
     #'(with-path ((make-path))
         e_i ...)]
    [(_ (base)
        e_i ...)
     #'(parameterize ([a-path base])
         e_i ...
         (a-path))]))

(define/contract (forward distance)
  (-> rational? void?)
  (a-path (send (a-path) forward distance)))

(define/contract (turn angle)
  (-> rational? void?)
  (a-path (send (a-path) turn angle)))

(define/contract (resize factor)
  (-> rational? void)
  (a-path (send (a-path) resize factor)))

(define/contract (move distance)
  (-> rational? void)
  (a-path (send (a-path) move distance)))

(define/contract (label text size)
  (-> string? rational? void)
  (a-path (send (a-path) label text size)))
