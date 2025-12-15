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
 racket/contract
 racket/draw
 "simple-image.rkt"
 "path.rkt"
 "image.rkt"
 "abstract-path.rkt")

(provide
 make-image
 make-path
 get-bitmap
 repeat
 with-path
 lambda/inductive
 define/inductive
 forward
 turn
 move
 resize)

(define/contract (make-image . path-list)
  (-> (is-a?/c path<%>) ... (is-a?/c image<%>))
  (new simple-image%
       [path-stream path-list]))

(define/contract (make-path)
  (-> (is-a?/c path<%>))
  (new origin-path%))

(define/contract (get-bitmap image width)
  (-> (is-a?/c image<%>) exact-positive-integer? (is-a?/c bitmap%))
  (send image get-bitmap width))


(define-syntax (repeat stx)
  (syntax-parse stx
    [(_ (n) x ...)
     #'(let recur ([i n])
         (unless (zero? i)
           x ...
           (recur (sub1 i))))]))

(define-syntax (lambda/inductive stx)
  (syntax-parse stx
    #:datum-literals (base inductive)
    [(_ name
        [base e_i ...]
        [inductive f_i ...])
     #'(lambda (path n)
         (with-path (path)
           (let recur ([i n])
             (define (name)
               (recur (sub1 i)))
             (cond
               [(zero? i)
                e_i ...]
               [else
                f_i ...]))))]))
                
(define-syntax (define/inductive stx)
  (syntax-parse stx
    [(_ name x_i ...)
     #'(define name
         (lambda/inductive name x_i ...))]))

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
