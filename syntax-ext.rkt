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
 racket/stream
 "abstract-turtle.rkt"
 "simple-image.rkt"
 (for-syntax
  racket/base
  syntax/parse))

(provide
 make-image
 make-turtle
 split
 get-bitmap
 repeat
 lambda/turtle
 define/turtle
 with-turtle
 forward
 hatch
 move
 turn
 resize)

(define-syntax (make-image stx)
  (syntax-parse stx
    [(_ p ...)
     #'(new simple-image%
            [turtle-stream (stream p ...)])]))

(define-syntax (make-turtle stx)
  (syntax-parse stx
    [(_)
     #'(new origin-turtle%)]))

(define-syntax (split stx)
  (syntax-parse stx
    [(_ p)
     #'(send p split)]))

(define-syntax (get-bitmap stx)
  (syntax-parse stx
    [(_ i n)
     #'(send i get-bitmap n)]))

(define-syntax (repeat stx)
  (syntax-parse stx
    [(_ (n) x ...)
     #'(let recur ([i n])
         (unless (zero? i)
           x ...
           (recur (sub1 i))))]))

(define-syntax (lambda/turtle stx)
  (syntax-parse stx
    #:datum-literals (base inductive)
    [(_ name
        [base e_i ...]
        [inductive f_i ...])
     #'(lambda (turtle n)
         (with-turtle (turtle)
           (let recur ([i n])
             (define (name)
               (recur (sub1 i)))
             (cond
               [(zero? i)
                e_i ...]
               [else
                f_i ...]))))]))
                
(define-syntax (define/turtle stx)
  (syntax-parse stx
    [(_ name x_i ...)
     #'(define name
         (lambda/turtle name x_i ...))]))


(define a-turtle
  (make-parameter #f))

(define-syntax (with-turtle stx)
  (syntax-parse stx
    [(_ () e_i ...)
     #'(with-turtle ((make-turtle)) e_i ...)]
    [(_ (base) e_i ...)
     #'(parameterize ([a-turtle base]) e_i ... (a-turtle))]))

(define-syntax (forward stx)
  (syntax-parse stx
    [(_ distance)
     #'(a-turtle (send (a-turtle) forward distance))]))

(define-syntax (hatch stx)
  (syntax-parse stx
    [(_)
     #'(a-turtle (send (a-turtle) hatch))]))

(define-syntax (move stx)
  (syntax-parse stx
    [(_ distance)
     #'(a-turtle (send (a-turtle) move distance))]))

(define-syntax (resize stx)
  (syntax-parse stx
    [(_ factor)
     #'(a-turtle (send (a-turtle) resize factor))]))

(define-syntax (turn stx)
  (syntax-parse stx
    [(_ angle)
     #'(a-turtle (send (a-turtle) turn angle))]))


