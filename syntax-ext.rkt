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
 with-turtle
 make-image
 make-turtle
 split
 get-bitmap
 for/turtle)

(define-syntax (with-turtle stx)
  (syntax-parse stx
    [(_ () x_i ...)
     #'(with-turtle ((make-turtle)) x_i ...)]
    [(_ (base))
     #'base]
    [(_ (base) (e_i ...) (f_i ...) ...)
     #'(with-turtle ((send base e_i ...)) (f_i ...) ...)]))

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

(define-syntax (for/turtle stx)
  (syntax-parse stx
    [(_ (n) x ...)
     #'(for/turtle ((make-turtle) n) x ...)]
    [(_ (base n) x ...)
     #'(let recur ([turtle base]
                   [i      n])
         (cond
           [(zero? i)
            turtle]
           [else
            (recur
             (with-turtle (turtle)
               x ...)
             (sub1 i))]))]))

