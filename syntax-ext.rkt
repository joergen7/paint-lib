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
 "abstract-paint.rkt"
 "simple-image.rkt"
 (for-syntax
  racket/base
  syntax/parse))

(provide
 with-paint
 make-image
 make-paint
 split
 get-bitmap)

(define-syntax (with-paint stx)
  (syntax-parse stx
    [(_ () x_i ...)
     #'(with-paint ((make-paint)) x_i ...)]
    [(_ (base))
     #'base]
    [(_ (base) (e_i ...) (f_i ...) ...)
     #'(with-paint ((send base e_i ...)) (f_i ...) ...)]))

(define-syntax (make-image stx)
  (syntax-parse stx
    [(_ p ...)
     #'(new simple-image%
            [paint-stream (stream p ...)])]))

(define-syntax (make-paint stx)
  (syntax-parse stx
    [(_)
     #'(new origin-paint%)]))

(define-syntax (split stx)
  (syntax-parse stx
    [(_ p)
     #'(send p split)]))

(define-syntax (get-bitmap stx)
  (syntax-parse stx
    [(_ i n)
     #'(send i get-bitmap n)]))
