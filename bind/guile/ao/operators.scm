#|
    Copyright (C) 2016 Matthew Keeter  <matt.j.keeter@gmail.com>

    This file is part of Ao.

    Ao is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    Ao is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Ao.  If not, see <http://www.gnu.org/licenses/>.
|#
(define-module (ao operators))

(use-modules (srfi srfi-1) (ice-9 receive))
(use-modules (ao bind) (ao jit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Creates a binary tree that reduces the given list of arguments
(define (bin-reduce sym default args)
    (let* ((len  (length args))
           (half (quotient len 2)))
        (cond ((= 0 len) (if (null? default) (error "Too few arguments")
                                             (make-token default)))
              ((= 1 len) (make-token (car args)))
              (else (make-token sym
                        (bin-reduce sym default (take args half))
                        (bin-reduce sym default (drop args half)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns a wrapper function for a given commutative input function
;;
;; The wrapper will call func if all arguments are numbers; otherwise,
;; it will return a parse tree
(define (make-commutative func sym default)
  (lambda (. args)
    (receive (nums other) (partition number? args)
      (if (pair? nums)
          (let ((accum (apply func nums)))
            (if (pair? other)
                (bin-reduce sym default (cons accum other))
                accum))
          (bin-reduce sym default other)))))

;; Helper macro to make a backup of the builtin procedure then
;; make the wrapper function and export it
(define-syntax-rule (wrap-commutative f sym default)
    (begin (define builtin f)
           (define f (make-commutative builtin sym default))
           (export! f)))

(wrap-commutative + 'add 0)
(wrap-commutative * 'mul 0)
(wrap-commutative min 'min #nil)
(wrap-commutative max 'max #nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Handle subtraction, which behaves as negation with a single argument
(define _- -)
(define (sub . args)
    (receive (nums other) (partition number? args)
    (cond ((null-list? other) (apply _- nums))
          ((= 1 (length args)) (make-token 'neg (car args)))
          (else (make-token 'sub (car args)
                                 (apply + (cdr args)))))))

;; Handle division, which divides 1 / x for a single argument
;; (and accumulates cdr args with multiplication otherwise)
(define _/ /)
(define (div . args)
    (receive (nums other) (partition number? args)
    (cond ((null-list? other) (apply _/ nums))
          ((= 1 (length args)) (make-token 'div 1 (car args)))
          (else (make-token 'div (car args) (apply * (cdr args)))))))

;; Helper macro to define and export the overloaded procedure
(define-syntax-rule (wrap-custom f g)
    (begin (define f g)
           (export! f)))

(wrap-custom - sub)
(wrap-custom / div)

(define-public (mod a b)
    (if (and (number? a) (number? b))
        (let ((r (floor (/ a b))))
            (- a (* r b)))
        (make-token 'mod a b)))

(define-public (nan-fill a b)
    (if (or (tree? a) (tree? b))
        (make-token 'nan-fill a b)
        (if (nan? a) b a)))

(define expt_ expt)
(define-public (expt a b)
  (if (or (tree? a) (tree? b))
      (begin
        (when (and (tree? b) (not (token-const? b)))
          (error "RHS of exponentiation must be a constant"))
        (make-token 'pow a b))
      (expt_ a b)))
(export! expt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return a wrapper function for a unary function
(define (make-unary f sym)
  (lambda (a)
    (if (number? a) (f a) (make-token sym a))))

;; Helper macro for unary definition and export
(define-syntax-rule (wrap-unary f sym)
    (begin (define builtin f)
           (define f (make-unary builtin sym))
           (export! f)))

(wrap-unary sqrt 'sqrt)
(wrap-unary abs 'abs)
(wrap-unary sin 'sin)
(wrap-unary cos 'cos)
(wrap-unary tan 'tan)
(wrap-unary asin 'asin)
(wrap-unary acos 'acos)
(wrap-unary exp 'exp)

(define _atan atan)
(define (atan a . b)
    (cond
        ((= 1 (length b))
            (if (and (number? a) (number? (car b)))
                (_atan a (car b))
                (make-token 'atan2 a (car b))))
        ((= 0 (length b))
            (if (number? a)
                (_atan a)
                (make-token 'atan a)))
        (else (error "Too many arguments to atan"))))
(export! atan)

(define-public (square f)
    (if (number? f) (* f f)
                    (make-token 'square f)))
