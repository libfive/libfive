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

(define-module (ao jit))

(use-modules (ice-9 common-list))
(use-modules (system foreign))

(use-modules (ao bind))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is our local store.  When populated, it's an opaque pointer to a
;; Store object (created in C++ with store_new)
(define store #nil)

;; Nested stores are pushed onto this stack
(define stack '())

(define (store-push)
    "Pushes the existing store to the stack and creates a new store"
    (set! stack (cons store stack))
    (set! store (store-new)))

(define (store-pop)
    "Replaces the store with the top of the stack"
    (set! store (car stack))
    (set! stack (cdr stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Converts the given argument to a token
;; Based on argument type, the following behavior occurs
;; - A number is converted to constant
;; - A symbol and further arguments is converted to an operator
(define-public (make-token a . args)
    "Creates a token in the global store.
A token is returned without changes
A number is converted to a constant
A symbol and further arguments are converted to an operation"
    (let ((len (length args)))
    (cond
        ((= 0 len)
            (cond ((token? a) a)
                  ((number? a) (token-const store a))
                  (else (error "Failed to construct token" a))))
        ((= 1 len)
            (token-op-unary store a (make-token (car args))))
        ((= 2 len)
            (token-op-binary store a (make-token (car args))
                                     (make-token (cadr args))))
        (else (error "Incorrect argument count to make-token")))))


(define-public (make-bounds-token shape lower upper)
    (token-bounded store shape lower upper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-root-token f)
    "Returns the root token for the given function"
    (let* ((x (token-x store))
           (y (token-y store))
           (z (token-z store))
           (out (make-token (f x y z))))
        out))

(define* (jit f #:key (manage #t))
    "Compile an arithmetic lambda function to a bare tree pointer
    If manage is #t (default), attaches a finalizer to the output"
    (store-push)
    (let* ((root (get-root-token f))
           (out (tree-new store root)))
        (store-pop)
        (if manage (tree-attach-finalizer out) out)))
(export jit)

(define-public (jit-function f)
    "Compile an arithmetic lambda function to a wrapped math function"
    (let ((t (jit f))
          (interval? (lambda (x) (and (pair? x) (not (list? x))))))
    (lambda (x y z)
        (cond ((and (interval? x) (interval? y) (interval? z))
               (tree-eval-interval t x y z))
              ((and (number? x) (number? y) (number? z))
               (tree-eval-double t x y z))
              (else (error "Invalid arguments (must be floats or pairs)"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (get-affine-vec f)
    "get-affine-vec f
    f should be a function of the form (lambda (x y z) ...)
    If the result is an affine combination a*x + b*y + c*z + d, returns
    '(a b c d); otherwise, returns #f"
    (store-push)
    (let* ((root (get-root-token f))
           (result (token-affine-vec root)))
        (store-pop)
        result))

(define-public (get-bounds f)
    "get-bounds f
    If f is wrapped with set-bounds, returns the bounds in the form
    '((xmin ymin zmin) (xmax ymax zmax)); otherwise, returns #f"
    (store-push)
    (let* ((root (get-root-token f))
           (result (token-bounds root)))
        (store-pop)
        result))
