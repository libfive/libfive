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
(use-modules (ao bind))
(use-modules (system foreign))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is our local store.  When populated, it's an opaque pointer to a
;; Store object (created in C++ with store_new)
(define store #nil)

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

(define-public (jit f)
    "Compile an arithmetic lambda function to a bare tree pointer"
    (set! store (store-new))
    (let* ((x (token-x store))
           (y (token-y store))
           (z (token-z store))
           (root (make-token (f x y z)))
           (out (tree-new store root)))
        (store-delete store)
        (set! store #nil)
    out))

(define-public (jit-function f)
    "Compile and arithmetic lambda function to a wrapped math function"
    (let ((t (tree-attach-finalizer (jit f))))
    (lambda (x y z) (tree-eval-double t x y z))))
