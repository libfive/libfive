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

;; Converts the given argument to a tree
;; Based on argument type, the following behavior occurs
;; - A number is converted to constant
;; - A symbol and further arguments is converted to an operator
(define-public (make-tree a . args)
    "Creates a tree.
A tree is returned without changes
A number is converted to a constant
A symbol and further arguments are converted to an operation"
    (let ((len (length args)))
    (cond
        ((= 0 len)
            (cond ((tree? a) a)
                  ((number? a) (tree-const a))
                  (else (error "Failed to construct tree" a))))
        ((= 1 len)
            (tree-op-unary a (make-tree (car args))))
        ((= 2 len)
            (tree-op-binary a (make-tree (car args))
                              (make-tree (cadr args))))
        (else (error "Incorrect argument count to make-tree")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (jit f)
    "Returns the root tree for the given function"
    (let* ((x (tree-x))
           (y (tree-y))
           (z (tree-z))
           (out (make-tree (f x y z))))
        out))

(define-public (jit-function f)
    "jit-function f
    Compile an arithmetic lambda function (f x y z) to a wrapped math function
    The returned function can be called with numbers or pairs representing
    intervals, i.e. '(lower . upper)"
    (let ((t (jit f))
          (interval? (lambda (x) (and (pair? x) (not (list? x))))))
    (lambda (x y z)
        "(f x y z)
    x, y, z are floats for single-point evaluation
    or pairs '(lower . upper) for interval evaluation"
        (cond ((every interval? (list x y z))
               (tree-eval-interval t x y z))
              ((every number? (list x y z))
               (tree-eval-double t x y z))
              (else (error "Invalid arguments (must be floats or pairs)"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (get-affine-vec f)
    "get-affine-vec f
    f should be a function of the form (lambda (x y z) ...)
    If the result is an affine combination a*x + b*y + c*z + d, returns
    '(a b c d); otherwise, returns #f"
    (tree-affine-vec (jit f)))
