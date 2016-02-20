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

(use-modules (system foreign))
(use-modules (ice-9 common-list))
(use-modules (ao lib) (ao ptr))

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
    (if (= 0 (length args))
        (cond ((tagged-ptr? 'Token a) a)
              ((number? a) (token_const store a))
              (else (error "Failed to construct token" a)))
        (token_op store a (map make-token args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (interval? i)
    "Check to see if the given argument is a valid Interval"
    (and (pair? i) (not (list? i)) (number? (car i)) (number? (cdr i))))

(define-public (wrap-tree t)
    " Wraps a tagged tree pointer in a callable interface "
    (lambda (x y z) ;; Generic evaluator that dispatches based on argument type
        (cond ((every interval? (list x y z))
                    (tree_eval_interval t x y z))
              ((every number? (list x y z))
                    (tree_eval_double t x y z))
              (else (error "Input arguments are of invalid types")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (jit f)
    "Compile an arithmetic lambda function to a bare tree pointer"
    (set! store (store_new))
    (let* ((x (token_x store))
           (y (token_y store))
           (z (token_z store))
           (root (make-token (f x y z)))
           (out (tree_new store root)))
       (set! store #nil)
       out))
