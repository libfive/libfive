#|
Guile bindings to the libfive CAD kernel
Copyright (C) 2021  Matt Keeter

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
|#
(define-module (libfive kernel))

(use-modules
  (oop goops)
  (rnrs bytevectors)
  (srfi srfi-28)
  (system foreign)

  (libfive lib)
  (libfive vec)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The basic shape class and constructor functions
(define-class <shape> (<number>) (ptr #:init-value #f #:init-keyword #:ptr))
(define (shape? t) (is-a? t <shape>))
(export shape? <shape>)

(define-public (shape->ptr t) (slot-ref t 'ptr))
(define (ptr->shape p)
  "Converts a foreign pointer to a shape, attaching a deletion finalizer"
  (make <shape> #:ptr (make-pointer (pointer-address p) libfive-tree-del-ptr)))

(define (ensure-shape t)
  (cond ((shape? t) t)
        ((number? t) (number->shape t))
        (else (scm-error 'wrong-type-arg ensure-shape
               "Wrong argument ~A (should be number or shape) " (list t) (list t)))))

(define-public (opcode-enum s)
  " Converts a symbol to an opcode, or throws an error if no match is found"
  (define op (libfive-opcode-enum (string->pointer (symbol->string s))))
  (if (eq? op -1)
    (error (format "Invalid opcode: ~s" s))
    op))

(define-public (number->shape f)
  (ptr->shape (libfive-tree-const f)))

(define* (make-shape op #:optional a b)
  " Universal shape construction function, e.g. (make-shape 'add a b)"
  (define opcode (opcode-enum op))
  (define args (libfive-opcode-args opcode))
  (cond
    ((= args 0)
      (if (or a b)
        (error (format "Opcode ~s takes 0 arguments" op))
        (ptr->shape (libfive-tree-nullary opcode))))
    ((= args 1)
      (if (or (not (shape? a)) b)
        (error (format "Opcode ~s takes 1 shape argument" op))
        (ptr->shape (libfive-tree-unary opcode (shape->ptr a)))))
    ((= args 2)
      (if (or (not (shape? a)) (not (shape? b)))
        (error (format "Opcode ~s takes 2 shape arguments" op))
        (ptr->shape (libfive-tree-binary opcode (shape->ptr a) (shape->ptr b)))))
))
(export make-shape)

(define-public (var? t)
  " Checks whether the given shape is a free variable"
  (equal? 1 (libfive-tree-is-var (shape->ptr t))))

(define-public (make-var)
  " Constructs a new shape representing a free variable"
  (ptr->shape (libfive-tree-var)))

(define-public (constant t)
  " Returns a new Shape which represents the given shape with its free variables
    locked (so that the partial derivatives with respect to those vars is 0).
    This is useful to fine-tune the behavior of direct modeling."
  (make-shape 'const-var t))

(define-public (shape-tree-id t)
  " Returns an id for the given tree.

    This ID is not globally deduplicated for trees other than x/y/z and free
    variables; in other words (+ x y) and (+ x y) will have different IDs."
  (libfive-tree-id (shape->ptr t)))

(define-public (shape-remap t x' y' z')
  " Remaps a shape with the given transform.

  Consider using the remap-shape syntax form instead, for ease of use."
  (ptr->shape (libfive-tree-remap (shape->ptr t)
    (shape->ptr x') (shape->ptr y') (shape->ptr z'))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematical operations on shapes

;; Helper macro to overload binary operations
(define-macro (overload func sym) `(begin
  (define-method (,func (a <number>) (b <shape>)) (,func (ensure-shape a) b))
  (define-method (,func (a <shape>) (b <number>)) (,func a (ensure-shape b)))
  (define-method (,func (a <shape>) (b <shape>)) (make-shape ,sym a b)))
)

;; Overload all binary operations for shapes
(overload + 'add)
(overload * 'mul)
(overload min 'min)
(overload max 'max)
(overload - 'sub)
(overload / 'div)
(overload atan 'atan2)
(overload modulo 'mod)
(overload nan-fill 'nanfill)
(overload compare 'compare)

;; Hand-written overload for expt
(define-method (expt (a <shape>) (b <fraction>))
  (make-shape 'nth-root (make-shape 'pow a (numerator b))
                        (denominator b)))
(define-method (expt (a <shape>) (b <number>))
  (scm-error 'wrong-type-arg 'expt
      "RHS of exponentiation must be rational, not ~A"
      (list b) (list b)))

;; Unary operations
(define-method (/ (a <shape>)) (make-shape 'recip a))
(define-method (- (a <shape>)) (make-shape 'neg a))
(define-method (sqrt (a <shape>)) (make-shape 'sqrt a))
(define-method (sin (a <shape>)) (make-shape 'sin a))
(define-method (cos (a <shape>)) (make-shape 'cos a))
(define-method (tan (a <shape>)) (make-shape 'tan a))
(define-method (asin (a <shape>)) (make-shape 'asin a))
(define-method (acos (a <shape>)) (make-shape 'acos a))
(define-method (exp (a <shape>)) (make-shape 'exp a))
(define-method (abs (a <shape>)) (make-shape 'abs a))
(define-method (atan (a <shape>)) (make-shape 'atan a))
(define-method (log (a <shape>)) (make-shape 'log a))
(define-method (square (a <shape>)) (make-shape 'square a))

;; Define operations which aren't normally present for numbers
(define-method (nan-fill (a <number>) (b <number>)) (if (nan? a) b a))
(define-method (compare (a <number>) (b <number>))
  (cond ((< a b) -1)
        ((> a b)  1)
        ( else    0)))
(define-method (square (f <number>)) (* f f))

;; Add identities to shape
(define-method (+ (a <shape>)) a)
(define-method (* (a <shape>)) a)
(define-method (min (a <shape>)) a)
(define-method (max (a <shape>)) a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shape evaluation
(define-method (shape-eval (a <shape>) (pt <vec3>))
    (libfive-tree-eval-f (shape->ptr a) (vec3->ffi pt)))

(define-method (shape-eval (a <shape>) (lower <vec3>) (upper <vec3>))
    (apply cons
      (parse-c-struct
        (libfive-tree-eval-i (shape->ptr a)
                 (libfive-region (libfive-interval (.x lower) (.x upper))
                          (libfive-interval (.y lower) (.y upper))
                          (libfive-interval (.z lower) (.z upper))))
        (list float float))))

(define-method (shape-derivs (a <shape>) (pt <vec3>))
    (ffi->vec3 (libfive-tree-eval-d (shape->ptr a) (vec3->ffi pt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntactic sugar for constructing and editing shapes
(define-syntax-rule (lambda-shape vars ...)
  (ensure-shape
    ((lambda vars ...) (make-shape 'var-x)
                       (make-shape 'var-y)
                       (make-shape 'var-z))))

(define-syntax-rule (define-shape (name . vars) body ...)
  (define name (lambda-shape vars body ...)))

(define-syntax remap-shape
  (syntax-rules ()
    ((remap-shape (shape . vars) x y z)
        (remap-shape shape vars x y z))
    ((remap-shape shape (. vars) x y z)
        (shape-remap shape (lambda-shape vars x)
                           (lambda-shape vars y)
                           (lambda-shape vars z)))))

(define-syntax sequence_
  (syntax-rules ()
   ((sequence_ a) a)
   ((sequence_ a (t . args) . rest)
      (sequence_ (t a . args) . rest))
))

(define-syntax-rule (v->list a)
  (call-with-values (lambda () a) (lambda (. args) args)))

(define-syntax vs->list
  (syntax-rules ()
    ((vs->list a) (list (v->list a)))
    ((vs->list a . rest) (cons (v->list a) (vs->list . rest)))))

(define-syntax-rule (values->list . vs)
  (apply append (vs->list . vs)))

(define-syntax-rule (values-from . vs)
  (apply values (values->list . vs)))

(define-syntax-rule (sequence a . args)
  (apply values (map (lambda (q) (sequence_ q . args))
                     (apply append (vs->list a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (shape->string t)
  " Converts a shape into a prefix string"
  (define s (libfive-tree-print (shape->ptr t)))
  (define out (pointer->string s))
  (libfive-free-str s)
  out)

(define-public (shape-save-mesh shape filename resolution bounds)
  " Saves the given shape to a file

    bounds is a list of pairs, e.g.
      '((xmin . xmax) (ymin . ymax) (zmin . zmax))
  "
  (let ((xmin (caar bounds))
        (xmax (cdar bounds))
        (ymin (caadr bounds))
        (ymax (cdadr bounds))
        (zmin (caaddr bounds))
        (zmax (cdaddr bounds)))
    (libfive-tree-save-mesh (shape->ptr shape)
      (libfive-region (list xmin xmax) (list ymin ymax) (list zmin zmax))
      resolution (string->pointer filename))))

(define* (shapes-save-mesh shapes filename resolution bounds
                           #:optional (quality 8))
  (define ptr-size (sizeof '*))
  (define bv (make-bytevector (* ptr-size (1+ (length shapes)))))
  ;; Pack the shape pointers into a bytevector array
  (let f ((i 0) (shapes shapes))
    (bytevector-uint-set! bv (* i ptr-size)
      (if (null? shapes) 0 (pointer-address (shape->ptr (car shapes))))
      (native-endianness) ptr-size)
    (unless (null? shapes)
      (f (1+ i) (cdr shapes))))
  (libfive-tree-save-meshes (bytevector->pointer bv)
    bounds resolution quality (string->pointer filename)))
(export shapes-save-mesh)

(define-public (shape-save shape filename)
  " Loads a shape from a file"
  (libfive-tree-save (shape->ptr shape) (string->pointer filename)))

(define-public (shape-load filename)
  " Loads a shape from a file"
  (libfive-tree-load (string->pointer filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are "safe" bindings that can be used in the sandbox
(define libfive-bindings '(
    square
    nan-fill
    constant
    compare
    lambda-shape
    define-shape
    remap-shape
    make-var
    shape->string
    shape-eval
    shape-derivs
    sequence
    values-from
    values->list
    libfive-bindings))
(eval (cons 'export libfive-bindings) (interaction-environment))

