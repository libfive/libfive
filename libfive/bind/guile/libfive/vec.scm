#|
Guile bindings to the libfive CAD kernel
Copyright (C) 2017  Matt Keeter

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

(define-module (libfive vec))

(use-modules
  (ice-9 textual-ports)
  (oop goops)
  (system foreign)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <vec2> () (x #:getter .x) (y #:getter .y))

(define-method (vec2 (x <number>) (y <number>))
  (let ((v (make <vec2>)))
    (slot-set! v 'x x)
    (slot-set! v 'y y)
    v))
(export vec2 <vec2>)

(define-method (write (self <vec2>) port)
  (format port "#[~a ~a]" (slot-ref self 'x) (slot-ref self 'y)))

(define-method (+ (a <vec2>) (b <vec2>))
  (vec2 (+ (slot-ref a 'x) (slot-ref b 'x))
        (+ (slot-ref a 'y) (slot-ref b 'y))))
(define-method (+ (a <vec2>) (b <number>))
  (vec2 (+ (slot-ref a 'x) b)
        (+ (slot-ref a 'y) b)))
(define-method (+ (a <number>) (b <vec2>)) (+ b a))

(define-method (* (a <vec2>) (b <number>))
  (vec2 (* (slot-ref a 'x) b)
        (* (slot-ref a 'y) b)))
(define-method (* (a <number>) (b <vec2>)) (* b a))

(define-method (- (a <vec2>) (b <vec2>))
  (vec2 (- (slot-ref a 'x) (slot-ref b 'x))
        (- (slot-ref a 'y) (slot-ref b 'y))))
(define-method (- (a <vec2>) (b <number>))
  (vec2 (- (slot-ref a 'x) b)
        (- (slot-ref a 'y) b)))
(define-method (- (a <number>) (b <vec2>))
  (vec2 (- a (slot-ref b 'x))
        (- a (slot-ref b 'y))))
(define-method (- (a <vec2>))
  (vec2 (- (slot-ref a 'x))
        (- (slot-ref a 'y))))

(define-method (/ (a <vec2>) (b <number>))
  (vec2 (/ (slot-ref a 'x) b)
        (/ (slot-ref a 'y) b)))

(define-method (norm (a <vec2>))
  (sqrt (+ (expt (.x a) 2)
           (expt (.y a) 2))))
(export norm)

(define-method (dot (a <vec2>) (b <vec2>))
  (+ (* (.x a) (.x b))
     (* (.y a) (.y b))))
(export dot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <vec3> () (x #:getter .x) (y #:getter .y) (z #:getter .z))

(define-method (write (self <vec3>) port)
  (format port "#[~a ~a ~a]"
    (slot-ref self 'x)(slot-ref self 'y) (slot-ref self 'z)))

(define-method (vec3 (x <number>) (y <number>) (z <number>))
  (let ((v (make <vec3>)))
    (slot-set! v 'x x)
    (slot-set! v 'y y)
    (slot-set! v 'z z)
    v))
(define-method (vec3 (a <vec2>) (z <number>))
  (let ((v (make <vec3>)))
    (slot-set! v 'x (slot-ref a 'x))
    (slot-set! v 'y (slot-ref a 'y))
    (slot-set! v 'z z)
    v))
(export vec3 <vec3>)

(define-method (+ (a <vec3>) (b <vec3>))
  (vec3 (+ (slot-ref a 'x) (slot-ref b 'x))
        (+ (slot-ref a 'y) (slot-ref b 'y))
        (+ (slot-ref a 'z) (slot-ref b 'z))))
(define-method (+ (a <vec3>) (b <number>))
  (vec3 (+ (slot-ref a 'x) b)
        (+ (slot-ref a 'y) b)
        (+ (slot-ref a 'z) b)))
(define-method (+ (a <number>) (b <vec3>)) (+ b a))

(define-method (* (a <vec3>) (b <number>))
  (vec3 (* (slot-ref a 'x) b)
        (* (slot-ref a 'y) b)
        (* (slot-ref a 'z) b)))
(define-method (* (a <number>) (b <vec3>)) (* b a))

(define-method (- (a <vec3>) (b <vec3>))
  (vec3 (- (slot-ref a 'x) (slot-ref b 'x))
        (- (slot-ref a 'y) (slot-ref b 'y))
        (- (slot-ref a 'z) (slot-ref b 'z))))
(define-method (- (a <vec3>) (b <number>))
  (vec3 (- (slot-ref a 'x) b)
        (- (slot-ref a 'y) b)
        (- (slot-ref a 'z) b)))
(define-method (- (a <number>) (b <vec3>))
  (vec3 (- a (slot-ref b 'x))
        (- a (slot-ref b 'y))
        (- a (slot-ref b 'z))))
(define-method (- (a <vec3>))
  (vec3 (- (slot-ref a 'x))
        (- (slot-ref a 'y))
        (- (slot-ref a 'z))))

(define-method (/ (a <vec3>) (b <number>))
  (vec3 (/ (slot-ref a 'x) b)
        (/ (slot-ref a 'y) b)
        (/ (slot-ref a 'z) b)))

(define-method (cross (a <vec3>) (b <vec3>))
  (vec3 (- (* (.y a) (.z b)) (* (.z a) (.y b)))
        (- (* (.z a) (.x b)) (* (.x a) (.z b)))
        (- (* (.x a) (.y b)) (* (.y a) (.x b)))))
(export cross)

(define-method (norm (a <vec3>))
  (sqrt (+ (expt (.x a) 2)
           (expt (.y a) 2)
           (expt (.z a) 2))))
(export norm)

(define-method (dot (a <vec3>) (b <vec3>))
  (+ (* (.x a) (.x b))
     (* (.y a) (.y b))
     (* (.z a) (.z b))))
(export dot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(read-hash-extend #\[
  (lambda (chr port)
    (define (eat-whitespace port)
      (when (char-whitespace? (lookahead-char port))
        (read-char port)
        (eat-whitespace port)))

    (define x (read port))
    (define y (read port))

    (eat-whitespace port)
    (if (eq? (lookahead-char port) #\])
      (begin (read-char port) `(vec2 ,x ,y))
      (let ((z (read port)))
        (eat-whitespace port)
        (if (eq? (lookahead-char port) #\])
          (begin (read-char port) `(vec3 ,x ,y ,z))
          (error "Expected terminating #\\]"))))))

(export .x .y .z)

(define-public (vec-constructor . args)
  (cond ((= 2 (length args)) (apply vec2 args))
        ((= 3 (length args)) (apply vec3 args))
        (else (error "Wrong number of arguments for vector"))))

(define-public (vec3? v)
    (eq? (class-name (class-of v)) '<vec3>))

(define-public (vec2? v)
    (eq? (class-name (class-of v)) '<vec2>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (vec2->ffi v)
  " Converts from a vec2 to a libfive_vec2 for FFI calls"
  (make-c-struct (list float float) (list (.x v) (.y v))))
(define-public (ffi->vec2 v)
  (apply vec2 (parse-c-struct v (list float float))))

(define-public (vec3->ffi v)
  " Converts from a vec3 to a libfive_vec3 for FFI calls"
  (make-c-struct (list float float float) (list (.x v) (.y v) (.z v))))
(define-public (ffi->vec3 v)
  (apply vec3 (parse-c-struct v (list float float))))
