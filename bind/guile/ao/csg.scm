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
(define-module (ao csg))

(use-modules (srfi srfi-1))
(use-modules (ao operators) (ao bounds))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (union . shapes)
    "union a [b [c [...]]]
    Returns the union of any number of shapes"
    (let ((bounds (map get-bounds shapes)))
    (if (= 0 (length shapes))
        (error "Cannot take the union of an empty list")
        (let ((out (lambda (x y z) ; New shape function!
                   (apply min (map (lambda (s) (s x y z)) shapes)))))
            (if (every (lambda (x) x) bounds)
                (let ((b (bounds-union bounds)))
                    (set-bounds out (car b) (cadr b)))
                out)))))

(define-public (intersection . shapes)
    "intersection a [b [c [...]]]
    Returns the intersection of any number of shapes"
    (let ((bounds (map get-bounds shapes)))
    (if (= 0 (length shapes))
        (error "Cannot take the intersection of an empty list")
        (let ((out (lambda (x y z) ; New shape function!
                   (apply max (map (lambda (s) (s x y z)) shapes)))))
            (if (every identity bounds)
                (let ((b (bounds-intersection bounds)))
                    (set-bounds out (car b) (cadr b)))
                out)))))

(define-public (difference a . b)
    "difference a b [c [d [...]]]
    Subtracts any number of shapes from the first argument"
    (let* ((accum (apply union b))
           (out (lambda (x y z) (max (a x y z) (-(accum x y z)))))
           (bounds (get-bounds a)))
    (if bounds (set-bounds out (car bounds) (cadr bounds))
               out)))

(define-public (offset s o)
    "offset shape o
    Expand or contract a given shape by an offset"
    (lambda (x y z) (- (s x y z) o)))

(define-public (clearance a b o)
    "clearance a b o
    Expands shape b by the given offset then subtracts it from shape a"
    (let* ((bo (offset b o))
           (out (lambda (x y z) (max (a x y z) (- (bo x y z)))))
           (bounds (get-bounds a)))
    (if bounds (set-bounds out (car bounds) (cadr bounds))
               out)))

(define-public (shell shape o)
    "shell shape o
    Returns a shell of a shape with the given offset"
    (clearance shape shape o))

(define-public (blend a b m)
    "blend a b m
    Blends two shapes by the given amount"
    (let ((joint (union a b))
          (fillet (lambda (x y z)
                    (- (+ (sqrt (abs (a x y z)))
                          (sqrt (abs (b x y z)))) m))))
    (union joint fillet)))

(define-public (morph a b m)
    "morph a b m
    Morphs between two shapes.
    m = 0 produces a, m = 1 produces b"
    (lambda (x y z) (+ (* (a x y z) (- 1 m)) (* (b x y z) m))))

