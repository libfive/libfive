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
(define-module (ao shapes))

(use-modules (ao operators) (ao transforms) (ao csg) (ao bounds))

(define-public pi 3.1415926)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2D shapes

(define-public (circle center r)
    "circle '(x y) r
    Constructs a circle from a center and radius"
    (move (set-bounds (lambda (x y z) (- (sqrt (+ (square x) (square y))) r))
                      (list (- r) (- r)) (list r r))
          center))

(define-public (rectangle a b)
    "rectangle '(xmin ymin) '(xmax ymax)
    Constructs a rectangle from two lists representing its corners"
    (let* ((xa (car a))
           (xb (car b))
           (ya (cadr a))
           (yb (cadr b))
           (xmin (min xa xb))
           (xmax (max xa xb))
           (ymin (min ya yb))
           (ymax (max ya yb)))
    (set-bounds (lambda (x y z)
            (max (- xmin x) (- x xmax)
            (- ymin y) (- y ymax)))
        (list xmin ymin) (list xmax ymax))))

(define-public (rounded-rectangle a b r)
    "rounded-rectangle '(xmin ymin) '(xmax ymax) r
    Constructs a rounded rectangle from two  lists representing corners
    and a value r in the range 0 to 1"
    (let* ((xa (car a))
           (xb (car b))
           (ya (cadr a))
           (yb (cadr b))
           (xmin (min xa xb))
           (xmax (max xa xb))
           (ymin (min ya yb))
           (ymax (max ya yb))
           (r (* r (min (- xmax xmin) (- ymax ymin)) 0.5)))
    (union (rectangle (list xmin (+ ymin r)) (list xmax (- ymax r)))
           (rectangle (list (+ xmin r) ymin) (list (- xmax r) ymax))
           (circle (list (+ xmin r) (+ ymin r)) r)
           (circle (list (+ xmin r) (- ymax r)) r)
           (circle (list (- xmax r) (+ ymin r)) r)
           (circle (list (- xmax r) (- ymax r)) r))))


(define-public (triangle a b c)
    "triangle '(xa ya) '(xb yb) '(xc yc)
    Constructs a triangle from three corners "
    (let* ;; Find the center point of the triangle
          ((xm (/ (+  (car a)  (car b)  (car c)) 3))
           (ym (/ (+ (cadr a) (cadr b) (cadr c)) 3))

           ;; Calculate the angles of each point about the center
           (get-angle (lambda (p) (atan (- (car p) xm) (- (cadr p) ym))))

           ;; Extract the three angles
           (ta (get-angle a))
           (tb (get-angle b))
           (tc (get-angle c))

           ;; Rotate the points so that the smallest angle is first
           (sorted (cond ((and (< tb ta) (< tb tc)) (list tb tc ta))
                         ((and (< tc ta) (< tc tb)) (list tc ta tb))
                         (else (list ta tb tc))))

           ;; If the points are in increasing order, it's a sign that
           ;; they aren't sorted in a clockwise fashion.  In this case, swap
           ;; the second and third point to make them clockwise
           (clockwise (if (> (caddr sorted) (cadr sorted))
                          (list a c b) (list a b c)))

           ;; Extract coordinates from sorted list
           (x0 (caar clockwise))   (y0 (cadr (car clockwise)))
           (x1 (caadr clockwise))  (y1 (cadr (cadr clockwise)))
           (x2 (caaddr clockwise)) (y2 (cadr (caddr clockwise)))

           ;; Function to calculate one edge of the triangle
           (edge (lambda (x0 y0 dx dy)
                 (lambda (x y z)
                    (- (* dy (- x x0)) (* dx (- y y0)))))))

          (set-bounds
          (intersection (edge x2 y2 (- x0 x2) (- y0 y2))
                        (edge x1 y1 (- x2 x1) (- y2 y1))
                        (edge x0 y0 (- x1 x0) (- y1 y0)))
          (list (min x0 x1 x2) (min y0 y1 y2))
          (list (max x0 x1 x2) (max y0 y1 y2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2D -> 3D functions

(define-public (extrude-z shape za zb)
    "extrude-z shape za zb
    Extrudes the given 2D shape between za and zb"
    (let* ((zmin (min za zb))
           (zmax (max za zb))
           (bounds (get-bounds shape))
           (out (lambda (x y z) (max (shape x y z) (- zmin z) (- z zmax)))))
    (if bounds
        (let* ((lower (car bounds))
               (upper (cadr bounds)))
        (set-bounds out (list (car lower) (cadr lower) zmin)
                        (list (car upper) (cadr upper) zmax)))
        out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3D shapes

(define-public (sphere center r)
    "sphere '(x y z) r
    Defines a sphere from a center and radius"
    (move
    (set-bounds
        (lambda (x y z) (- (sqrt (+ (square x) (square y) (square z))) r))
        (list (- r) (- r) (- r)) (list r r r))
    center))

(define-public (cube a b)
    "cube '(xmin ymin zmin) '(xmax ymax zmax)
    Defines a cube from its two corners"
    (extrude-z (rectangle a b) (caddr a) (caddr b)))

(define-public (cylinder-z base r height)
    "cylinder '(x y zmin) r dz
    Creates a circle from a base location, radius, and height"
    (let ((zmin (caddr base)))
    (extrude-z (circle base r) zmin (+ zmin height))))

(define-public (cone-z base r height)
    "cone '(x y zmin) r dz
    Creates a cone from a base location, radius, and height"
    (let ((zmin (caddr base)))
    (taper-xy-z (cylinder-z base r height)
                base zmin (+ zmin height) 1 0)))

(define-public (pyramid-z a b zmin height)
    "pyramid '(xmin ymin) '(xmax ymax) zmin dz
    Creates a pyramid from a base rectangle, lower z value and height"
    (taper-xy-z (extrude-z (rectangle a b) zmin (+ zmin height))
        (list (/ (+  (car a)  (car b)) 2)
              (/ (+ (cadr a) (cadr b)) 2))
        zmin (+ zmin height) 1 0))

(define-public (torus-z center R r)
    "torus '(x y z) R r
    Create a torus from the given center, outer radius, and inner radius"
    (let ((distance (lambda (a b)
        (sqrt (+ (* a a) (* b b))))))
    (move
    (set-bounds (lambda (x y z)
            (let ((d (distance x y)))
            (- (distance (- R d) z) r)))
        (list (- (+ R r)) (- (+ R r)) (- r))
        (list (+ R r) (+ R r) r))
    center)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Higher-level shapes

(define-public (array-2d shape i j dx dy)
    "array-2d shape i j dx dy
    Iterates a part in a 2D array.
    i and j are part counts along each axis
    dx and dy are offsets along each axis"
    (let ((a (apply union
        (map (lambda (i)
            (move shape (list (* i dx) 0 0))) (iota i)))))
    (apply union
        (map (lambda (j)
            (move a (list 0 (* j dy) 0))) (iota j)))))

(define-public (array-3d shape i j k dx dy dz)
    "array-3d shape i j k dx dy dz
    Iterates a part in a 3D array.
    i, j, k are part counts along each axis
    dx, dy, dz are offsets along each axis"
    (let*
        ((a (apply union
        (map (lambda (i)
            (move shape (list (* i dx) 0 0))) (iota i))))
        (b (apply union
        (map (lambda (j)
            (move a (list 0 (* j dy) 0))) (iota j)))))
    (apply union
        (map (lambda (k)
            (move b (list 0 0 (* k dz)))) (iota k)))))

(define-public (array-polar shape n . args)
    "array-polar shape n ['(x y)]
    Iterate a shape about an optional center position"
    (apply union
        (map (lambda (i)
            (apply rotate-z (append (list shape (* 2 pi (/ i n))) args)))
            (iota n))))
