#|
Guile bindings to the Ao CAD kernel
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
(use-modules (ao kernel) (ao vec) (ao csg) (ao transforms))

(define-public (circle center r)
  "circle #[x y] r
  A 2D circle with the given center and radius"
  (move (lambda-shape (x y z) (- (sqrt (+ (square x) (square y))) r)) center))

(define-public (polygon center r n)
  "polygon #[x y] r n
  A polygon with center-to-vertex distance r and n sides"
  (let* ((r (* r (cos (/ pi n))))
         (half (lambda-shape (x y z) (- y r))))
    (move (apply intersection
      (map (lambda (i) (rotate-z half (* 2 pi i (/ n))))
           (iota n))) center)))

(define-public (rectangle a b)
  "rectangle #[xmin ymin] #[xmax ymax]
  A rectangle with the given bounding corners"
  (lambda-shape (x y z)
                (max (- (.x a) x) (- x (.x b)) (- (.y a) y) (- y (.y b)))))

(define-public (rounded-rectangle a b r)
  "rounded-rectangle #[xmin ymin] #[xmax ymax] r
  A rectangle with rounded corners"
  (union (rectangle (+ a (vec2 0 r)) (- b (vec2 0 r)))
         (rectangle (+ a (vec2 r 0)) (- b (vec2 r 0)))
         (circle (+ a r) r)
         (circle (- b r) r)
         (circle (vec2 (+ (.x a) r) (- (.y b) r)) r)
         (circle (vec2 (- (.x b) r) (+ (.y a) r)) r)))

(define-public (triangle a b c)
  "triangle #[x0 y0] #[x1 y1] #[x2 y2]
  A 2D triangle"
  (define (half-plane a b)
    (lambda-shape (x y z)
      (- (* (- (.y b) (.y a)) (- x (.x a)))
         (* (- (.x b) (.x a)) (- y (.y a))))))
  (if (< 0 (.z (cross (vec3 (- b a) 0) (vec3 (- c a) 0))))
    (intersection
      (half-plane a b) (half-plane b c) (half-plane c a))
    (intersection
      (half-plane a c) (half-plane c b) (half-plane b a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (box a b)
  "box #[xmin ymin zmin] #[xmax ymax zmax]
  A box with the given bounds"
  (extrude-z (rectangle a b) (.z a) (.z b)))

(define-public (sphere center r)
  "sphere #[x y z] r
  A sphere with the given center and radius"
  (move (lambda-shape (x y z) (- (sqrt (+ (square x) (square y) (square z))) r))
        center))

(define-public (cylinder-z base r h)
  "cylinder-z #[x0 y0 z0] r h
  A cylinder (oriented along the Z axis) "
  (extrude-z (circle base r) (.z base) (+ h (.z base))))
(define-public cylinder cylinder-z)

(define-public (cone-z base r height)
  "cone-z #[x y z] r height
  Creates a cone from a base location, radius, and height"
  (taper-xy-z (cylinder-z base r height) base height 0))
(define-public cone cone-z)

(define-public (pyramid-z a b zmin height)
  "pyramid-z #[xmin ymin] #[xmax ymax] zmin dz
  Creates a pyramid from a base rectangle, lower z value and height"
  (taper-xy-z (extrude-z (rectangle a b) zmin (+ zmin height))
              (vec3 (/ (+ a b) 2) zmin) height 0))
(define-public pyramid pyramid-z)

(define-public (torus-z center R r)
  "torus-z #[x y z] R r
  Create a torus from the given center, outer radius, and inner radius"
  (define (c a b) (sqrt (+ (square a) (square b))))
  (move (lambda-shape (x y z) (- (c (- R (c x y)) z) r))
        center))
(define-public torus torus-z)

(define-public (gyroid period thickness)
  "gyroid #[x_period y_period z_period] thickness
Create a volume filling gyroid with the given periods and thickness"
  (let* (
         (pi 3.14159265359)
         (tau (* 2 pi))
         (x_factor (/ tau (.x period)))
         (y_factor (/ tau (.y period)))
         (z_factor (/ tau (.z period)))
         )
    (shell (lambda-shape (x y z)
                         (+
                          (* (sin (/ x x_factor))
                             (cos (/ y y_factor))
                             )
                          (* (sin (/ y y_factor))
                             (cos (/ z z_factor))
                             )
                          (* (sin (/ z z_factor))
                             (cos (/ x x_factor))
                             )
                          )
                         ) thickness
                           )
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Higher-order shapes

(define-public (array-x shape i dx)
  "array-x shape i dx
  Iterates a part in a 1D array.
  i is the number of repetitions
  dx is the offset on the X axis"
  (apply union (map (lambda (i) (move shape (* (vec2 dx 0) i))) (iota i))))

(define-public (array-xy shape i j dxy)
  "array-xy shape i j #[dx dy]
  Iterates a part in a 2D array.
  i and j are part counts along each axis
  dxy is a vec2 representing offsets along each axis"
  (define x (array-x shape i (.x dxy)))
  (apply union (map (lambda (j) (move x (* (vec2 0 (.y dxy)) j))) (iota j))))

(define-public (array-xyz shape i j k dxyz)
  "array-xyz shape i j k #[dx dy dz]
  Iterates a part in a 2D array.
  i, j, k are part counts along each axis
  dxyz is a vec3 representing offsets along each axis"
  (define xy (array-xy shape i j dxyz))
  (apply union (map (lambda (k)
                      (move xy (* (vec3 0 0 (.z dxyz)) k))) (iota k))))

(define-public (extrude-z shape zmin zmax)
  "extrude-z shape zmin zmax
  Extrudes a 2D shape between za and zb"
  (max shape (lambda-shape (x y z) (max (- zmin z) (- z zmax)))))
(define-public extrude extrude-z)

(define* (array-polar shape n #:optional (c #[0 0]))
  "array-polar shape n [#[x y]]
  Iterates a shape about an optional center position"
  (define pi 3.1415926)
  (apply union (map (lambda (i) (rotate-z shape (* 2 pi (/ i n)) c)) (iota n))))
(export array-polar)
