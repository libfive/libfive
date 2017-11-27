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
(use-modules (ao kernel) (ao vec) (ao csg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translation
(define-public (move shape delta)
  "move shape #[dx dy [dz]]
  Moves the given shape in 2D or 3D space"
  (remap-shape (shape x y z)
    (- x (.x delta))
    (- y (.y delta))
    (- z (catch #t (lambda ()(.z delta)) (lambda (. _) 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reflections
(define* (reflect-x shape #:optional (x0 0))
  "reflect-x shape [x0]
  Reflect the given shape about the x origin or an optional offset"
  (remap-shape (shape x y z) (- (* 2 x0) x) y z))

(define* (reflect-y shape #:optional (y0 0))
  "reflect-y shape [y0]
  Reflect the given shape about the y origin or an optional offset"
  (remap-shape (shape x y z) x (- (* 2 y0) y) z))

(define* (reflect-z shape #:optional (z0 0))
  "reflect-z shape [z0]
  Reflect the given shape about the z origin or an optional offset"
  (remap-shape (shape x y z) x y (- (* 2 z0) z)))

(define-public (reflect-xy shape)
  "reflect-xy shape
  Moves the given shape across the plane Y=X"
  (remap-shape (shape x y z) y x z))

(define-public (reflect-yz shape)
  "reflect-xy shape
  Moves the given shape across the plane Y=Z"
  (remap-shape (shape x y z) x z y))

(define-public (reflect-xz shape)
  "reflect-xz shape
  Moves the given shape across the plane X=Z"
  (remap-shape (shape x y z) z y x))

(export reflect-x reflect-y reflect-z)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scaling
(define* (scale-x shape sx #:optional (x0 0))
  "scale-x shape sx [x0]
  Scales a shape by sx on the x axis about 0 or an optional offset"
  (remap-shape (shape x y z) (+ x0 (/ (- x x0) sx)) y z))

(define* (scale-y shape sy #:optional (y0 0))
  "scale-y shape sy [y0]
  Scales a shape by sy on the y axis about 0 or an optional offset"
  (remap-shape (shape x y z) x (+ y0 (/ (- y y0) sy)) z))

(define* (scale-z shape sz #:optional (z0 0))
  "scale-z shape sz [z0]
  Scales a shape by sz on the z axis about 0 or an optional offset"
  (remap-shape (shape x y z) x y (+ z0 (/ (- z z0) sz))))

(export scale-x scale-y scale-z)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rotation
(define* (rotate-x shape angle #:optional (center #[0 0 0]))
  "rotate-x shape angle [#[x0 y0 z0]]
  Rotate the given shape by an angle in radians
  The center of rotation is #[0 0 0] or specified by the optional argument"
  (when (not center) (set! center #(0 0 0)))
  (let ((centered (move shape (- center)))
        (ca (cos angle))
        (sa (sin angle)))
    (move (remap-shape (centered x y z)
          x
          (+ (* ca y) (* sa z))
          (+ (* (- sa) y) (* ca z))) center)))

(define* (rotate-y shape angle #:optional (center #[0 0 0]))
  "rotate-y shape angle [#[x0 y0 z0]]
  Rotate the given shape by an angle in radians
  The center of rotation is #[0 0 0] or specified by the optional argument"
  (when (not center) (set! center #(0 0 0)))
  (let ((centered (move shape (- center)))
        (ca (cos angle))
        (sa (sin angle)))
    (move (remap-shape (centered x y z)
          (+ (* ca x) (* sa z))
          y
          (+ (* (- sa) x) (* ca z))) center)))

(define* (rotate-z shape angle #:optional (center #[0 0 0]))
  "rotate-z shape angle [#[x0 y0 z0]]
  Rotate the given shape by an angle in radians
  The center of rotation is #[0 0 0] or specified by the optional argument"
  (when (not center) (set! center #(0 0 0)))
  (let ((centered (move shape (- center)))
        (ca (cos angle))
        (sa (sin angle)))
    (move (remap-shape (centered x y z)
          (+ (* ca x) (* sa y))
          (+ (* (- sa) x) (* ca y))
          z) center)))

(define rotate rotate-z)
(export rotate-x rotate-y rotate-z rotate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Distortions
(define* (taper-x-y shape base h scale #:optional (base-scale 1))
  "taper-x-y shape #[x0 y0] height scale [base-scale]
  Tapers a shape along the x axis as a function of y
  width = base-scale at base
  width = scale at base + #[0 h]"
  (move (remap-shape ((move shape (- base)) x y z)
    (* x (/ h (+ (* scale y) (* base-scale (- h y)))))
    y z) base))
(export taper-x-y)

(define* (taper-xy-z shape base h scale #:optional (base-scale 1))
  "taper-xy-z shape #[x0 y0 z0] height scale [base-scale]
  Tapers a shape in the xy plane as a function of z
  width = base-scale at base
  width = scale at base + #[0 0 h]"
  (define (s z) (/ h (+ (* scale z) (* base-scale (- h z)))))
  (move (remap-shape ((move shape (- base)) x y z)
    (* x (s z))
    (* y (s z))
    z) base))
(export taper-xy-z)

(define* (shear-x-y shape base h offset #:optional (base-offset 0))
  "shear-x-y shape #[x0 y0] height offset [base-offset]
  Shears a shape on the x axis as a function of y
  offset = base-offset at base.y
  offset = offset = base.y + h"
  (remap-shape (shape x y z)
    (let ((f (/ (- y (.y base)) h)))
      (- x (* base-offset (- 1 f)) (* offset f)))
    y z))
(export shear-x-y)

(define-public (repel shape locus r)
  "repel shape #[x0 y0 z0] radius
  Repels the shape away from a point based upon a radius r"
  (define (falloff point)
    (- 1 (exp (- (/ (norm point) r)))))
  (let ((shapep (move shape (- locus))))
    (move
     (remap-shape
      (shapep x y z)
      (* x (falloff (vec3 x y z)))
      (* y (falloff (vec3 x y z)))
      (* z (falloff (vec3 x y z)))
      )
     locus)
    )
  )

(define-public (attract shape locus r)
  "attract shape #[x0 y0 z0] radius
  Attracts the shape towards a point based upon a radius r"
  (define (falloff point)
    (+ 1 (exp (- (/ (norm point) r)))))
  (let ((shapep (move shape (- locus))))
    (move
     (remap-shape
      (shapep x y z)
      (* x (falloff (vec3 x y z)))
      (* y (falloff (vec3 x y z)))
      (* z (falloff (vec3 x y z)))
      )
     locus)
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (revolve-y shape #:optional (x0 0))
  "revolve-y shape [x0]
  Revolves a 2D (xy) shape around the vertical line x=x0"
  (union
    (remap-shape (shape x y z)
      (sqrt (+ (square x) (square z))) y z)
    (remap-shape (shape x y z)
      (- (sqrt (+ (square x) (square z)))) y z)))
(export revolve-y)
