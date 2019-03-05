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
(use-modules (libfive kernel) (libfive vec) (libfive csg))

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

(define-public (symmetric-x shape)
  "symmetric-x shape
  Clip the given shape at the x origin,
  and duplicate the remaining shape reflected
  on the other side of the origin"
  (remap-shape (shape x y z) (abs x) y z)
)
(define-public (symmetric-y shape)
  "symmetric-y shape
  Clip the given shape at the y origin,
  and duplicate the remaining shape reflected
  on the other side of the origin"
  (remap-shape (shape x y z) x (abs y) z)
)
(define-public (symmetric-z shape)
  "symmetric-z shape
  Clip the given shape at the z origin,
  and duplicate the remaining shape reflected
  on the other side of the origin"
  (remap-shape (shape x y z) x y (abs z))
)

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

(define* (scale-xyz shape s #:optional (center #[0 0 0]))
  "scale-xyz shape #[sx sy sz] [#[x0 y0 z0]]
  Scales a shape on all three axes, about 0 or an optional offset"
  (remap-shape (shape x y z) (+ (.x center) (/ (- x (.x center)) (.x s))) (+ (.y center) (/ (- y (.y center)) (.y s))) (+ (.z center) (/ (- z (.z center)) (.z s)))))

(export scale-x scale-y scale-z scale-xyz)

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

(define (attractrepel-generic shape locus r e signfunc remapfunc)
  (define (falloff point)
    (signfunc 1 (* e (exp (- (/ (norm point) r)))))
  )
  (let ((shapep (move shape (- locus))))
    (move
      (remapfunc shapep falloff)
    locus)
  )
)

(define (attractrepel-remapfunc-xyz shapep falloff)
  (remap-shape
    (shapep x y z)
    (* x (falloff (vec3 x y z)))
    (* y (falloff (vec3 x y z)))
    (* z (falloff (vec3 x y z)))
  )
)

(define (attractrepel-remapfunc-x shapep falloff)
  (remap-shape
    (shapep x y z)
    (* x (falloff (vec3 x 0 0)))
    y
    z
  )
)

(define (attractrepel-remapfunc-y shapep falloff)
  (remap-shape
    (shapep x y z)
    x
    (* y (falloff (vec3 0 y 0)))
    z
  )
)

(define (attractrepel-remapfunc-z shapep falloff)
  (remap-shape
    (shapep x y z)
    x
    y
    (* z (falloff (vec3 0  0 z)))
  )
)

(define (attractrepel-remapfunc-xy shapep falloff)
  (remap-shape
    (shapep x y z)
    (* x (falloff (vec3 x y 0)))
    (* y (falloff (vec3 x y 0)))
    z
  )
)

(define (attractrepel-remapfunc-yz shapep falloff)
  (remap-shape
    (shapep x y z)
    x
    (* y (falloff (vec3 0 y z)))
    (* z (falloff (vec3 0 y z)))
  )
)

(define (attractrepel-remapfunc-xz shapep falloff)
  (remap-shape
    (shapep x y z)
    (* x (falloff (vec3 x 0 z)))
    y
    (* z (falloff (vec3 x 0 z)))
  )
)


(define* (repel shape locus r #:optional (e 1))
  "repel shape #[x0 y0 z0] radius [exaggeration]
  Repels the shape away from a point based upon a radius r,
  with optional exaggeration e"
  (attractrepel-generic shape locus r e - attractrepel-remapfunc-xyz)
)
(export repel)

(define* (repel-x shape locus r #:optional (e 1))
  "repel-x shape #[x0 y0 z0] radius [exaggeration]
  Repels the shape away from a plane on the yz axes,
  based upon a radius r, with optional exaggeration e"
  (attractrepel-generic shape locus r e - attractrepel-remapfunc-x)
)
(export repel-x)

(define* (repel-y shape locus r #:optional (e 1))
  "repel-y shape #[x0 y0 z0] radius [exaggeration]
  Repels the shape away from a plane on the xz axes,
  based upon a radius r, with optional exaggeration e"
  (attractrepel-generic shape locus r e - attractrepel-remapfunc-y)
)
(export repel-y)

(define* (repel-z shape locus r #:optional (e 1))
  "repel-z shape #[x0 y0 z0] radius [exaggeration]
  Repels the shape away from a plane on the xy axes,
  based upon a radius r, with optional exaggeration e"
  (attractrepel-generic shape locus r e - attractrepel-remapfunc-z)
)
(export repel-z)

(define* (repel-xy shape locus r #:optional (e 1))
  "repel-xy shape #[x0 y0 z0] radius [exaggeration]
  Repels the shape away from a line parallel to the the z axis,
  based upon a radius r, with optional exaggeration e"
  (attractrepel-generic shape locus r e - attractrepel-remapfunc-xy)
)
(export repel-xy)

(define* (repel-yz shape locus r #:optional (e 1))
  "repel-yz shape #[x0 y0 z0] radius [exaggeration]
  Repels the shape away from a line parallel to the the x axis,
  based upon a radius r, with optional exaggeration e"
  (attractrepel-generic shape locus r e - attractrepel-remapfunc-yz)
)
(export repel-yz)

(define* (repel-xz shape locus r #:optional (e 1))
  "repel-xz shape #[x0 y0 z0] radius [exaggeration]
  Repels the shape away from a line parallel to the the y axis,
  based upon a radius r, with optional exaggeration e"
  (attractrepel-generic shape locus r e - attractrepel-remapfunc-xz)
)
(export repel-xz)


(define* (attract shape locus r #:optional (e 1))
  "attract shape #[x0 y0 z0] radius [exaggeration]
  Attracts the shape towards a point based upon a radius r,
  with optional exaggeration e"
  (attractrepel-generic shape locus r e + attractrepel-remapfunc-xyz)
)
(export attract)

(define* (attract-x shape locus r #:optional (e 1))
  "attract-x shape #[x0 y0 z0] radius [exaggeration]
  Attracts the shape towards a plane on the yz axes,
  based upon a radius r, with optional exaggeration e"
  (attractrepel-generic shape locus r e + attractrepel-remapfunc-x)
)
(export attract-x)

(define* (attract-y shape locus r #:optional (e 1))
  "attract-y shape #[x0 y0 z0] radius [exaggeration]
  Attracts the shape towards a plane on the xz axes,
  based upon a radius r, with optional exaggeration e"
  (attractrepel-generic shape locus r e + attractrepel-remapfunc-y)
)
(export attract-y)

(define* (attract-z shape locus r #:optional (e 1))
  "attract-z shape #[x0 y0 z0] radius [exaggeration]
  Attracts the shape towards a plane on the xy axes,
  based upon a radius r, with optional exaggeration e"
  (attractrepel-generic shape locus r e + attractrepel-remapfunc-z)
)
(export attract-z)

(define* (attract-xy shape locus r #:optional (e 1))
  "attract-xy shape #[x0 y0 z0] radius [exaggeration]
  Attracts the shape towards a line parallel to the the z axis,
  based upon a radius r, with optional exaggeration e"
  (attractrepel-generic shape locus r e + attractrepel-remapfunc-xy)
)
(export attract-xy)

(define* (attract-yz shape locus r #:optional (e 1))
  "attract-yz shape #[x0 y0 z0] radius [exaggeration]
  Attracts the shape towards a line parallel to the the x axis,
  based upon a radius r, with optional exaggeration e"
  (attractrepel-generic shape locus r e + attractrepel-remapfunc-yz)
)
(export attract-yz)

(define* (attract-xz shape locus r #:optional (e 1))
  "attract-xz shape #[x0 y0 z0] radius [exaggeration]
  Attracts the shape towards a line parallel to the the y axis,
  based upon a radius r, with optional exaggeration e"
  (attractrepel-generic shape locus r e + attractrepel-remapfunc-xz)
)
(export attract-xz)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Twirls

(define (generic-centered-twirl-x shape a r vecmaker)
  (define (falloff point) (exp (- (/ (norm point) r))) )
  (define (fa op a point) (op (* a (falloff point))) )
  (remap-shape shape (x y z)
    x
    (+
      (* (fa cos a (vecmaker x y z)) y)
      (* (fa sin a (vecmaker x y z)) z)
    )
    (+
      (* (- (fa sin a (vecmaker x y z))) y)
      (*    (fa cos a (vecmaker x y z))  z)
    )
  )
)

(define (centered-twirl-x shape a r)
  (generic-centered-twirl-x shape a r vec3)
)

(define (centered-twirl-axis-x shape a r)
  (generic-centered-twirl-x shape a r (lambda (x y z) (vec2 y z)))
)

(define (generic-twirl-n shape a r center method axisremap)
  (sequence
    shape
    (move (- center))
    (axisremap)
    (method a r)
    (axisremap)
    (move center)
  )
)

(define (generic-twirl-x shape a r center method)
  (generic-twirl-n shape a r center method (lambda (subshape) subshape))
)

(define (generic-twirl-y shape a r center method)
  (generic-twirl-n shape a r center method reflect-xy)
)

(define (generic-twirl-z shape a r center method)
  (generic-twirl-n shape a r center method reflect-xz)
)

(define* (twirl-x shape a r #:optional (center #[0 0 0]))
  "twirl-x shape amount radius [center]
  Twirls the shape in the x axis
  around the optional center point"
  (generic-twirl-x shape a r center centered-twirl-x)
)
(export twirl-x)

(define* (twirl-axis-x shape a r #:optional (center #[0 0 0]))
  "twirl-axis-x shape amount radius [center]
  Twirls the shape in the x axis
  around the line extending from
  the center point"
  (generic-twirl-x shape a r center centered-twirl-axis-x)
)
(export twirl-axis-x)

(define* (twirl-y shape a r #:optional (center #[0 0 0]))
  "twirl-y shape amount radius [center]
  Twirls the shape in the y axis
  around the optional center point"
  (generic-twirl-y shape a r center centered-twirl-x)
)
(export twirl-y)

(define* (twirl-axis-y shape a r #:optional (center #[0 0 0]))
  "twirl-axis-y shape amount radius [center]
  Twirls the shape in the y axis
  around the line extending from
  the center point"
  (generic-twirl-y shape a r center centered-twirl-axis-x)
)
(export twirl-axis-y)

(define* (twirl-z shape a r #:optional (center #[0 0 0]))
  "twirl-z shape amount radius [center]
  Twirls the shape in the z axis
  around the optional center point"
  (generic-twirl-z shape a r center centered-twirl-x)
)
(export twirl-z)

(define* (twirl-axis-z shape a r #:optional (center #[0 0 0]))
  "twirl-axis-z shape amount radius [center]
  Twirls the shape in the y axis
  around the line extending from
  the center point"
  (generic-twirl-z shape a r center centered-twirl-axis-x)
)
(export twirl-axis-z)
