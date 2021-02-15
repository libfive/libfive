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
(define-module (libfive stdlib shapes))

(use-modules
  (libfive kernel)
  (libfive vec)
  (libfive stdlib csg)
  (libfive stdlib transforms)
)

(define-public pi 3.14159265359)

(define* (circle r #:optional (center #[0 0]))
  "circle r [#[x y]]
  A 2D circle with the given radius and optional center"
  (move (lambda-shape (x y z) (- (sqrt (+ (square x) (square y))) r)) center))
(export circle)

(define* (ring ro ri #:optional (center #[0 0]))
  "ring ro ri [#[x y]]
  A 2D ring with the given outer/inner radii and optional center"
  (difference (circle ro center) (circle ri center)))
(export ring)

(define* (polygon r n #:optional (center #[0 0]))
  "polygon r n [#[x y]]
  A polygon with center-to-vertex distance r and n sides"
  (let* ((r (* r (cos (/ pi n))))
         (half (lambda-shape (x y z) (- y r))))
    (move (apply intersection
      (map (lambda (i) (rotate-z half (* 2 pi i (/ n))))
           (iota n))) center)))
(export polygon)

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
         (circle r (+ a r))
         (circle r (- b r))
         (circle r (vec2 (+ (.x a) r) (- (.y b) r)))
         (circle r (vec2 (- (.x b) r) (+ (.y a) r)))))

(define* (rectangle-centered-exact b #:optional (c #[0 0 0]))
  "rectangle-centered-exact #[width height]
  Constructs an exact-field rectangle at the origin"
  (move
    (lambda-shape (x y z)
      (let ((d (- #[(abs x) (abs y)] (/ b 2))))
           (+ (min (max (.x d) (.y d)) 0)
              (norm #[(max (.x d) 0) (max (.y d) 0)]))))
  c)
)
(export rectangle-centered-exact)

(define-public (rectangle-exact a b)
  "rectangle-exact #[xmin ymin] #[xmax ymax]
  Constructs a rectangle from an exact distance field"
  (let ((size (- b a))
        (center (/ (+ a b) 2)))
    (rectangle-centered-exact size center)))

(define-public (rounded-rectangle-exact a b r)
  "rounded-rectangle-exact #[xmin ymin] #[xmax ymax] r
  A rectangle with rounded corners, with an exact distance field"
  (offset (rectangle-exact (+ a r) (- b r)) r))

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

(define-public (box-mitred a b)
  "box-mitred #[xmin ymin zmin] #[xmax ymax zmax]
  A box with the given bounds, that will stay creased
  as it's expanded by other functions"
  (extrude-z (rectangle a b) (.z a) (.z b)))
(define-public cube box-mitred)
(define-public box box-mitred)

(define* (box-mitred-centered s #:optional (m #[0 0 0]))
  "box-mitred-centered #[xsize ysize zsize] [#[x0 y0 z0]]
  A box with the given size, centered around the given point,
  that will stay creased as it's expanded by other functions"
  (box-mitred (- m (/ s 2)) (+ m (/ s 2)))
)
(export box-mitred-centered)
(define-public box-centered box-mitred-centered)

(define* (box-exact-centered size #:optional (orig #[0 0 0]))
  "box-exact-centered #[xsize ysize zsize] [#[x0 y0 z0]]
  A box with the given size, centered around the given point,
  that will become rounder as it's expanded by other functions"
  (lambda-shape (x y z)
    (let (
      (dx (- (abs (- x (.x orig))) (/ (.x size) 2)))
      (dy (- (abs (- y (.y orig))) (/ (.y size) 2)))
      (dz (- (abs (- z (.z orig))) (/ (.z size) 2)))
    )
      (+
        (min 0 (max dx dy dz))
        (norm #[(max dx 0) (max dy 0) (max dz 0)])
      )
    )
  )
)
(export box-exact-centered)

(define-public (box-exact bmin bmax)
  "box-exact #[xmin ymin zmin] #[xmax ymax zmax]
  A box with the given bounds, that will become rounder
  as it's expanded by other functions"
  (box-exact-centered (- bmax bmin) (/ (+ bmax bmin) 2))
)

(define* (sphere r #:optional (center #[0 0 0]))
  "sphere r [#[x y z]]
  A sphere with the given radius and optional center"
  (move (lambda-shape (x y z) (- (sqrt (+ (square x) (square y) (square z))) r))
        center))
(export sphere)

(define* (half-space norm #:optional (pos #[0 0 0]))
  "half-space #[nx ny nz] [#[px py pz]]
  A plane that divides the world into inside and outside
  Arguments are the plane normal and optional position"
  (lambda-shape (x y z)
    (dot (- #[x y z] pos) norm)))
(export half-space)

(define* (cylinder-z r h #:optional (base #[0 0 0]))
  "cylinder-z r h [#[x0 y0 z0]]
  A cylinder (oriented along the Z axis)"
  (extrude-z (circle r base) (.z base) (+ h (.z base))))
(export cylinder-z)
(define-public cylinder cylinder-z)

(define* (cone-ang-z a height #:optional (base #[0 0 0]))
  "cone-ang-z a height [#[x y z]]
  Creates a cone from a radius, slope angle, and optional base location"
  (sequence
    (lambda-shape (x y z)
      (max
        (dot #[(cos a) (sin a)] #[(norm #[x y]) (- z height)])
        (- z)
      )
    )
    (move base)
  )
)
(export cone-ang-z)
(define-public cone-ang cone-ang-z)

(define* (cone-z r height #:optional (base #[0 0 0]))
  "cone-z r height [#[x y z]]
  Creates a cone from a radius, height, and optional base location"
  (cone-ang-z (atan r height) height base))
(export cone-z)
(define-public cone cone-z)

(define-public (pyramid-z a b zmin height)
  "pyramid-z #[xmin ymin] #[xmax ymax] zmin dz
  Creates a pyramid from a base rectangle, lower z value and height"
  (taper-xy-z (extrude-z (rectangle a b) zmin (+ zmin height))
              (vec3 (/ (+ a b) 2) zmin) height 0))
(define-public pyramid pyramid-z)

(define* (torus-z R r #:optional (center #[0 0 0]))
  "torus-z R r [#[x y z]]
  Create a torus from the given outer radius, inner radius, and optional center"
  (define (c a b) (sqrt (+ (square a) (square b))))
  (move (lambda-shape (x y z) (- (c (- R (c x y)) z) r))
        center))
(export torus-z)
(define-public torus torus-z)

(define-public (gyroid period thickness)
  "gyroid #[x_period y_period z_period] thickness
Create a volume-filling gyroid with the given periods and thickness"
  (let* (
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
                         ) (- thickness)
                       )
    ))

(define-public (rounded-box a b r)
  "rounded-box #[xmin ymin zmin] #[xmax ymax zmax] r
  Rounded box with the given bounds and radius (as a 0-1 fraction)"
  (define d (- b a))
  (define r' (* r (min (.x d) (.y d) (.z d)) (/ 2)))

  (define corners (apply union
    (map (lambda (i) (sphere r' #[
        (if (logbit? 0 i) (+ (.x a) r') (- (.x b) r'))
        (if (logbit? 1 i) (+ (.y a) r') (- (.y b) r'))
        (if (logbit? 2 i) (+ (.z a) r') (- (.z b) r'))]))
      (iota 8))))

  ;; Constructs a set of 4 edges for the given lengths
  (define (edges x y z)
    (apply union
      (map (lambda (i) (cylinder-z r' (- z (* 2 r')) #[
          (if (logbit? 0 i) r' (- x r'))
          (if (logbit? 1 i) r' (- y r'))
          r']))
        (iota 4))))

  (define edges-x (reflect-xz
    (edges (.z d) (.y d) (.x d))))

  (define edges-y (reflect-yz
    (edges (.x d) (.z d) (.y d))))

  (define edges-z
    (edges (.x d) (.y d) (.z d)))

  (union
    corners
    (move (union edges-z edges-y edges-x) a)
    (extrude-z
      (union (rectangle #[(+ (.x a) r') (.y a)]
                        #[(- (.x b) r') (.y b)])
             (rectangle #[(.x a) (+ (.y a) r')]
                        #[(.x b) (- (.y b) r')]))
      (+ (.z a) r') (- (.z b) r'))
    (extrude-z (rectangle (+ a r') (- b r'))
      (.z a) (.z b))
  ))
(define-public rounded-cube rounded-box)

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
  (apply union (map (lambda (i) (rotate-z shape (* 2 pi (/ i n)) c)) (iota n))))
(export array-polar)

(define-public (emptiness)
  "emptiness
  Returns a shape which is empty everywhere"
  (lambda-shape (x y z) 1))
