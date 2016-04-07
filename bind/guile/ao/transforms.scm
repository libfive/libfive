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
(define-module (ao transforms))

(use-modules (ao operators) (ao bounds) (ao jit) (ao bind))

(define (apply-affine v)
    (lambda (x y z)
    (+ (* (car v) x) (* (cadr v) y) (* (caddr v) z) (cadddr v))))

(define-syntax-rule (apply-transform shape coords fx fy fz)
    "apply-transform shape coords fx fy fz
    Applies a generic transform to a shape
        coords is usually (x, y, z)
        fx, fy, and fz are modified coordinates
            (and must use only variables from coords, for hygiene)
        If fx, fy, and fz are affine and shape has bounds, the output
        will also have attached bounds"
    (let ((vx (get-affine-vec (lambda coords fx)))
          (vy (get-affine-vec (lambda coords fy)))
          (vz (get-affine-vec (lambda coords fz)))
          (bounds (get-bounds shape))
          (out (lambda coords (shape fx fy fz))))

    ; If we have bounds and an affine transform, then we should calculate
    ; the new bounding box and apply it to the output shape
    (if (and vx vy vz bounds)
        (let* (; Get bounds from the bounds list
               (lower (car bounds))
               (upper (cadr bounds))
               (X (cons (car lower)   (car upper)))
               (Y (cons (cadr lower)  (cadr upper)))
               (Z (cons (caddr lower) (caddr upper)))

               ; Get matrix inverse and affine functions
               (inverse (matrix-invert vx vy vz))
               (ix (apply-affine (car inverse)))
               (iy (apply-affine (cadr inverse)))
               (iz (apply-affine (caddr inverse)))

               ; Get inverted bounds
               (Xi ((jit-function ix) X Y Z))
               (Yi ((jit-function iy) X Y Z))
               (Zi ((jit-function iz) X Y Z))

               (new-min (list (car Xi) (car Yi) (car Zi)))
               (new-max (list (cdr Xi) (cdr Yi) (cdr Zi))))

            ;; Apply this inverse to intervals
            ;; Set new bounds on shape
            (set-bounds out new-min new-max))
        out)))

(export apply-transform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (move shape delta)
    "move shape '(dx dy [dz=0])
    Moves a shape by the given offset"
    (let ((dx   (car delta))
          (dy  (cadr delta))
          (dz (if (>= (length delta) 3) (caddr delta) 0)))
    (apply-transform shape (x y z) (- x dx) (- y dy) (- z dz))))

(define-public (rotate-x shape angle . args)
    "rotate-x shape angle ['(x0 y0 z0)]
    Rotate the given shape by an angle in radians
    The center of rotation is 0,0,0 or specified by the optional argument"
    (let* ((argc (length args))
           (y0 (if (> argc 1) (cadar  args) 0))
           (z0 (if (> argc 2) (caddar args) 0))
           (ca (cos angle))
           (sa (sin angle))
           (centered (move shape (list 0 (- y0) (- z0)))))
    (move (apply-transform centered (x y z)
            x (+ (* ca y) (* sa z))
              (+ (* (- sa) y) (* ca z)))
        (list 0 y0 z0))))

(define-public (rotate-y shape angle . args)
    "rotate-y shape angle ['(x0 y0 z0)]
    Rotate the given shape by an angle in radians
    The center of rotation is 0,0,0 or specified by the optional argument"
    (let* ((argc (length args))
           (x0 (if (> argc 0) (caar  args) 0))
           (z0 (if (> argc 2) (caddar args) 0))
           (ca (cos angle))
           (sa (sin angle))
           (centered (move shape (list (- x0) 0 (- z0)))))
    (move (apply-transform centered (x y z)
            (+ (* ca x) (* sa z)) y
            (+ (* (- sa) x) (* ca z)))
        (list x0 0 z0))))

(define-public (rotate-z shape angle . args)
    "rotate-z shape angle ['(x0 y0 z0)]
    Rotate the given shape by an angle in radians
    The center of rotation is 0,0,0 or specified by the optional argument"
    (let* ((argc (length args))
           (x0 (if (> argc 0) (caar  args) 0))
           (y0 (if (> argc 1) (cadar args) 0))
           (ca (cos angle))
           (sa (sin angle))
           (centered (move shape (list (- x0) (- y0)))))
    (move (apply-transform centered (x y z)
            (+ (* ca x) (* sa y))
            (+ (* (- sa) x) (* ca y)) z)
        (list x0 y0))))

(define-public (reflect-x shape . args)
    "reflect-x shape [x0]
    Reflect the given shape about the x origin or an optional offset"
    (let ((x0 (if (> (length args) 0) (car args) 0)))
    (apply-transform shape (x y z) (- (* 2 x0) x) y z)))

(define-public (reflect-y shape . args)
    "reflect-y shape [y0]
    Reflect the given shape about the y origin or an optional offset"
    (let ((y0 (if (> (length args) 0) (car args) 0)))
    (apply-transform shape (x y z) x (- (* 2 y0) y) z)))

(define-public (reflect-z shape . args)
    "reflect-z shape [z0]
    Reflect the given shape about the z origin or an optional offset"
    (let ((z0 (if (> (length args) 0) (car args) 0)))
    (apply-transform shape (x y z) x y (- (* 2 z0) z))))

(define-public (reflect-xy shape)
    "reflect-xy shape
    Reflects the given shape about the line x = y"
    (apply-transform shape (x y z) y x z))

(define-public (reflect-xz shape)
    "reflect-xz shape
    Reflects the given shape about the line x = z"
    (apply-transform shape (x y z) z y x))

(define-public (reflect-yz shape)
    "reflect-yz shape
    Reflects the given shape about the line y = z"
    (apply-transform shape (x y z) x z y))

(define-public (scale-x shape sx . args)
    "scale-x shape sx [x0]
    Scales a shape by sx on the x axis about 0 or an optional offset"
    (let ((x0 (if (> (length args) 0) (car args) 0)))
    (apply-transform shape (x y z) (+ x0 (/ (- x x0) sx)) y z)))

(define-public (scale-y shape sy . args)
    "scale-y shape sy [y0]
    Scales a shape by sy on the y axis about 0 or an optional offset"
    (let ((y0 (if (> (length args) 0) (car args) 0)))
    (apply-transform shape (x y z) x (+ y0 (/ (- y y0) sy)) z)))

(define-public (scale-z shape sz . args)
    "scale-z shape sz [z0]
    Scales a shape by sz on the z axis about 0 or an optional offset"
    (let ((z0 (if (> (length args) 0) (car args) 0)))
    (apply-transform shape (x y z) x y (+ z0 (/ (- z z0) sz)))))

(define-public (shear-x-y shape ymin ymax dx0 dx1)
    "shear-x-y shape ymin ymax dx0 dx1
    Shears a shape in the XY plane.
    dx0 is the offset at ymin; dx1 is the offset at ymax"
    (apply-transform shape (x y z)
        (let ((f (/ (- y ymin) (- ymax ymin))))
        (- x (* dx0 (- 1 f)) (* dx1 f)))
        y z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-affine transforms

(define-public (taper-x-y shape x0 ymin ymax s0 s1)
    "taper-x-y shape x0 ymin ymax s0 s1
    Tapers a shape in the XY plane.
    s0 is the scale at ymin; s1 is the scale at ymax
    Scales are applied centered at x0"
    (let ((dy (- ymax ymin))
          (ds (- s1 s0)))
    (lambda (x y z)
        (let ((s (/ dy (- (* s1 (- y ymin)) (* s0 (- y ymax))))))
        (shape (+ x0 (* (- x x0) s))
               y z)))))

(define-public (taper-xy-z shape xy zmin zmax s0 s1)
    "taper-xy-z shape '(x0 y0) ymin ymax s0 s1
    Tapers a shape in the along the Z axis.
    s0 is the scale at zmin; s1 is the scale at zmax
    Scales are applied centered at x0, y0 in the XY plane"
    (let ((x0 (car xy))
          (y0 (cadr xy))
          (dz (- zmax zmin)))
    (lambda (x y z)
        (let ((s (/ dz (- (* s1 (- z zmin)) (* s0 (- z zmax))))))
        (shape (+ x0 (* (- x x0) s))
               (+ y0 (* (- y y0) s))
               z)))))
