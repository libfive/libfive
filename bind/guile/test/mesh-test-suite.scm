#!/usr/bin/env guile
!#

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
(add-to-load-path (string-append (dirname (current-filename)) "/../"))

(use-modules (ao export) (ao shapes) (ao csg) (ao transforms) (ao operators))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(catch #t
  (lambda () (mkdir "meshes"))
  (lambda (key args . rest) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (diamond x y z)
  (max (+ x y -1)
       (- x y 1)
       (- y x 1)
       (- -1 x y)))
(ao-export-mesh (extrude-z diamond -1 1)
                "meshes/diamond.stl" 4 '(-2.1 -2.1 -2.1) '(2.1 2.1 2.1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; How do edges and vertex placement look?
(define (slice x y z) (+ x y z -2.73))
(define split (intersection (cube '(-1.5 -1.5 -1.5) '(1.5 1.5 1.5)) slice))
(ao-export-mesh split "meshes/split.stl" 2 '(-2.1 -2.1 -2.1) '(2.1 2.1 2.1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Triangle union with epsilon gap
;; Triggers bad meshing without jitter due to octree
;; corners hitting model singularities
(define epsilon 0.0001)
(define (triangle-union X Y Z)
  (max ;; Bound on Z axis
  (min ;; Union two triangles into a diamond
    (- (max (- Y X 1) (* 2 X) (- -1 X Y)) epsilon)
    (- (max (- X Y 1) (+ X Y -1) (* -2 X)) epsilon))
  (- -0.125 Z) (- Z 0.125)))
(ao-export-mesh triangle-union "meshes/triangle-union.stl" 50 '(-5 -5 -5) '(5 5 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Charm model to get export speed
(define charm
  (let* ((ro 1)   ; Outer radius
         (ri 0.7) ; Inner radius
         (t 0.2)  ; Extrusion thickness
         (pi 3.14159)

         ; Initial 2D model
         (s (difference (circle '(0 0) ro)
                        (circle '(0 0) ri)))

         ; Extruded into 3D
         (e (extrude-z s (- t) t)))

         ; And rotated a few times
         (union e (rotate-x e (/ pi 2))
                  (rotate-y e (/ pi 2)))))

(define start-time (get-internal-real-time))
(ao-export-mesh charm "meshes/charm.stl" 100 '(-2 -2 -2) '(2 2 2))
(format #t "Exported charm in ~a sec\n"
        (/ (- (get-internal-real-time) start-time 0.0)
           internal-time-units-per-second))
