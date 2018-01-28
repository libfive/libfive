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
(use-modules (libfive kernel) (libfive vec))

(define-public (union . args)
  "union a [b [c [...]]]
  Returns the union of any number of shapes"
  (apply min args))

(define-public (intersection . args)
  "intersection a [b [c [...]]]
  Returns the intersection of any number of shapes"
  (apply max args))

(define-public (inverse a)
  "inverse a
  Returns a shape that's the inverse of the input shape"
  (- a))

(define-public (difference a . bs)
  "difference a b [c [d [...]]]
  Subtracts any number of shapes from the first argument"
  (intersection a (inverse (apply union bs))))

(define-public (offset s o)
  "offset shape o
  Expand or contract a given shape by an offset"
  (+ s o))

(define-public (clearance a b o)
  "clearance a b o
  Expands shape b by the given offset then subtracts it from shape a"
  (difference a (offset b o)))

(define-public (shell shape o)
  "shell shape o
  Returns a shell of a shape with the given offset"
  (clearance shape shape o))

(define-public (blend a b m)
  "blend a b m
  Blends two shapes by the given amount"
  (union a b (- (+ (sqrt (abs a)) (sqrt (abs b))) m)))

(define-public (blend-difference a b m)
  "blend-difference a b m
  Blends the subtraction of b from a"
  (- (blend (- a) b m)))

(define-public (morph a b m)
  "morph a b m
  Morphs between two shapes.
  m = 0 produces a, m = 1 produces b"
  (+ (* a (- 1 m)) (* b m)))

(define-public (loft a b zmin zmax)
  "loft a b zmin zmax
  Produces a blended loft between a (at zmin) and b (at zmax)
  a and b should be 2D shapes (i.e. invariant along the z axis)"
  (lambda-shape (x y z)
    (max (- z zmax) (- zmin z)
      (/ (+ (* (- z zmin) b) (* (- zmax z) a))
         (- zmax zmin)))))

(define-public (loft-between a b lower upper)
  "loft a b #[xa ya zmin] #[xb yb zmax]
  Produces a blended loft between a (at zmin) and b (at zmax), with XY
  coordinates remapped to slide between the two centers #[xa ya] and #[xb yb].
  a and b should be 2D shapes (i.e. invariant along the z axis)"
  (define (f z) (/ (- z (.z lower)) (- (.z upper) (.z lower))))
  (define (g z) (/ (- (.z upper) z) (- (.z upper) (.z lower))))
  (let ((a (remap-shape (a x y z)
          (+ x (* (f z) (- (.x lower) (.x upper))))
          (+ y (* (f z) (- (.y lower) (.y upper))))
          z))
        (b (remap-shape (b x y z)
          (+ x (* (g z) (- (.x upper) (.x lower))))
          (+ y (* (g z) (- (.y upper) (.y lower))))
          z)))
    (loft a b (.z lower) (.z upper))))
