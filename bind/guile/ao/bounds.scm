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
(define-module (ao bounds))

(define-public (set-default-bounds shape)
    "set-default-bounds shape
    Tags the given shape with [-inf, inf] bounds
    Used when we want a shape to have bounds meta-data but don't know what
    that meta-data should actually be"
    (set-bounds shape '(-inf.0 -inf.0 -inf.0) '(+inf.0 +inf.0 +inf.0)))

(define-public (set-bounds shape lower upper)
    "set-bounds shape '(xmin ymin [zmin]) '(xmax ymax [zmax])
    Sets the bounds of a shape"
    (lambda (. args)
        "(shape x y z) or (shape 'bounds)
    Shape with associated bounds"
        (let ((lower (if (= 3 (length lower)) lower
                         (list (car lower) (cadr lower) -inf.0)))
              (upper (if (= 3 (length upper)) upper
                         (list (car upper) (cadr upper) +inf.0))))
        (if (and (= 1 (length args))
                 (eq? (car args) 'bounds))
            (list lower upper)
            (apply shape args)))))

(define-public (get-bounds f)
    "get-bounds f
    If f is wrapped with set-bounds, returns the bounds in the form
    '((xmin ymin zmin) (xmax ymax zmax)); otherwise, returns #f"
    (catch #t
        (lambda () (f 'bounds))
        (lambda (key . params) #f)))

(define-public (bounds-union bs)
    "bounds-union bs
    bs should be a list of '((xmin ymin zmin) (xmax ymax zmax)) lists
    Finds the union along each dimension and returns a list in the same form"
    (let* ((lower (map car bs))
           (upper (map cadr bs))
           (xmin (apply min (map car lower)))
           (ymin (apply min (map cadr lower)))
           (zmin (apply min (map caddr lower)))
           (xmax (apply max (map car upper)))
           (ymax (apply max (map cadr upper)))
           (zmax (apply max (map caddr upper))))
    (list (list xmin ymin zmin) (list xmax ymax zmax))))

(define-public (bounds-intersection bs)
    "bounds-intersection bs
    bs should be a list of '((xmin ymin zmin) (xmax ymax zmax)) lists
    Finds the intersection along each dimension
    If bounds are disjoint, returns empty interval (0, 0)"
    (let* ((lower (map car bs))
           (upper (map cadr bs))
           (xmin (apply max (map car lower)))
           (ymin (apply max (map cadr lower)))
           (zmin (apply max (map caddr lower)))
           (xmax (apply min (map car upper)))
           (ymax (apply min (map cadr upper)))
           (zmax (apply min (map caddr upper))))
    ;; Clamp intervals to empty (0,0) if bounds are disjoint
    (if (< xmax xmin)   (begin (set! xmin 0) (set! xmax 0)))
    (if (< ymax ymin)   (begin (set! ymin 0) (set! ymax 0)))
    (if (< zmax zmin)   (begin (set! zmin 0) (set! zmax 0)))
    (list (list xmin ymin zmin) (list xmax ymax zmax))))

