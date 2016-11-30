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
(define-module (ao export))

(use-modules (ao bind) (ao jit) (ao bounds) (ao util))

(define (export-bounded func shape file res args)
    "export-bounded func shape file res args
    Checks for bounds on the shape; otherwise uses args"
    (let ((bounds (get-bounds shape))
          (shape (jit shape)))
      (if bounds
        (func shape file (car bounds) (cadr bounds) res)
        (if (not (= 2 (length args)))
          (ao-error 'export-bounds
                    "Shape has no bounds; must provide them for export")
          (func shape file (car args) (cadr args) res)))))


(define-public (ao-export-heightmap shape file res . args)
    "ao-export-heightmap shape file res ['(x0 y0 z0) '(x1 y1 z1)]
    Renders a shape and saves it to an image.
    bounds are three-element lists; res is a resolution in voxels per unit."
    (export-bounded tree-export-heightmap shape file res args))

(define-public (ao-export-mesh shape file res . args)
    "ao-export-mesh shape file res ['(x0 y0 z0) '(x1 y1 z1)]
    Renders a shape and saves it to an mesh
    bounds are three-element lists; res is a resolution in voxels per unit."
    (export-bounded tree-export-mesh shape file res args))

(define-public (ao-export-slice shape file res z . args)
    "ao-export-slice shape file res z ['(x0 y0) '(x1 y1)]
    Renders a 2D slice of a model and saves it to an svg file
      res is a resolution in voxels per unit
      z is a single number
      bounds are two-element lists"
    (export-bounded (lambda (shape file a b res)
                      (tree-export-slice shape file a b z res))
      shape file res args))
