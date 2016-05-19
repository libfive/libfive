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

(use-modules (ao bind) (ao jit))

(define-public (ao-export-heightmap shape file a b res)
    "ao-export-heightmap shape file '(x0 y0 z0) '(x1 y1 z1) res
    Renders a shape and saves it to an image.
    bounds are three-element lists; res is a resolution in voxels per unit."
    (tree-export-heightmap (jit shape) file a b res))

(define-public (ao-export-mesh shape file a b res)
    "ao-export-mesh shape file '(x0 y0 z0) '(x1 y1 z1) res
    Renders a shape and saves it to an mesh
    bounds are three-element lists; res is a resolution in voxels per unit."
    (tree-export-mesh (jit shape) file a b res))

(define-public (ao-export-slice shape file a b z res)
    "ao-export-slice shape file '(x0 y0) '(x1 y1) z res
    Renders a 2D slice of a model and saves it to an svg file
    bounds are two-element lists
    z is a single number
    res is a resolution in voxels per unit"
    (tree-export-slice (jit shape) file a b z res))
