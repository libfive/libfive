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

(use-modules (ao jit) (ao lib))

(define-public (ao-export-heightmap shape file a b res)
    " Renders a shape and saves it to an image.
a and b are '(x y z) corners; res is a resolution in voxels per unit."
    (tree_export_heightmap (jit shape) file
        (cons (car   a) (car   b))
        (cons (cadr  a) (cadr  b))
        (cons (if (>= (length a) 3) (caddr a) 0)
              (if (>= (length b) 3) (caddr b) 0)) res))

(define-public (ao-export-mesh shape file a b res)
    " Renders a shape and saves it to an mesh
a and b are '(x y z) corners; res is a resolution in voxels per unit."
    (tree_export_mesh (jit shape) file
        (cons (car   a) (car   b))
        (cons (cadr  a) (cadr  b))
        (cons (caddr a)
              (caddr b)) res))
