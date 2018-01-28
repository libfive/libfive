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

(define-public (clamp a lower upper)
  "clamp a lower upper
  Clamps the given value into a particular range"
  (max lower (min upper a)))

(define-public (mix a b h)
  "mix a b h
  Mixes the two values based on h (which should be in the 0-1 range)
  When h = 0, returns a; when h = 1, returns b"
  (+ (* b h) (* a (- 1 h))))
