#|
Guile bindings to the libfive CAD kernel
Copyright (C) 2021  Matt Keeter

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
(use-modules (system foreign))

;; TODO: find this library with more smarts
(define lib (dynamic-link "libfive"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define libfive-tree-del
  (pointer->procedure
    void (dynamic-func "libfive_tree_delete" lib) '(*)))

(define-public (wrap-tree t)
  " Wraps a foreign pointer t with a finalizer that deletes it"
  )

(define libfive-tree-const
  (pointer->procedure
    '* (dynamic-func "libfive_tree_const" lib) '(float)))

(define libfive-opcode-enum
  (pointer->procedure
    int (dynamic-func "libfive_opcode_enum" lib) '(*)))

(define libfive-tree-is-var
  (pointer->procedure
    uint8 (dynamic-func "libfive_tree_is_var" lib) '(*)))

(define libfive-tree-var
  (pointer->procedure
    '* (dynamic-func "libfive_tree_var" lib) '()))

