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

;; This is the core FFI library, which wraps libfive.h
;; For now, it's manually generated, since there aren't that many functions
(define-module (libfive lib))

(use-modules (system foreign))

;; TODO: find this library with more smarts
(define lib (dynamic-link "../../../build/libfive/src/libfive"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public libfive-tree-del-ptr
    (dynamic-func "libfive_tree_delete" lib))

(define-public libfive-tree-const
  (pointer->procedure
    '* (dynamic-func "libfive_tree_const" lib) (list float)))

(define-public libfive-opcode-enum
  (pointer->procedure
    int (dynamic-func "libfive_opcode_enum" lib) (list '*)))

(define-public libfive-tree-is-var
  (pointer->procedure
    uint8 (dynamic-func "libfive_tree_is_var" lib) (list '*)))

(define-public libfive-tree-var
  (pointer->procedure
    '* (dynamic-func "libfive_tree_var" lib) '()))

(define-public libfive-opcode-args
  (pointer->procedure
    int (dynamic-func "libfive_opcode_args" lib) (list int)))

(define-public libfive-tree-nullary
  (pointer->procedure
    '* (dynamic-func "libfive_tree_nullary" lib) (list int)))

(define-public libfive-tree-unary
  (pointer->procedure
    '* (dynamic-func "libfive_tree_unary" lib) (list int '*)))

(define-public libfive-tree-binary
  (pointer->procedure
    '* (dynamic-func "libfive_tree_binary" lib) (list int '* '*)))

(define-public libfive-tree-constant-vars
  (pointer->procedure
    '* (dynamic-func "libfive_tree_constant_vars" lib) (list '*)))

(define-public libfive-tree-id
  (pointer->procedure
    '* (dynamic-func "libfive_tree_id" lib) (list '*)))

(define-public libfive-tree-remap
  (pointer->procedure
    '* (dynamic-func "libfive_tree_remap" lib) (list '* '* '* '*)))

(define-public libfive-tree-print
  (pointer->procedure
    '* (dynamic-func "libfive_tree_print" lib) (list '*)))

(define-public libfive-free
  (pointer->procedure
    void (dynamic-func "libfive_free" lib) (list '*)))
