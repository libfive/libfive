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

(use-modules (system foreign) (srfi srfi-1))

(define (try-link lib)
  (with-exception-handler
    (lambda (exn) #f)
    (lambda () (dynamic-link lib))
    #:unwind? #t))

;; Right now, it assumes that we're looking from the build directory
(define lib-paths (list "libfive/src/libfive"))
(let ((f (current-filename)))
  (if f (append! lib-paths
    (list
      (string-append (dirname f) "/../../../../build/libfive/src/libfive")))))
(define lib (any (lambda (i) i) (map try-link lib-paths)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define libfive-interval-type (list float float))
(define-public (libfive-interval lower upper)
  " Constructs a libfive_interval "
  (make-c-struct libfive-interval-type (list lower upper)))

(define libfive-region-type
  (list libfive-interval-type libfive-interval-type libfive-interval-type))
(define-public (libfive-region X Y Z)
  " Constructs a libfive_region "
  (make-c-struct libfive-region-type (list X Y Z)))

(define libfive-vec2-type (list float float))
(define libfive-vec3-type (list float float float))

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

(define-public libfive-tree-id
  (pointer->procedure
    '* (dynamic-func "libfive_tree_id" lib) (list '*)))

(define-public libfive-tree-remap
  (pointer->procedure
    '* (dynamic-func "libfive_tree_remap" lib) (list '* '* '* '*)))

(define-public libfive-tree-print
  (pointer->procedure
    '* (dynamic-func "libfive_tree_print" lib) (list '*)))

(define-public libfive-free-str
  (pointer->procedure
    void (dynamic-func "libfive_free_str" lib) (list '*)))

(define-public libfive-tree-save-mesh
  (pointer->procedure
    uint8 (dynamic-func "libfive_tree_save_mesh" lib)
      (list '* libfive-region-type float '*)))

(define-public libfive-tree-save-meshes
  (pointer->procedure
    uint8 (dynamic-func "libfive_tree_save_meshes" lib)
      (list '* libfive-region-type float float '*)))

(define-public libfive-tree-save
  (pointer->procedure
    '* (dynamic-func "libfive_tree_save" lib) (list '* '*)))

(define-public libfive-tree-load
  (pointer->procedure
    uint8 (dynamic-func "libfive_tree_load" lib) (list '*)))

(define-public libfive-tree-eval-f
  (pointer->procedure
    float (dynamic-func "libfive_tree_eval_f" lib)
      (list '* libfive-vec3-type)))

(define-public libfive-tree-eval-i
  (pointer->procedure
    libfive-interval-type (dynamic-func "libfive_tree_eval_r" lib)
      (list '* libfive-region-type)))

(define-public libfive-tree-eval-d
  (pointer->procedure
    libfive-vec3-type (dynamic-func "libfive_tree_eval_d" lib)
      (list '* libfive-vec3-type)))
