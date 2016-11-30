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
(define-module (ao bind))

(use-modules (srfi srfi-1))
(use-modules (srfi srfi-13))
(use-modules (ice-9 i18n))
(use-modules (system foreign))

(use-modules (ao util))

(define libao (dynamic-link (string-append (dirname (current-filename))
                                           "/../../../lib/libao")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-wrapped-pointer-type
    tree tree? wrap-tree_ unwrap-tree
    (lambda (o p)
        (format p "#<tree 0x~x>"
        (pointer-address (unwrap-tree o)))))

(define (wrap-tree ptr)
    "wrap-tree ptr
    Attaches tree_delete to a bare pointer"
    (wrap-tree_ (make-pointer
        (pointer-address ptr)
        (get-function "tree_delete"))))
(export tree?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cached function lookup in libao
(define get-function
    (let ((cache (make-hash-table)))
    (lambda (f)
        "Look up a function in libao, with caching"
        (or (hash-ref cache f)
            (let ((ptr (dynamic-func f libao)))
                 (hash-set! cache f ptr)
                 ptr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Structures used in sending / receiving data
(define v2 (list float float))
(define v3 (list float float float))
(define v4 (list float float float float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (opcode->int op)
    "opcode->int op
    Converts an opcode (which should be a symbol, e.g. 'add)
    into an integer representing its enum value"
    ((pointer->procedure
        int (get-function "opcode_enum") '(*))
    (string->pointer (symbol->string op))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tree-x)
    "tree-x
    Constructs a X tree"
    (wrap-tree ((pointer->procedure '* (get-function "tree_x") '()))))

(define-public (tree-y)
    "tree-y
    Constructs a Y tree"
    (wrap-tree ((pointer->procedure '* (get-function "tree_y") '()))))

(define-public (tree-z)
    "tree-z
    Constructs a Z tree"
    (wrap-tree ((pointer->procedure '* (get-function "tree_z") '()))))

(define-public (tree-const v)
    "tree-const value
    Constructs a constant tree"
    (wrap-tree ((pointer->procedure
    '* (get-function "tree_const") (list float)) v)))

(define-public (tree-op-unary op a)
    "tree-op-unary opcode a
    Constructs a unary operation"
    (wrap-tree ((pointer->procedure
        '* (get-function "tree_unary") (list int '*))
      (opcode->int op) (unwrap-tree a))))

(define-public (tree-op-binary op a b)
    "tree-op-binary opcode a b
    Constructs a binary operation"
    (wrap-tree ((pointer->procedure
        '* (get-function "tree_binary") (list int '* '*))
      (opcode->int op) (unwrap-tree a) (unwrap-tree b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tree-affine-vec t)
    "tree-affine-vec tree
    Extracts the affine terms from a tree, returning a list '(a b c d)
    or #f if the tree is not affine"
    (let* ((v (make-c-struct v4 '(0 0 0 0)))
           (tree_affine_vec (pointer->procedure int
                            (get-function "tree_affine_vec") (list '* '*)))
           (result (tree_affine_vec (unwrap-tree t) v)))
    (if (= result 1) (parse-c-struct v v4) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tree-eval-double t x y z)
    "tree-eval-double tree x y z
    Evaluates a Tree at the given position
    x y z should be numbers"
    ((pointer->procedure
    float (get-function "tree_eval_double")
    (list '* float float float))
        (unwrap-tree t) x y z))

(define-public (tree-eval-interval t X Y Z)
    "tree-eval-interval tree X Y Z
    Evaluates a Tree over the given region
    X, Y, Z should be pairs '(lower . upper)
    Returns a pair '(lower . upper)"
    (if (any (lambda (I) (< (cdr I) (car I))) (list X Y Z))
        (ao-error 'interval-order "Intervals must be ordered"))
    (let ((vx (make-c-struct v2 (list (car X) (cdr X))))
          (vy (make-c-struct v2 (list (car Y) (cdr Y))))
          (vz (make-c-struct v2 (list (car Z) (cdr Z)))))
    ((pointer->procedure void (get-function "tree_eval_interval")
        (list '* '* '* '*)) (unwrap-tree t) vx vy vz)
    (let ((out (parse-c-struct vx v2)))
        (cons (car out) (cadr out)))))

(define (check-extension filename exts)
    "check-extension filename exts
    Checks that the given filename ends with one of the members of list exts
    If there is a match, returns filename with ext lowercased
    Otherwise, emits an error"
    (let* ((len (string-length filename))
           (lower-filename
            (any (lambda (ext)
                   (if (string-suffix-ci? ext filename)
                       (string-append
                         (string-drop-right filename (string-length ext)) ext)
                       #f))
                 exts)))
      (or lower-filename
          (ao-error 'export-file-extension
           (format #f
                   "Invalid file extension. Expected one of ~a, got ~a"
                   exts filename)))))

(define-public (tree-export-heightmap t filename a b res)
    "tree-export-heightmap tree filename lower upper res
    Exports a tree as a heightmap
    lower and upper should be '(x y z) lists
    res is resolution in voxels per unit"
    (set! filename (check-extension filename '(".png")))
    ((pointer->procedure void (get-function "tree_export_heightmap")
        (list '* '* float float float
                    float float float float))
    (unwrap-tree t) (string->pointer filename)
    (car a) (car b) (cadr a) (cadr b) (caddr a) (caddr b) res))

(define-public (tree-export-mesh t filename a b res)
    "tree-export-mesh tree filename lower upper res
    Exports a tree as a mesh
    lower and upper should be '(x y z) lists
    res is resolution in voxels per unit"
    (set! filename (check-extension filename '(".stl" ".obj")))
    ((pointer->procedure void (get-function "tree_export_mesh")
        (list '* '* float float float
                    float float float float))
    (unwrap-tree t) (string->pointer filename)
    (car a) (car b) (cadr a) (cadr b) (caddr a) (caddr b) res))

(define-public (tree-export-slice t filename a b z res)
    "tree-export-slice tree filename lower upper z res
    Exports a tree as an svg slice
    lower and upper should be '(x y) lists
    z is the z height of the slice
    res is resolution in voxels per unit"
    (set! filename (check-extension filename '(".svg")))
    ((pointer->procedure void (get-function "tree_export_slice")
        (list '* '* float float float
                    float float float))
    (unwrap-tree t) (string->pointer filename)
    (car a) (car b) (cadr a) (cadr b) z res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (window-show-tree filename name t)
    ((pointer->procedure void (get-function "window_show_tree") '(* * *))
    (string->pointer filename) (string->pointer name) (unwrap-tree t)))

(define-public (window-watch-file dir file)
    ((pointer->procedure void (get-function "window_watch_file") '(* *))
    (string->pointer dir) (string->pointer file)))

(define-public window-clear-frames
    (pointer->procedure void (get-function "window_clear_frames") '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (matrix-invert x y z)
    "Inverts a matrix with rows x, y, z, '(0 0 0 1)
    x, y, and z should be four-element lists
    Returns a list of the new x, y, z rows (from the inverted matrix)"
    (let* ((x (make-c-struct v4 x))
           (y (make-c-struct v4 y))
           (z (make-c-struct v4 z))
           (matrix_invert (pointer->procedure '*
                          (get-function "matrix_invert") (list '* '* '*))))
    (matrix_invert x y z)
    (list (parse-c-struct x v4)
          (parse-c-struct y v4)
          (parse-c-struct z v4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public ao-run
    (pointer->procedure void (get-function "ao_run") '()))

(define-public ao-halt
    (pointer->procedure void (get-function "ao_halt") '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The callback will be called from a non-Guile thread, so we need to install
;; scm_init_guile as the watcher thread initialization function.
(define libguile (dynamic-link "libguile-2.0"))

;; This is the callback function that activates when a file changes
(define (callback f)
    (catch #t
        (lambda () (primitive-load (pointer->string f)))
        (lambda (key . params)
            (format #t "Error in evaluating ~s:\n" (pointer->string f))
            (apply format (append (list #t (cadr params)) (cddr params)))
            (newline))))

(define-public (ao-init-guile)
    "Initialize libao by setting a init and callback pointers for file watchers"
    ((pointer->procedure void (get-function "window_set_thread_init") '(*))
     (dynamic-func "scm_init_guile" libguile))
    ((pointer->procedure void (get-function "window_set_callback") '(*))
     (procedure->pointer void callback '(*))))
