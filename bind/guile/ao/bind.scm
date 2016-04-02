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

(use-modules (system foreign))

(define libao (dynamic-link (string-append (dirname (current-filename))
                                           "/../../../lib/libao")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-wrapped-pointer-type
    store store? wrap-store unwrap-store
    (lambda (o p)
        (format p "#<store 0x~x>"
        (pointer-address (unwrap-store o)))))
(export store store? wrap-store unwrap-store)

(define-wrapped-pointer-type
    token token? wrap-token unwrap-token
    (lambda (o p)
        (format p "#<token 0x~x>"
        (pointer-address (unwrap-token o)))))
(export token token? wrap-token unwrap-token)

(define-wrapped-pointer-type
    tree tree? wrap-tree unwrap-tree
    (lambda (o p)
        (format p "#<tree 0x~x>"
        (pointer-address (unwrap-tree o)))))
(export tree tree? wrap-tree unwrap-tree)

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

(define (store-new-raw)
    (wrap-store ((pointer->procedure
    '* (get-function "store_new") '()))))

(define-public (store-new)
    (store-attach-finalizer (store-new-raw)))

(define-public (store-delete s)
    ((pointer->procedure
    void (get-function "store_delete") '(*))
        (unwrap-store s)))

(define-public (store-attach-finalizer s)
    "Attaches store_delete to a wrapped Store pointer"
    (wrap-store (make-pointer
        (pointer-address (unwrap-store s))
        (get-function "store_delete"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (opcode->int op)
    "opcode->int op
    Converts an opcode (which should be a symbol, e.g. 'add)
    into an integer representing its enum value"
    ((pointer->procedure
        int (get-function "opcode_enum") '(*))
    (string->pointer (symbol->string op))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (token-x s)
    "token-x store
    Constructs a X token in the given Store"
    (wrap-token ((pointer->procedure
    '* (get-function "token_x") '(*)) (unwrap-store s))))

(define-public (token-y s)
    "token-y store
    Constructs a Y token in the given Store"
    (wrap-token ((pointer->procedure
    '* (get-function "token_y") '(*)) (unwrap-store s))))

(define-public (token-z s)
    "token-z store
    Constructs a Z token in the given Store"
    (wrap-token ((pointer->procedure
    '* (get-function "token_z") '(*)) (unwrap-store s))))

(define-public (token-const s v)
    "token-const store value
    Constructs a constant token in the given Store"
    (wrap-token ((pointer->procedure
    '* (get-function "token_const") (list '* float)) (unwrap-store s) v)))

(define-public (token-op-unary s op a)
    "token-op-unary store opcode a
    Constructs a unary token in the given Store"
    (wrap-token ((pointer->procedure
        '* (get-function "token_unary") (list '* int '*))
    (unwrap-store s) (opcode->int op) (unwrap-token a))))

(define-public (token-op-binary s op a b)
    "token-op-binary store opcode a b
    Constructs a binary token in the given Store"
    (wrap-token ((pointer->procedure
        '* (get-function "token_binary") (list '* int '* '*))
    (unwrap-store s) (opcode->int op) (unwrap-token a) (unwrap-token b))))

(define-public (token-bounded s t a b)
    "token-bounded store shape lower upper
    Constructs a META_BOUNDS token for the given shape.
    lower and upper should be '(x y z) lists"
    (let* ((struct (list float float float))
           (a (make-c-struct struct a))
           (b (make-c-struct struct b))
           (token_bounded (pointer->procedure '*
                          (get-function "token_bounded") (list '* '* '* '*)))
           (result (token_bounded (unwrap-store s) (unwrap-token t) a b)))
    (wrap-token result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (token-affine-vec t)
    "Extracts the affine terms from a Token, returning a list '(a b c d)
    or #f if the token is not affine"
    (let* ((struct (list float float float float))
           (v (make-c-struct struct '(0 0 0 0)))
           (token_affine_vec (pointer->procedure int
                             (get-function "token_affine_vec") (list '* '*)))
           (result (token_affine_vec (unwrap-token t) v)))
    (if (= result 1) (parse-c-struct v struct) #f)))

(define-public (token-bounds t)
    "Extracts the bounds from a token, return a list
    '((xmin ymin zmin) (xmax ymax zmax))
    (or #f if the token is not a META_BOUNDS token"
    (let* ((struct (list float float float))
           (lower (make-c-struct struct '(0 0 0)))
           (upper (make-c-struct struct '(0 0 0)))
           (token_bounds (pointer->procedure int
                         (get-function "token_bounds") (list '* '* '*)))
           (result (token_bounds (unwrap-token t) lower upper)))
    (if (= result 1) (list (parse-c-struct lower struct)
                           (parse-c-struct upper struct)) #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tree-new s t)
    "Construct a new Tree pointer"
    (wrap-tree ((pointer->procedure
    '* (get-function "tree_new") '(* *))
        (unwrap-store s) (unwrap-token t))))

(define-public (tree-attach-finalizer t)
    "Attaches tree_delete to a wrapped Tree pointer"
    (wrap-tree (make-pointer
        (pointer-address (unwrap-tree t))
        (get-function "tree_delete"))))

(define-public (tree-eval-double t x y z)
    "Evaluates a Tree at the given position
    x y z should be numbers"
    ((pointer->procedure
    float (get-function "tree_eval_double")
    (list '* float float float))
        (unwrap-tree t) x y z))

(define-public (tree-export-heightmap t filename a b res)
    "tree-export-heightmap tree filename lower upper res
    Exports a tree as a heightmap
    lower and upper should be '(x y z) lists
    res is resolution in voxels per unit"
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
    ((pointer->procedure void (get-function "tree_export_mesh")
        (list '* '* float float float
                    float float float float))
    (unwrap-tree t) (string->pointer filename)
    (car a) (car b) (cadr a) (cadr b) (caddr a) (caddr b) res))

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
            (newline) (display key) (display " in ") (display (car params))
            (newline) (display (cadr params)) (newline))))

(define-public (ao-init-guile)
    "Initialize libao by setting a init and callback pointers for file watchers"
    ((pointer->procedure void (get-function "window_set_thread_init") '(*))
     (dynamic-func "scm_init_guile" libguile))
    ((pointer->procedure void (get-function "window_set_callback") '(*))
     (procedure->pointer void callback '(*))))
