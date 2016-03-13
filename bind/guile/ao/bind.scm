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
    (lambda (f) (or (hash-ref cache f)
                    (let ((ptr (dynamic-func f libao)))
                         (hash-set! cache f ptr)
                         ptr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define c_store_new (get-function "store_new"))

(define-public (store-new)
    (wrap-store ((pointer->procedure
    '* (get-function "store_new") '()))))

(define-public (store-delete s)
    ((pointer->procedure
    void (get-function "store_delete") '(*))
        (unwrap-store s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (opcode->int op)
    ((pointer->procedure
        int (get-function "opcode_enum") '(*))
    (string->pointer (symbol->string op))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (token-x s)
    (wrap-token ((pointer->procedure
    '* (get-function "token_x") '(*)) (unwrap-store s))))

(define-public (token-y s)
    (wrap-token ((pointer->procedure
    '* (get-function "token_y") '(*)) (unwrap-store s))))

(define-public (token-z s)
    (wrap-token ((pointer->procedure
    '* (get-function "token_z") '(*)) (unwrap-store s))))

(define-public (token-const s v)
    (wrap-token ((pointer->procedure
    '* (get-function "token_const") (list '* float)) (unwrap-store s) v)))

(define-public (token-op-unary s op a)
    (wrap-token ((pointer->procedure
        '* (get-function "token_unary") (list '* int '*))
    (unwrap-store s) (opcode->int op) (unwrap-token a))))

(define-public (token-op-binary s op a b)
    (wrap-token ((pointer->procedure
        '* (get-function "token_binary") (list '* int '* '*))
    (unwrap-store s) (opcode->int op) (unwrap-token a) (unwrap-token b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tree-new s t)
    (wrap-tree ((pointer->procedure
    '* (get-function "tree_new") '(* *))
        (unwrap-store s) (unwrap-token t))))

(define-public (tree-attach-finalizer t)
    "Attaches tree_delete to a wrapped Tree pointer"
    (wrap-tree (make-pointer
        (pointer-address (unwrap-tree t))
        (get-function "tree_delete"))))

(define-public (tree-eval-double t x y z)
    ((pointer->procedure
    float (get-function "tree_eval_double")
    (list '* float float float))
        (unwrap-tree t) x y z))

(define-public (tree-export-heightmap t filename a b res)
    ((pointer->procedure void (get-function "tree_export_heightmap")
        (list '* '* float float float
                    float float float float))
    (unwrap-tree t) (string->pointer filename)
    (car a) (car b) (cadr a) (cadr b) (caddr a) (caddr b) res))

(define-public (tree-export-mesh t filename a b res)
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
