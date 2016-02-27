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

(define-public (store-new)
    (wrap-store ((pointer->procedure
    '* (dynamic-func "store_new" libao) '()))))

(define-public (store-delete s)
    ((pointer->procedure
    void (dynamic-func "store_delete" libao) '(*))
        (unwrap-store s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (opcode->int op)
    ((pointer->procedure
        int (dynamic-func "opcode_enum" libao) '(*))
    (string->pointer (symbol->string op))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (token-x s)
    (wrap-token ((pointer->procedure
    '* (dynamic-func "token_x" libao) '(*)) (unwrap-store s))))

(define-public (token-y s)
    (wrap-token ((pointer->procedure
    '* (dynamic-func "token_y" libao) '(*)) (unwrap-store s))))

(define-public (token-z s)
    (wrap-token ((pointer->procedure
    '* (dynamic-func "token_z" libao) '(*)) (unwrap-store s))))

(define-public (token-const s v)
    (wrap-token ((pointer->procedure
    '* (dynamic-func "token_const" libao) (list '* float)) (unwrap-store s) v)))

(define-public (token-op-unary s op a)
    (wrap-token ((pointer->procedure
        '* (dynamic-func "token_unary" libao) (list '* int '*))
    (unwrap-store s) (opcode->int op) (unwrap-token a))))

(define-public (token-op-binary s op a b)
    (wrap-token ((pointer->procedure
        '* (dynamic-func "token_binary" libao) (list '* int '* '*))
    (unwrap-store s) (opcode->int op) (unwrap-token a) (unwrap-token b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tree-new s t)
    (wrap-tree ((pointer->procedure
    '* (dynamic-func "tree_new" libao) '(* *))
        (unwrap-store s) (unwrap-token t))))

(define-public (tree-delete t)
    ((pointer->procedure
    void (dynamic-func "tree_delete" libao) '(*)) (unwrap-tree t)))

(define-public (tree-eval-double t x y z)
    ((pointer->procedure
    float (dynamic-func "tree_eval_double" libao)
    (list '* float float float))
        (unwrap-tree t) x y z))

(define-public (tree-export-heightmap t filename a b res)
    ((pointer->procedure void (dynamic-func "tree_export_heightmap" libao)
        (list '* '* float float float
                    float float float float))
    (unwrap-tree t) (string->pointer filename)
    (car a) (car b) (cadr a) (cadr b) (caddr a) (caddr b) res))

(define-public (tree-export-mesh t filename a b res)
    ((pointer->procedure void (dynamic-func "tree_export_mesh" libao)
        (list '* '* float float float
                    float float float float))
    (unwrap-tree t) (string->pointer filename)
    (car a) (car b) (cadr a) (cadr b) (caddr a) (caddr b) res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (window-show-tree filename name t)
    ((pointer->procedure void (dynamic-func "window_show_tree" libao) '(* * *))
    (string->pointer filename) (string->pointer name) (unwrap-tree t)))

(define-public (window-watch-file dir file)
    ((pointer->procedure void (dynamic-func "window_watch_file" libao) '(* *))
    (string->pointer dir) (string->pointer file)))

(define-public window-clear-frames
    (pointer->procedure void (dynamic-func "window_clear_frames" libao) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public ao-run
    (pointer->procedure void (dynamic-func "ao_run" libao) '()))

(define-public ao-halt
    (pointer->procedure void (dynamic-func "ao_halt" libao) '()))

(define-public (ao-init-guile)
    "Initialize libao by setting a callback pointer for file watchers"
    ((pointer->procedure void (dynamic-func "window_set_callback" libao) '(*))
     (procedure->pointer void
        (lambda (f) (primitive-load (pointer->string f)))
     '(*))))
