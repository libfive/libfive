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

(define-public store-new
    (pointer->procedure
    '* (dynamic-func "store_new" libao) '()))

(define-public store-delete
    (pointer->procedure
    void (dynamic-func "store_delete" libao) '(*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (opcode->int op)
    ((pointer->procedure
        int (dynamic-func "opcode_enum" libao) '(*))
    (string->pointer (symbol->string op))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public token-x
    (pointer->procedure
    '* (dynamic-func "token_x" libao) '(*)))

(define-public token-y
    (pointer->procedure
    '* (dynamic-func "token_y" libao) '(*)))

(define-public token-z
    (pointer->procedure
    '* (dynamic-func "token_z" libao) '(*)))

(define-public token-const
    (pointer->procedure
    '* (dynamic-func "token_const" libao) '(* float)))

(define-public (token-op store op a . bs)
    (let ((i (opcode->int op))
          (len (length bs)))
    (cond ((= 0 len) ((pointer->procedure
        '* (dynamic-func "token_unary" libao) '(* int *))
            store i a))
          ((= 1 len) ((pointer->procedure
        '* (dynamic-func "token_binary" libao) '(* int * *))
            store i a (car bs)))
          (else (error "Incorrect arguments to token-op")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public tree-new
    (pointer->procedure
    '* (dynamic-func "store_new" libao) '(* *)))

(define-public tree-delete
    (pointer->procedure
    void (dynamic-func "tree_delete" libao) '(*)))

(define-public tree-eval-double
    (pointer->procedure
    float (dynamic-func "tree_eval_double" libao) '(* float float float)))

(define-public (tree-export-heightmap tree filename a b res)
    ((pointer->procedure void (dynamic-func "tree_export_heightmap" libao)
        '(* * float float float
              float float float float))
    tree (string->pointer filename)
    (car a) (car b) (cadr a) (cadr b) (caddr a) (caddr b) res))

(define-public (tree-export-mesh tree filename a b res)
    ((pointer->procedure void (dynamic-func "tree_export_mesh" libao)
        '(* * float float float
              float float float float))
    tree (string->pointer filename)
    (car a) (car b) (cadr a) (cadr b) (caddr a) (caddr b) res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (window-show-tree filename name tree)
    ((pointer->procedure void (dynamic-func "window_show_tree" libao) '(* * *))
    (string->pointer filename) (string->pointer name) tree))

(define-public (window-watch-file dir file)
    ((pointer->procedure void (dynamic-func "window_watch_file" libao) '(* *))
    (string->pointer dir) (string->pointer file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public ao-run
    (pointer->procedure void (dynamic-func "ao_run" libao) '()))

(define-public ao-halt
    (pointer->procedure void (dynamic-func "ao_halt" libao) '()))

(define-public ao-init-guile
    (pointer->procedure void (dynamic-func "ao_init_guile" libao) '()))
