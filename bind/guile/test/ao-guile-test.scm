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
;; Tweak the load path to find the ggspec and ao module
(add-to-load-path (dirname (current-filename)))
(add-to-load-path (string-append (dirname (current-filename)) "/../"))

(use-modules (ggspec lib))

(use-modules (ao bind) (ao jit) (ao operators) (ao transforms))

(suite "bind.scm (low-level libao interface)"
    (tests
    (test "store-new" env
        (assert-true (store? (store-new))))
    (test "Making variables" env
        (let ((store (store-new)))
            (assert-all
                (assert-true (token? (token-x store)))
                (assert-true (token? (token-y store)))
                (assert-true (token? (token-z store))))))
    (test "token-const" env
        (let ((store (store-new)))
            (assert-true (token? (token-const store 12)))))
    (test "token-op-unary" env
        (let* ((store (store-new))
               (x (token-x store)))
            (assert-true (token? (token-op-unary store 'neg x)))))
    (test "token-op-binary" env
        (let* ((store (store-new))
               (x (token-x store))
               (y (token-y store)))
            (assert-true (token? (token-op-binary store 'add x y)))))
    (test "token-bounded and token-bounds" env
        (let* ((store (store-new))
               (x (token-x store))
               (b (token-bounded store x '(-1 -2 -3) '( 5 6 7))))
            (assert-all
                (assert-true (token? b))
                (assert-equal (token-bounds b)
                              '((-1.0 -2.0 -3.0) (5.0 6.0 7.0))))))
    (test "token-affine-vec" env
        (let* ((store (store-new))
               (x (token-x store))
               (y (token-y store))
               (p (token-op-binary store 'add x y)))
            (assert-all
                (assert-equal (token-affine-vec x) '(1.0 0.0 0.0 0.0))
                (assert-equal (token-affine-vec y) '(0.0 1.0 0.0 0.0))
                (assert-equal (token-affine-vec p) '(1.0 1.0 0.0 0.0)))))
    (test "tree-new" env
        (let* ((store (store-new))
               (x (token-x store))
               (t (tree-new store x)))
            (assert-true (tree? t))))
    (test "tree-eval-double" env
        (let* ((store (store-new))
               (x (token-x store))
               (t (tree-new store x)))
            (assert-equal (tree-eval-double t 1.0 2.0 3.0) 1.0)))
    (test "tree-eval-interval" env
        (let* ((store (store-new))
               (x (token-x store))
               (t (tree-new store x)))
            (assert-equal (tree-eval-interval t '(1 . 2) '(3 . 4) '(5 . 6)) '(1.0 . 2.0))))
    (test "matrix-invert" env
        ; TODO: Make this use a floating-point comparison
        (assert-equal (matrix-invert '(1 0 0 1)
                                     '(0 1 0 2)
                                     '(0 0 1 3))
                                    '((1.0 -0.0 0.0 -1.0)
                                      (-0.0 1.0 -0.0 -2.0)
                                      (0.0 -0.0 1.0 -3.0))))
))

(suite "jit.scm (operations on lambda functions)"
    (tests
    (test "jit-function (float evaluation)" env
        (let ((f (jit-function (lambda (x y z) x))))
        (assert-all
            (assert-equal (f 1 0 0) 1.0)
            (assert-equal (f 2 0 0) 2.0))))
    (test "jit-function (interval evaluation)" env
        (let ((f (jit-function (lambda (x y z) x))))
        (assert-all
            (assert-equal (f '(1 . 2) '(0 . 0) '(0 . 0)) '(1.0 . 2.0))
            (assert-equal (f '(2 . 3) '(0 . 0) '(0 . 0)) '(2.0 . 3.0)))))
    (test "get-affine-vec" env
        (let ((f (lambda (x y z) (+ (* 2 x) y (* 3 z) 4)))
              (g (lambda (x y z) (* x y))))
        (assert-all
            (assert-equal (get-affine-vec f) '(2.0 1.0 3.0 4.0))
            (assert-equal (get-affine-vec g) #f))))
    (test "get-bounds" env
        (let ((f (set-bounds (lambda (x y z) x) '(-1 -2 -3) '(4 5 6)))
              (g (lambda (x y z) x)))
        (assert-all
            (assert-equal (get-bounds f) '((-1.0 -2.0 -3.0) (4.0 5.0 6.0)))
            (assert-equal (get-bounds g) #f))))
))

(suite "transforms.scm (transforms and bounds tracking)"
    (tests
    (test "apply-transform" env
        (let* ((f (lambda (x y z) x))
               (fb (set-bounds f '(-1 -2 -3) '(4 5 6)))
               (t (apply-transform fb (x y z)
                        (* x 2) (+ y 1) (/ z 3)))
               (b (get-bounds t))
          )
        (assert-all
            (assert-true b)
            (assert-equal b '((-0.5 -3.0 -9.0) ( 2.0 4.0 18.0))))))
))
