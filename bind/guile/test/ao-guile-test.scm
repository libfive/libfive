#!/usr/bin/env guile
!#

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

(use-modules (ao bind) (ao jit) (ao operators) (ao transforms) (ao csg) (ao bounds))

(suite "bind.scm (low-level libao interface)"
    (tests
    (test "Making variables" env
        (assert-all
            (assert-true (tree? (tree-x)))
            (assert-true (tree? (tree-y)))
            (assert-true (tree? (tree-z)))))
    (test "tree-const" env
        (assert-true (tree? (tree-const 12))))
    (test "tree-op-unary" env
        (let* ((x (tree-x)))
            (assert-true (tree? (tree-op-unary 'neg x)))))
    (test "tree-op-binary" env
        (let* ((x (tree-x))
               (y (tree-y)))
            (assert-true (tree? (tree-op-binary 'add x y)))))
    (test "tree-affine-vec" env
        (let* ((x (tree-x))
               (y (tree-y))
               (p (tree-op-binary 'add x y)))
            (assert-all
                (assert-equal (tree-affine-vec x) '(1.0 0.0 0.0 0.0))
                (assert-equal (tree-affine-vec y) '(0.0 1.0 0.0 0.0))
                (assert-equal (tree-affine-vec p) '(1.0 1.0 0.0 0.0)))))
    (test "tree-eval-double" env
          (assert-near (tree-eval-double (tree-x) 1 2 3) 1))
    (test "tree-eval-interval" env
          (assert-equal (tree-eval-interval (tree-x)
            '(1 . 2) '(3 . 4) '(5 . 6)) '(1.0 . 2.0)))
    (test "matrix-invert" env
        ; TODO: Make this use a floating-point comparison
        (assert-equal (matrix-invert '(1 0 0 1)
                                     '(0 1 0 2)
                                     '(0 0 1 3))
                                    '(( 1.0 -0.0  0.0 -1.0)
                                      (-0.0  1.0 -0.0 -2.0)
                                      ( 0.0 -0.0  1.0 -3.0))))
))

(suite "jit.scm (operations on lambda functions)"
    (tests
    (test "jit-function (float evaluation)" env
        (let ((f (jit-function (lambda (x y z) x))))
        (assert-all
            (assert-near (f 1 0 0) 1)
            (assert-near (f 2 0 0) 2))))
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
    (test "expt" env
        (let ((f (jit-function (lambda (x y z) (expt x 3))))
              (g (jit-function (lambda (x y z) (expt x 1/2))))
              (h (jit-function (lambda (x y z) (expt x -1/2)))))
          (assert-all
            (assert-near (f 3 0 0) 27)
            (assert-near (g 4 0 0) 2)
            (assert-near (h 4 0 0) 1/2))))
))

(suite "bounds.scm (bounds metadata)"
    (tests
    (test "get-bounds" env
        (let ((f (set-bounds (lambda (x y z) x) '(-1 -2 -3) '(4 5 6)))
              (g (lambda (x y z) x)))
        (assert-all
            (assert-equal (get-bounds f) '((-1 -2 -3) (4 5 6)))
            (assert-equal (get-bounds g) #f))))
    (test "Default Z bounds" env
        (let ((f (set-bounds (lambda (x y z) x) '(-1 -2) '(4 5))))
        (assert-all
            (assert-equal (get-bounds f) '((-1 -2 -inf.0) (4 5 +inf.0))))))
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


(suite "csg.scm (CSG and bounds merging)"
    (tests
    (test "union evaluation" env
        (let* ((f (lambda (x y z) x))
               (g (lambda (x y z) y))
               (u (union f g)))
        (assert-all
            (assert-equal (u 1 0 0) 0)
            (assert-equal (u 2 3 0) 2))))
    (test "union (bounds merging)" env
        (let* ((f (set-bounds (lambda (x y z) x) '(0 -2 0) '(1 0 3)))
               (g (set-bounds (lambda (x y z) y) '(-1 0 -3) '(0 2 0)))
               (u (union f g)))
            (assert-equal (get-bounds u) '((-1 -2 -3) (1 2 3)))))
    (test "intersection (evaluation)" env
        (let* ((f (lambda (x y z) x))
               (g (lambda (x y z) y))
               (u (intersection f g)))
        (assert-all
            (assert-equal (u 1 0 0) 1)
            (assert-equal (u 2 3 0) 3))))
    (test "intersection (bounds merging)" env
        (let* ((f (set-bounds (lambda (x y z) x) '(-5 -2 0) '(1 8 3)))
               (g (set-bounds (lambda (x y z) y) '(-1 -5 -3) '(3 2 0)))
               (u (intersection f g)))
            (assert-equal (get-bounds u) '((-1 -2 0) (1 2 0)))))
    (test "intersection (disjoint bounds)" env
        (let* ((f (set-bounds (lambda (x y z) x) '(-1 -1 -1) '(0 0 0)))
               (g (set-bounds (lambda (x y z) y) '(1 2 3) '(2 3 4)))
               (u (intersection f g)))
            (assert-equal (get-bounds u) '((0 0 0) (0 0 0)))))
))
