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

(use-modules (ao bind))
(suite "Low-level bindings"
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

))

