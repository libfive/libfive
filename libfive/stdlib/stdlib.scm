#|
libfive standard library
Copyright (C) 2019  Matt Keeter

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
(use-modules (ice-9 textual-ports))
(use-modules ((rnrs io ports) #:select (eof-object?)))

;; This is an EXTREME PROTOTYPE of using a single file to generate a standard
;; library (as a C++ source file, compiled into libfive-stdlib.dylib), plus
;; enough meta-data to automatically build bindings from higher-level
;; languages (using their FFI functionality).
;;
;; Right now, it doesn't do anything besides defining an Extremely Cursed DSL
;; to mix Scheme meta-data and C++ source.

(define lib (make-hash-table))
(define (insert-shape fun doc src)
  (if (hash-ref lib (car fun))
      (error (format #f "Item '~A' already exists" (car fun))))
  (hash-set! lib (car fun) (list fun doc src)))

(read-hash-extend #\C
  (lambda (chr port)
    (define fun (read port))
    (define doc (read port))
    (get-line port) ; discard
    (let f ((out ""))
      (let ((line (get-line port)))
        (cond
          ((or (eof-object? line) (= (string-length line) 0))
            `(insert-shape ',fun ,doc ,out))
          ((eq? (string-ref line 0) #\space)
            (f (string-append out "\n" line)))
          (else
            (unget-string port line)
            `(insert-shape ',fun ,doc ,out)))))))

#C (circle (r float) (center vec2 [0 0]))
  "A 2D circle with the given radius and center"
  auto c = Tree::sqrt(Tree::pow(Tree::X(), 2) + Tree::pow(Tree::Y(), 2)) - r;
  return libfive_move(c, center);

#C (ring (ro float) (ri float) (center vec2 [0 0]))
  "A 2D circle with the given outer/inner radii and optional center"
  auto outer = libfive_circle(ro, center);
  auto inner = libfive_circle(ri, center);
  return libfive_difference(outer, inner);

(hash-map->list (lambda (k v) (format #t "~A\n\n" v)) lib)
