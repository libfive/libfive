#|
Guile bindings to the Ao CAD kernel
Copyright (C) 2017  Matt Keeter

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
(use-modules (ao kernel) (ao shapes) (ao csg) (ao vec) (ao transforms))

(define glyphs (make-hash-table))
(define (make-glyph! char width shape)
  (hash-set! glyphs char (cons shape width)))
(define (glyph-shape char) (car (hash-ref glyphs char)))
(define (glyph-width char) (cdr (hash-ref glyphs char)))

(define (text txt pt)
  "text txt #[x y]
  Draws text at the given position"
  (define (line txt pt)
    (format #t "Got point ~A ~A\n" txt pt)
    (apply union
      (let recurse ((chars (string->list txt)) (pt pt))
        (if (null? chars)
          '()
          (let ((r (hash-ref glyphs (car chars))))
            (if r
              (cons (move (car r) pt)
                    (recurse (cdr chars) (+ pt #[0.1 0] #[(cdr r) 0])))
              (recurse (cdr chars) pt)))))))
  (define lines (string-split (string-upcase txt) #\newline))
  (format #t "lines: ~A\n" lines)
  (apply union
    (let recurse ((lines lines) (pt pt))
      (if (null? lines)
        '()
        (cons (line (car lines) pt) (recurse (cdr lines) (- pt #[0 1.1])))))))
(export text)

;; Glyphs are stored as (shape . width) pairs
(make-glyph! #\A 0.8
    (union (triangle #[0 0] #[0.35 1] #[0.1 0])
           (triangle #[0.1 0] #[0.35 1] #[0.45 1])
           (triangle #[0.35 1] #[0.45 1] #[0.8 0])
           (triangle #[0.7 0] #[0.35 1] #[0.8 0])
           (rectangle #[0.2 0.3] #[0.6 0.4])))

(make-glyph! #\B 0.575
    (union (intersection (union (ring #[0.3 0.725] 0.275 0.175)
                                (ring #[0.3 0.275] 0.275 0.175))
                         (rectangle #[0.3 0] #[1 1]))
                 ;; Bars connecting to the semicircles
                 (rectangle #[0.1 0] #[0.3 0.1])
                 (rectangle #[0.1 0.45] #[0.3 0.55])
                 (rectangle #[0.1 0.9] #[0.3 1])

                 ;; Main bar of the letter
                 (rectangle #[0 0] #[0.1 1])))

(make-glyph! #\C 0.57
    (union (difference (union (ring #[0.3 0.7] 0.275 0.175)
                              (ring #[0.3 0.3] 0.275 0.175))
                       (rectangle #[0 0.3] #[0.6 0.7])
                       (triangle #[0.3 0.5] #[1 1.5] #[1 -0.5]))
           (rectangle #[0.025 0.3] #[0.125 0.7])))

(make-glyph! #\D 0.6
    (union (rectangle #[0 0] #[0.1 1])
           (intersection (rectangle #[0 0] #[1 1])
                         (ring #[0.1 0.5] 0.5 0.4))))

(make-glyph! #\E 0.6
  (union (rectangle #[0 0] #[0.1 1])
         (rectangle #[0 0] #[0.6 0.1])
         (rectangle #[0 0.9] #[0.6 1])
         (rectangle #[0 0.45] #[0.6 0.55])))

(make-glyph! #\F 0.6
  (union (rectangle #[0 0] #[0.1 1])
         (rectangle #[0 0.9] #[0.6 1])
         (rectangle #[0 0.45] #[0.6 0.55])))

(make-glyph! #\G 0.6
  (union (rectangle #[0 0.3] #[0.1 0.7])
         (rectangle #[0.5 0.3] #[0.6 0.4])
         (rectangle #[0.3 0.4] #[0.6 0.5])
         (difference (union (ring #[0.3 0.7] 0.3 0.2)
                            (ring #[0.3 0.3] 0.3 0.2))
                     (rectangle #[0 0.3] #[0.6 0.7]))))

(make-glyph! #\H 0.6
  (union (rectangle #[0 0] #[0.1 1])
         (rectangle #[0.5 0] #[0.6 1])
         (rectangle #[0 0.45] #[0.6 0.55])))

(make-glyph! #\I 0.5
  (union (rectangle #[0.2 0] #[0.3 1])
         (rectangle #[0 0] #[0.5 0.1])
         (rectangle #[0 0.9] #[0.5 1])))

(make-glyph! #\J 0.55
  (union (rectangle #[0.4 0.275] #[0.5 1])
         (intersection (ring #[0.275 0.275] 0.225 0.125)
                       (rectangle #[0 0] #[0.55 0.275]))))

(make-glyph! #\K 0.6
  (difference (rectangle #[0 0] #[0.6 1])
              (triangle #[0.1 1] #[0.5 1] #[0.1 0.6])
              (triangle #[0.5 0] #[0.1 0] #[0.1 0.4])
              (triangle #[0.6 0.95] #[0.6 0.05] #[0.18 0.5])))

(make-glyph! #\L 0.6
  (union (rectangle #[0 0] #[0.1 1])
         (rectangle #[0 0] #[0.6 0.1])))

(make-glyph! #\M 0.8
  (union (rectangle #[0 0] #[0.1 1])
         (rectangle #[0.7 0] #[0.8 1])
         (triangle #[0 1] #[0.1 1] #[0.45 0])
         (triangle #[0.45 0] #[0.35 0] #[0 1])
         (triangle #[0.7 1] #[0.8 1] #[0.35 0])
         (triangle #[0.35 0] #[0.8 1] #[0.45 0])))

(make-glyph! #\N 0.6
  (union (rectangle #[0 0] #[0.1 1])
         (rectangle #[0.5 0] #[0.6 1])
         (triangle #[0 1] #[0.1 1] #[0.6 0])
         (triangle #[0.6 0] #[0.5 0] #[0 1])))

(make-glyph! #\O 0.6
  (union (rectangle #[0 0.3] #[0.1 0.7])
         (rectangle #[0.5 0.3] #[0.6 0.7])
         (difference (union (ring #[0.3 0.7] 0.3 0.2)
                            (ring #[0.3 0.3] 0.3 0.2))
                     (rectangle #[0 0.3] #[0.6 0.7]))))

(make-glyph! #\P 0.575
  (union (difference (ring #[0.3 0.725] 0.275 0.175)
                     (rectangle #[0 0] #[0.3 1]))
         (rectangle #[0 0] #[0.1 1])
         (rectangle #[0.1 0.45] #[0.3 0.55])
         (rectangle #[0.1 0.9] #[0.3 1])))

(make-glyph! #\Q 0.6
  (union (rectangle #[0 0.3] #[0.1 0.7])
         (rectangle #[0.5 0.3] #[0.6 0.7])
         (difference (union (ring #[0.3 0.7] 0.3 0.2)
                            (ring #[0.3 0.3] 0.3 0.2))
                     (rectangle #[0 0.3] #[0.6 0.7]))
         (triangle #[0.5 0.1] #[0.6 0.1] #[0.6 0])
         (triangle #[0.5 0.1] #[0.5 0.3] #[0.6 0.1])))

(make-glyph! #\R 0.575
  (union (glyph-shape #\P)
         (triangle #[0.3 0.5] #[0.4 0.5] #[0.575 0])
         (triangle #[0.475 0] #[0.3 0.5] #[0.575 0])))

(make-glyph! #\S 0.55
  (let ((half (difference (ring #[0.275 0.725] 0.275 0.175)
                          (rectangle #[0.275 0.45] #[0.55 0.725]))))
    (union half (reflect-x (reflect-y half 0.5) 0.275))))

(make-glyph! #\T 0.6
  (union (rectangle #[0 0.9] #[0.6 1])
         (rectangle #[0.25 0] #[0.35 1])))

(make-glyph! #\U 0.6
  (union (rectangle #[0 0.3] #[0.1 1])
         (rectangle #[0.5 0.3] #[0.6 1])
         (difference (ring #[0.3 0.3] 0.3 0.2)
                     (rectangle #[0 0.3] #[0.6 0.7]))))

(make-glyph! #\V 0.6
  (let ((half (union (triangle #[0 1] #[0.1 1] #[0.35 0])
                     (triangle #[0.35 0] #[0.25 0] #[0 1]))))
    (union half (reflect-x half 0.3))))

(make-glyph! #\W (- (* 2 (glyph-width #\V)) 0.1)
  (union (glyph-shape #\V)
         (move (glyph-shape #\V) #[(- (glyph-width #\V) 0.1) 0])))

(make-glyph! #\X 0.8
  (let ((half (union (triangle #[0 1] #[0.125 1] #[0.8 0])
                     (triangle #[0.8 0] #[0.675 0] #[0 1]))))
    (union half (reflect-x half 0.4))))

(make-glyph! #\Y 0.8
  (union (difference (triangle #[0 1] #[0.4 0.5] #[0.8 1])
                     (triangle #[0.1 1] #[0.4 0.65] #[0.7 1]))
         (rectangle #[0.35 0] #[0.45 0.6])))

(make-glyph! #\Z 0.6
  (difference (rectangle #[0 0] #[0.6 1])
              (triangle #[0 0.1] #[0 0.9] #[0.45 0.9])
              (triangle #[0.6 0.1] #[0.15 0.1] #[0.6 0.9])))

(make-glyph! #\space 0.55 (lambda-shape (x y z) 1))

(make-glyph! #\, 0.175
  (union (circle #[0.1 0.075] 0.075)
    (intersection (circle #[0.075 0.075] 0.075)
                  (rectangle #[0 0.15] #[-0.15 0.075])
                  (inverse (triangle #[0.075 0.075] #[0 -0.15] #[-0.5 0.075])))
))

(make-glyph! #\. 0.175 (circle #[0.075 0.075] 0.075))

(make-glyph! #\' 0.1 (rectangle #[0 0.1] #[0.55 0.8]))

(make-glyph! #\" 0.3
  (union (rectangle #[0 0.1] #[0.55 0.8])
         (rectangle #[0.2 0.3] #[0.55 0.8])))

(make-glyph! #\: 0.15
  (union (circle #[0.075 0.15] 0.075)
         (circle #[0.075 0.45] 0.075)))

(make-glyph! #\; 0.15
  (union
    (intersection (scale-y (circle #[0.075 0.15] 0.074) 3 0.015)
                  (rectangle #[0 0.15] #[-0.075 0.15])
                  (inverse (triangle #[0.075 0.15] #[0 -0.075] #[-0.5 0.15])))
    (circle #[0.075 0.45] 0.075)
    (circle #[0.1 0.15] 0.075)))

(make-glyph! #\! 0.1
  (union (rectangle #[0.025 0.125] #[0.3 1])
         (circle #[0.075 0.075] 0.075)))

(make-glyph! #\- 0.45 (rectangle #[0.05 0.4] #[0.35 0.45]))

(make-glyph! #\) 0.3
  (scale-x (intersection (circle #[0 0.4] 0.6)
                         (inverse (scale-x (circle #[0 0.4] 0.5) 0.7))
                         (rectangle #[0 0.6] #[-0.2 1])) 0.5))

(make-glyph! #\( 0.3
  (scale-x (intersection (circle #[0.6 0.4] 0.6)
                         (inverse (scale-x (circle #[0.6 0.4] 0.5) 0.7 0.6))
                         (rectangle #[0 0.6] #[-0.2 1])) 0.5))

(make-glyph! #\1 0.3
  (difference (rectangle #[0 0] #[0.3 1])
              (rectangle #[0 0] #[0.2 1])
              (circle #[0 1] 0.2)))

(make-glyph! #\2 0.55
  (union (difference (ring #[0.275 0.725] 0.275 0.175)
                     (rectangle #[0 0] #[0.55 0.725]))
         (rectangle #[0 0] #[0.55 0.1])
         (triangle #[0 0.1] #[0.45 0.775] #[0.55 0.725])
         (triangle #[0 0.1] #[0.55 0.725] #[0.125 0.1])))

(make-glyph! #\3 0.55
  (difference
    (union (ring #[0.3 0.725] 0.275 0.175)
           (ring #[0.3 0.225] 0.275 0.175))
    (rectangle #[0 0.275] #[0.275 0.725])))

(make-glyph! #\4 0.5
  (intersection (union (triangle #[-0.10 0.45] #[0.4 1] #[0.4 0.45])
                       (rectangle #[0.4 0] #[0.5 1]))
              (inverse (triangle #[0.4 0.85] #[0.4 0.55] #[0.1 0.55]))
              (rectangle #[0 0] #[0.5 1])))

(make-glyph! #\5 0.65
  (union (difference (ring #[0.325 0.325] 0.325 0.225)
                     (rectangle #[0 0.325] #[0.325 0.65]))
         (rectangle #[0 0.55] #[0.325 0.65])
         (rectangle #[0 0.55] #[0.1 1])
         (rectangle #[0.1 0.65] #[0.9 1])))

(make-glyph! #\6 0.55
  (let ((hook
    (intersection
      (circle #[0.275 0.725] 0.275)
      (inverse (scale-y (circle #[0.275 0.725] 0.175) 1.2 0.725))
      (rectangle #[0 0.725] #[0.55 1])
      (inverse (triangle #[0.275 0.925] #[0.55 0.9] #[0.55 0.725])))))
  (union (ring #[0.275 0.275] 0.275 0.175)
         (rectangle #[0 0.275] #[0.1 0.45])
         (difference (scale-x (scale-y hook 2 1) 1.1)
                     (rectangle #[0.275 0.65] #[0 0.7])))))

(make-glyph! #\7 0.6
  (union (rectangle #[0 0.9] #[0.6 1])
         (triangle #[0 0] #[0.475 0.9] #[0.6 0.9])
         (triangle #[0 0] #[0.6 0.9] #[0.125 0])))

(make-glyph! #\8 0.55
  (union (ring #[0.3 0.725] 0.275 0.175)
         (ring #[0.3 0.225] 0.275 0.175)))

(make-glyph! #\9 (glyph-width #\6)
  (reflect-x (reflect-y (glyph-shape #\6) 0.5)
             (/ (glyph-width #\6) 2)))

(make-glyph! #\0 0.7
  (scale-x (difference (circle #[0.5 0.5] 0.5)
                       (scale-x (circle #[0.5 0.5] 0.4) (sqrt 0.7) 0.5))
           0.7))

(make-glyph! #\+ 0.55
  (union (rectangle #[0 0.45] #[0.5 0.55])
         (rectangle #[0.2 0.25] #[0.3 0.75])))

(make-glyph! #\/ 0.55
  (union (triangle #[0 0] #[0.425 1] #[0.55 1])
         (triangle #[0 0] #[0.55 1] #[0.125 0])))

(make-glyph! #\? 0.55
  (intersection (difference (ring #[0.275 0.725] 0.275 0.175)
                            (rectangle #[0 0.45] #[0.275 0.725]))
                (rectangle #[0.225 0.3] #[0.325 0.55])
                (circle #[0.275 0.075] 0.075)))
